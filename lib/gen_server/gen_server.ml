open Core.Std
open Async.Std

type 'a t = 'a Pipe.Writer.t

type 'a _t = 'a t

type 'ie init_ret = [ `Error of 'ie | `Exn of exn ]
type send_ret     = [ `Closed ]

module Response = struct
  type ('s, 'e) t =
    | Stop  of 's
    | Ok    of 's
    | Error of 'e
end

(*
 * This is the actual server loop, it handles listening on the pipe and
 * calling the appropriate callback
 *)
module Server = struct
  type ('s, 'e) ret              = ('s, 'e) Response.t Deferred.t

  type 'he error                 = Normal | Exn of exn | Error of 'he
  type ('m, 'i, 's, 'ie) init    = 'm _t -> 'i -> ('s, 'ie) Deferred.Result.t
  type ('m, 's, 'he) handle_call = 'm _t -> 's -> 'm -> ('s, 'he) ret
  type ('he, 's) terminate       = 'he error -> 's -> unit Deferred.t
  type ('i, 's, 'm, 'ie, 'he) t  = { init        : ('m, 'i, 's, 'ie) init
				   ; handle_call : ('m, 's, 'he) handle_call
				   ; terminate   : ('he, 's) terminate
				   }

  type ('i, 's, 'm, 'ie, 'he) s  = { callbacks : ('i, 's, 'm, 'ie, 'he) t
				   ; state     : 's
				   ; r         : 'm Pipe.Reader.t
				   ; w         : 'm Pipe.Writer.t
				   }

  let safe_init init w init_arg =
    Monitor.try_with
      (fun () -> init w init_arg)
    >>= function
      | Result.Ok (Result.Ok anything) ->
	Deferred.return (`Ok anything)
      | Result.Ok (Result.Error anything) ->
	Deferred.return (`Error anything)
      | Result.Error exn ->
	Deferred.return (`Exn exn)

  let safe_call handle_call w s msg =
    Monitor.try_with
      (fun () -> handle_call w s msg)
    >>= function
      | Result.Ok (Response.Ok anything) ->
	Deferred.return (`Ok anything)
      | Result.Ok (Response.Error anything) ->
	Deferred.return (`Error anything)
      | Result.Ok (Response.Stop anything) ->
	Deferred.return (`Stop anything)
      | Result.Error exn ->
	Deferred.return (`Exn exn)

  (* Termination errors are just consumed *)
  let safe_terminate terminate reason state =
    Monitor.try_with
      (fun () -> terminate reason state)
    >>= function
      | Result.Ok ()   -> Deferred.return ()
      | Result.Error _ -> Deferred.return ()


  let rec loop s =
    Pipe.read s.r >>= function
      | `Eof ->
	safe_terminate s.callbacks.terminate Normal s.state
      | `Ok msg ->
	handle_call s msg
  and handle_call s msg =
    safe_call s.callbacks.handle_call s.w s.state msg >>= function
      | `Ok state ->
	loop { s with state }
      | `Error err -> begin
	Pipe.close s.w;
	safe_terminate s.callbacks.terminate (Error err) s.state
      end
      | `Exn exn -> begin
	Pipe.close s.w;
	safe_terminate s.callbacks.terminate (Exn exn) s.state
      end
      | `Stop state -> begin
	Pipe.close s.w;
	safe_terminate s.callbacks.terminate Normal state
      end

  let start init_arg callbacks r w =
    safe_init callbacks.init w init_arg >>= function
      | `Ok state -> begin
	let s = { callbacks; state; r; w } in
	ignore (loop s);
	Deferred.return (Result.Ok w)
      end
      | `Error err ->
	Deferred.return (Result.Error (`Error err))
      | `Exn exn ->
	Deferred.return (Result.Error (`Exn exn))
end

(*
 * This is the core API for a polymorphic gen_server.
 * This is the base from which everything else can be constructed
 *)
let start init_arg callbacks =
  let (r, w) = Pipe.create () in
  Server.start init_arg callbacks r w

let stop t =
  if not (Pipe.is_closed t) then begin
    Pipe.close t;
    Deferred.return (Ok ())
  end
  else
    Deferred.return (Error `Closed)

let send t m =
  if not (Pipe.is_closed t) then begin
    Pipe.write t m >>= fun () ->
    Deferred.return (Ok m)
  end
  else
    Deferred.return (Error `Closed)

(*
 * We provide a functor for those who are in to that kind of thing
 *)
module type GEN_SERVER = sig
  type state
  type msg
  type init_arg
  type init_err
  type err

  val init        : (msg, init_arg, state, init_err) Server.init
  val handle_call : (msg, state, err) Server.handle_call
  val terminate   : (err, state) Server.terminate
end

(*
 * The functor doesn't do much other than wrap the callbacks
 * up in the proper structure for start
 *)
module Make = functor (Gs : GEN_SERVER) -> struct
  type t = Gs.msg _t

  let callbacks = { Server.init        = Gs.init
		  ;        handle_call = Gs.handle_call
		  ;        terminate   = Gs.terminate
		  }

  let start init_arg = start init_arg callbacks
  let stop           = stop
  let send           = send
end
