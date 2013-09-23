open Core.Std
open Async.Std

type 'a t = 'a Pipe.Writer.t

type 'a _t = 'a t

module Response = struct
  type 'a t =
    | Stop of 'a
    | Ok   of 'a
end

(*
 * This is the actual server loop, it handles listening on the pipe and
 * calling the appropriate callback
 *)
module Server = struct
  type 'a ret         = 'a Response.t Deferred.t
  type ('a, 'b, 'c) t = { init        : ('c _t -> 'a -> 'b ret)
			; handle_call : ('c _t -> 'b -> 'c -> 'b ret)
			; terminate   : ('b -> unit Deferred.t)
			}

  type ('a, 'b, 'c) s = { callbacks : ('a, 'b, 'c) t
			; state     : 'b
			; r         : 'c Pipe.Reader.t
			; w         : 'c Pipe.Writer.t
			}

  let rec loop s =
    Pipe.read s.r >>= function
      | `Eof ->
	s.callbacks.terminate s.state
      | `Ok msg ->
	handle_call s msg
  and handle_call s msg =
    s.callbacks.handle_call s.w s.state msg >>= function
      | Response.Ok state ->
	loop { s with state }
      | Response.Stop state -> begin
	Pipe.close s.w;
	s.callbacks.terminate state
      end

  let start init_arg callbacks r w =
    callbacks.init w init_arg >>= function
      | Response.Ok state -> begin
	let s = { callbacks; state; r; w } in
	ignore (loop s);
	Deferred.return (Ok ())
      end
      | Response.Stop _ ->
	Deferred.return (Error ())
end

(*
 * This is the core API for a polymorphic gen_server.
 * This is the base from which everything else can be constructed
 *)
let start init_arg callbacks =
  let open Deferred.Result.Monad_infix in
  let (r, w) = Pipe.create () in
  Server.start init_arg callbacks r w >>= fun () ->
  Deferred.return (Ok w)

let stop t =
  Pipe.close t;
  Deferred.unit

let send = Pipe.write

let return state =
  Deferred.return (Response.Ok state)

let fail state =
  Deferred.return (Response.Stop state)

(*
 * We provide a functor for those who are in to that kind of thing
 *)
module type GEN_SERVER = sig
  type state
  type msg
  type init_arg

  val init        : msg _t -> init_arg -> state Server.ret
  val handle_call : msg _t -> state -> msg -> state Server.ret
  val terminate   : state -> unit Deferred.t
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
