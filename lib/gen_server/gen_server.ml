open Core.Std
open Async.Std

type 'a t = 'a Pipe.Writer.t

type 'a _t = 'a t

module Response = struct
  type 'a t =
    | Stop
    | Ok of 'a
end

module Server = struct
  type 'a ret         = 'a Response.t Deferred.t
  type ('a, 'b, 'c) t = { init        : ('c _t -> 'a -> 'b ret)
			; handle_call : ('c _t -> 'b -> 'c -> 'b ret)
			; terminate   : ('b -> unit Deferred.t)
			}

  type ('a, 'b, 'c) s = { server : ('a, 'b, 'c) t
			; state  : 'b
			; r      : 'c Pipe.Reader.t
			; w      : 'c Pipe.Writer.t
			}

  let rec loop s =
    Pipe.read s.r >>= function
      | `Eof ->
	s.server.terminate s.state
      | `Ok msg ->
	handle_call s msg
  and handle_call s msg =
    s.server.handle_call s.w s.state msg >>= function
      | Response.Ok state ->
	let s = { s with state } in
	loop s
      | Response.Stop -> begin
	Pipe.close s.w;
	loop s
      end

  let start init_arg server r w =
    server.init w init_arg >>= function
      | Response.Ok state -> begin
	let s = { server; state; r; w } in
	ignore (loop s);
	Deferred.return (Ok ())
      end
      | Response.Stop ->
	Deferred.return (Error ())
end 
let start init_arg server =
  let open Deferred.Result.Monad_infix in
  let (r, w) = Pipe.create () in
  Server.start init_arg server r w >>= fun () ->
  Deferred.return (Ok w)

let stop t =
  Pipe.close t;
  Deferred.unit

let send = Pipe.write

let return v =
  Deferred.return (Response.Ok v)
