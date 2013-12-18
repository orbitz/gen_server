open Core.Std
open Async.Std

module Msg = struct
  type t =
    | Inc of int
    | Get of int Ivar.t
end

let init _self init =
  Deferred.return (Ok init)

let handle_call _self state = function
  | Msg.Inc i ->
    Deferred.return (`Ok (state + i))
  | Msg.Get i -> begin
    Ivar.fill i state;
    Deferred.return (`Ok state)
  end

let terminate _reason state =
  Deferred.return ()

let main () =
  let server =
    let module S = Gen_server.Server in
    { S.init        = init
    ;   handle_call = handle_call
    ;   terminate   = terminate
    }
  in
  Gen_server.start 0 server >>= function
    | `Ok gs -> begin
      Gen_server.send gs (Msg.Inc 1) >>= fun () ->
      let i = Ivar.create () in
      Gen_server.send gs (Msg.Get i) >>= fun () ->
      Gen_server.stop gs             >>= fun () ->
      Ivar.read i                    >>= fun num ->
      assert (num = 1);
      Deferred.return (shutdown 0)
    end
    | `Error _ | `Exn _ ->
      Deferred.return (shutdown 1)

let () =
  ignore (main ());
  never_returns (Scheduler.go ())
