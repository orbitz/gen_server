open Core.Std
open Async.Std

module Msg = struct
  type t = unit
end

let init _self init =
  failwith "blaaaaaaaaargh"

let handle_call _self state () =
  Deferred.return (`Ok ())

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
  Gen_server.start () server >>= function
    | `Ok gs -> begin
      Gen_server.send gs () >>= fun () ->
      Gen_server.send gs () >>= fun () ->
      Gen_server.stop gs    >>= fun () ->
      Deferred.return (shutdown 0)
    end
    | `Error _ | `Exn _ ->
      Deferred.return (shutdown 1)

let () =
  ignore (main ());
  never_returns (Scheduler.go ())
