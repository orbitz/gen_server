open Core.Std
open Async.Std

module Msg = struct
  type t = unit
end

let init _self init =
  Deferred.return (Ok init)

let handle_call _self state () =
  Deferred.return (`Ok ())

let terminate _reason state =
  failwith "blaaargh"

let test () =
  let server =
    let module S = Gen_server.Server in
    { S.init        = init
    ;   handle_call = handle_call
    ;   terminate   = terminate
    }
  in
  let open Deferred.Result in
  Gen_server.start () server >>= fun gs ->
  Gen_server.send gs ()      >>= fun _ ->
  Gen_server.send gs ()      >>= fun _ ->
  Gen_server.stop gs         >>= fun _ ->
  Deferred.return (Ok ())

let main () =
  test () >>= function
    | Ok () ->
      Deferred.return (shutdown 0)
    | Error _ ->
      Deferred.return (shutdown 1)

let () =
  ignore (main ());
  never_returns (Scheduler.go ())
