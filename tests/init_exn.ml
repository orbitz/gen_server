open Core.Std
open Async.Std

module Resp = Gen_server.Response

module Msg = struct
  type t = unit
end

let init _self init =
  failwith "blaaaaaaaaargh"

let handle_call _self state () =
  Deferred.return (Resp.Ok ())

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
    | Ok _ -> begin
      Deferred.return (shutdown 1)
    end
    | Error (`Error _) | Error (`Exn _) ->
      Deferred.return (shutdown 0)

let () =
  ignore (main ());
  never_returns (Scheduler.go ())
