open Core.Std
open Async.Std

module Resp = Gen_server.Response

module Msg = struct
  type t =
    | Inc of int
    | Get of int Ivar.t
end

let init _self init =
  Deferred.return (Ok init)

let handle_call _self state = function
  | Msg.Inc i ->
    Deferred.return (Resp.Ok (state + i))
  | Msg.Get i -> begin
    Ivar.fill i state;
    Deferred.return (Resp.Ok state)
  end

let terminate _reason state =
  Deferred.return ()

let inc gs i =
  Gen_server.send gs (Msg.Inc i)

let get gs =
  let ivar = Ivar.create () in
  Gen_server.send gs (Msg.Get ivar) >>= function
    | Ok _ -> begin
      Ivar.read ivar >>= fun v ->
      Deferred.return (Ok v)
    end
    | Error err ->
      Deferred.return (Error err)

let start init_val =
  let server =
    let module S = Gen_server.Server in
    { S.init        = init
    ;   handle_call = handle_call
    ;   terminate   = terminate
    }
  in
  Gen_server.start init_val server

let do_stuff () =
  let open Deferred.Result in
  start 0            >>= fun gs ->
  inc gs 1           >>= fun _ ->
  get gs             >>= fun v ->
  Gen_server.stop gs >>= fun _ ->
  Deferred.return (Ok v)

let main () =
  do_stuff () >>= function
    | Ok v -> begin
      assert (v = 1);
      Deferred.return (shutdown 0)
    end
    | Error (`Error _) | Error (`Exn _) | Error `Closed ->
      Deferred.return (shutdown 1)

let () =
  ignore (main ());
  never_returns (Scheduler.go ())
