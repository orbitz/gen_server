open Core.Std
open Async.Std

module Msg = struct
  type t =
    | Print of string
    | Sync
end

module Simple_server : sig
  val start : unit -> (Msg.t Gen_server.t, [> unit Gen_server.init_ret ]) Deferred.Result.t
  val stop  : Msg.t Gen_server.t -> (unit, [> `Closed ]) Deferred.Result.t
  val print : Msg.t Gen_server.t -> string -> (Msg.t, [> `Closed ]) Deferred.Result.t
  val sync  : Msg.t Gen_server.t -> (Msg.t, [> `Closed ]) Deferred.Result.t
end = struct
  (* Callbacks *)
  let init _self () =
    Deferred.return (Ok ())

  let handle_call _self () = function
    | Msg.Print s -> begin
      print_endline s;
      Deferred.return (`Ok ())
    end
    | Msg.Sync ->
      Deferred.return (`Ok ())

  let terminate _reason () =
    print_endline "Shutting down";
    Deferred.unit

  (* Package the callbacks *)
  let create =
    { Gen_server.Server.init; handle_call; terminate }

  (* Actual API level *)
  let start () =
    Gen_server.start () create

  let stop s =
    Gen_server.stop s

  let print s str =
    Gen_server.send s (Msg.Print str)

  let sync s =
    Gen_server.send s Msg.Sync
end

let simple () =
  let open Deferred.Result in
  Simple_server.start ()           >>= fun gs ->
  Simple_server.print gs "foo bar" >>= fun _ ->
  Simple_server.sync gs            >>= fun _ ->
  Simple_server.stop gs            >>= fun () ->
  Deferred.return (Ok ())

let main () =
  simple () >>= function
    | Ok () ->
      Deferred.return (shutdown 0)
    | Error _ -> begin
      printf "Failed for unknown reason";
      Deferred.return (shutdown 1)
    end

let () =
  ignore (main ());
  never_returns (Scheduler.go ())
