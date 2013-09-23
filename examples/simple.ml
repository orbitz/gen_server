open Core.Std
open Async.Std

module Msg = struct
  type t =
    | Print of string
    | Sync
end

module Simple_server : sig
  val start : unit -> (Msg.t Gen_server.t, unit) Deferred.Result.t
  val stop  : Msg.t Gen_server.t -> unit Deferred.t
  val print : Msg.t Gen_server.t -> string -> unit Deferred.t
  val sync  : Msg.t Gen_server.t -> unit Deferred.t
end = struct
  (* Callbacks *)
  let init self init_arg =
    Deferred.return (Gen_server.Response.Ok ())

  let handle_call self () = function
    | Msg.Print s -> begin
      print_endline s;
      Deferred.return (Gen_server.Response.Ok ())
    end
    | Msg.Sync ->
      Deferred.return (Gen_server.Response.Ok ())

  let terminate () =
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

let main () =
  Simple_server.start () >>= function
    | Ok gs -> begin
      Simple_server.print gs "foo bar" >>= fun () ->
      Simple_server.sync gs            >>= fun () ->
      Simple_server.stop gs            >>= fun () ->
      Deferred.return (shutdown 0)
    end
    | Error () -> begin
      printf "Failed for unknown reason";
      Deferred.return (shutdown 1)
    end

let () =
  ignore (main ());
  never_returns (Scheduler.go ())
