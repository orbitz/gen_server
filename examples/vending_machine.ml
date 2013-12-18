open Core.Std
open Async.Std

module Msg = struct
  type pick_ret =
    | Ok
    | Not_enough_money
    | Out_of_stock
    | Bad_id

  type t =
    | Pick      of (string * pick_ret Ivar.t)
    | Add_money of int
    | Cancel
end

module Food = struct
  type id       = string
  type cost     = int
  type quantity = int

  type item = { id       : id
	      ; cost     : cost
	      ; quantity : quantity
	      }
  type t = item list

  let pick id money t =
    match List.find ~f:(fun item -> item.id = id) t with
      | Some item when item.quantity > 0 && money >= item.cost ->
	let item = { item with quantity = item.quantity - 1 } in
	let t    = List.filter ~f:(fun item -> item.id <> id) t in
	let t    = item::t in
	Ok t
      | Some item when item.quantity <= 0 ->
	Error `Out_of_stock
      | Some _ ->
	Error `Not_enough_money
      | None ->
	Error `Bad_id
end

module Vending_machine : sig
  val start     : Food.t -> (Msg.t Gen_server.t, [> unit Gen_server.init_ret ]) Deferred.Result.t
  val stop      : Msg.t Gen_server.t -> (unit, [> `Closed ]) Deferred.Result.t
  val pick      : Msg.t Gen_server.t -> string -> (Msg.pick_ret, [> `Closed ]) Deferred.Result.t
  val add_money : Msg.t Gen_server.t -> int -> (Msg.t, [> `Closed ]) Deferred.Result.t
  val cancel    : Msg.t Gen_server.t -> (Msg.t, [> `Closed ]) Deferred.Result.t
end = struct
  type s = { current_money : int
	   ; food          : Food.t
	   }

  (* Callbacks *)
  let init _self food =
    print_endline "Starting Vending Machine";
    let state = { current_money = 0; food } in
    Deferred.return (Ok state)

  let handle_call _self state = function
    | Msg.Pick (id, reply) -> begin
      match Food.pick id state.current_money state.food with
	| Ok food -> begin
	  Ivar.fill reply Msg.Ok;
	  Deferred.return (`Ok { current_money = 0; food })
	end
	| Error `Out_of_stock -> begin
	  Ivar.fill reply Msg.Out_of_stock;
	  Deferred.return (`Ok state)
	end
	| Error `Bad_id -> begin
	  Ivar.fill reply Msg.Bad_id;
	  Deferred.return (`Ok state)
	end
	| Error `Not_enough_money -> begin
	  Ivar.fill reply Msg.Not_enough_money;
	  Deferred.return (`Ok state)
	end
    end
    | Msg.Cancel ->
      Deferred.return (`Ok { state with current_money = 0 })
    | Msg.Add_money amount ->
      Deferred.return (`Ok { state with current_money = state.current_money + amount })

  let terminate _reason state =
    print_endline "Shutting down Vending Machine";
    Deferred.unit

  (* Package the callbacks *)
  let create =
    { Gen_server.Server.init; handle_call; terminate }

  (* Actual API level *)
  let start food =
    Gen_server.start food create

  let stop s =
    Gen_server.stop s

  let pick s id =
    let open Deferred.Result in
    let ret = Ivar.create () in
    Gen_server.send s (Msg.Pick (id, ret)) >>= fun _ ->
    Deferred.(Ivar.read ret >>= Result.return)

  let cancel s =
    Gen_server.send s Msg.Cancel

  let add_money s amount =
    Gen_server.send s (Msg.Add_money amount)
end

let food = [ { Food.id       = "cheetos"
	     ;      cost     = 75
	     ;      quantity = 10
	     }
	   ; { Food.id       = "doritos"
	     ;      cost     = 75
	     ;      quantity = 10
	     }
	   ; { Food.id       = "granola"
	     ;      cost     = 50
	     ;      quantity = 5
	     }
	   ]

let try_pick gs id =
  let print = function
      | Msg.Ok               -> printf "Yummy %s!\n%!" id
      | Msg.Not_enough_money -> print_endline "You need to add more money"
      | Msg.Out_of_stock     -> printf "There are not enough %s in stock\n%!" id
      | Msg.Bad_id           -> printf "I have no idea what %s is\n%!" id
  in
  let open Deferred.Result in
  Vending_machine.pick gs id >>= fun msg ->
  print msg;
  Deferred.return (Ok ())

let vending_machine () =
  let open Deferred.Result in
  Vending_machine.start food      >>= fun gs ->
  try_pick gs "something"         >>= fun _ ->
  try_pick gs "cheetos"           >>= fun _ ->
  Vending_machine.add_money gs 75 >>= fun _ ->
  try_pick gs "cheetos"           >>= fun _ ->
  Vending_machine.add_money gs 25 >>= fun _ ->
  Vending_machine.add_money gs 25 >>= fun _ ->
  try_pick gs "granola"           >>= fun _ ->
  Vending_machine.add_money gs 75 >>= fun _ ->
  Vending_machine.cancel gs       >>= fun _ ->
  try_pick gs "doritos"           >>= fun _ ->
  Vending_machine.stop gs         >>= fun _ ->
  Deferred.return (Ok ())

let main () =
  vending_machine () >>= function
    | Ok () ->
      Deferred.return (shutdown 0)
    | Error _ -> begin
      printf "Failed for unknown reason\n";
      Deferred.return (shutdown 1)
    end

let () =
  ignore (main ());
  never_returns (Scheduler.go ())
