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
  type t

  val start     : Food.t -> (t, unit) Deferred.Result.t
  val stop      : t -> unit Deferred.t
  val pick      : t -> string -> Msg.pick_ret Deferred.t
  val add_money : t -> int -> unit Deferred.t
  val cancel    : t -> unit Deferred.t
end = struct
  module Server = Gen_server.Make(struct
    type state    = { current_money : int
		    ; food          : Food.t
		    }
    type msg      = Msg.t
    type init_arg = Food.t

    (* Callbacks *)
    let init self food =
      print_endline "Starting Vending Machine";
      let state = { current_money = 0; food } in
      Gen_server.return state

    let handle_call self state = function
      | Msg.Pick (id, reply) -> begin
	match Food.pick id state.current_money state.food with
	  | Ok food -> begin
	    Ivar.fill reply Msg.Ok;
	    Gen_server.return { current_money = 0; food }
	  end
	  | Error `Out_of_stock -> begin
	    Ivar.fill reply Msg.Out_of_stock;
	    Gen_server.return state
	  end
	  | Error `Bad_id -> begin
	    Ivar.fill reply Msg.Bad_id;
	    Gen_server.return state
	  end
	  | Error `Not_enough_money -> begin
	    Ivar.fill reply Msg.Not_enough_money;
	    Gen_server.return state
	end
      end
      | Msg.Cancel ->
	Gen_server.return { state with current_money = 0 }
      | Msg.Add_money amount ->
	Gen_server.return { state with current_money = state.current_money + amount }

    let terminate state =
      print_endline "Shutting down Vending Machine";
      Deferred.unit
  end)

  type t = Server.t

  let start = Server.start
  let stop  = Server.stop

  (* Actual API level *)
  let pick s id =
    let ret = Ivar.create () in
    Server.send s (Msg.Pick (id, ret)) >>= fun () ->
    Ivar.read ret

  let cancel s =
    Server.send s Msg.Cancel

  let add_money s amount =
    Server.send s (Msg.Add_money amount)
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
  Vending_machine.pick gs id >>| function
    | Msg.Ok               -> printf "Yummy %s!\n%!" id
    | Msg.Not_enough_money -> print_endline "You need to add more money"
    | Msg.Out_of_stock     -> printf "There are not enough %s in stock\n%!" id
    | Msg.Bad_id           -> printf "I have no idea what %s is\n%!" id


let main () =
  Vending_machine.start food >>= function
    | Ok gs -> begin
      try_pick gs "something"         >>= fun () ->
      try_pick gs "cheetos"           >>= fun () ->
      Vending_machine.add_money gs 75 >>= fun () ->
      try_pick gs "cheetos"           >>= fun () ->
      Vending_machine.add_money gs 25 >>= fun () ->
      Vending_machine.add_money gs 25 >>= fun () ->
      try_pick gs "granola"           >>= fun () ->
      Vending_machine.add_money gs 75 >>= fun () ->
      Vending_machine.cancel gs       >>= fun () ->
      try_pick gs "doritos"           >>= fun () ->
      Vending_machine.stop gs         >>= fun () ->
      Deferred.return (shutdown 0)
    end
    | Error () -> begin
      printf "Failed for unknown reason\n";
      Deferred.return (shutdown 1)
    end

let () =
  ignore (main ());
  never_returns (Scheduler.go ())
