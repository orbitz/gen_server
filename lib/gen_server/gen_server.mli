open Async.Std

type 'a t

type 'a _t = 'a t

module Response : sig
  type 'a t =
    | Stop of 'a
    | Ok   of 'a
end

(*
 * Defines the callbacks a server must implement
 *)
module Server : sig
  type 'a ret         = 'a Response.t Deferred.t

  (*
   * i = initial args, s = server state, m = message type
   *)
  type ('i, 's, 'm) t = { init        : ('m _t -> 'i -> 's ret)
			; handle_call : ('m _t -> 's -> 'm -> 's ret)
			; terminate   : ('s -> unit Deferred.t)
			}
end

(*
 * Functor implementation!
 *)
module type GEN_SERVER = sig
  type state
  type msg
  type init_arg

  val init        : msg _t -> init_arg -> state Server.ret
  val handle_call : msg _t -> state -> msg -> state Server.ret
  val terminate   : state -> unit Deferred.t
end

module Make : functor (Gs : GEN_SERVER) -> sig
  type t

  val start  : Gs.init_arg -> (t, unit) Deferred.Result.t
  val stop   : t -> unit Deferred.t

  val send   : t -> Gs.msg -> unit Deferred.t
end

(*
 * Polymorphic API
 *)
val start  : 'a -> ('a, 'b, 'c) Server.t -> ('c t, unit) Deferred.Result.t
val stop   : 'a t -> unit Deferred.t

val send   : 'a t -> 'a -> unit Deferred.t

val return : 'a -> 'a Response.t Deferred.t
val fail   : 'a -> 'a Response.t Deferred.t
