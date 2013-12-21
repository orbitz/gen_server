open Async.Std

type 'a t

type 'ie init_ret = [ `Error of 'ie | `Exn of exn ]
type send_ret     = [ `Closed ]

module Response : sig
  type ('s, 'e) t =
    | Stop  of 's
    | Ok    of 's
    | Error of 'e
end

(*
 * Defines the callbacks a server must implement
 *)
module Server : sig
  type ('s, 'e) ret              = ('s, 'e) Response.t Deferred.t

  (*
   * i = initial args, s = server state, m = message type
   * ie = initialization error, he = handle call errors
   *)
  type 'he error                 = Normal | Exn of exn | Error of 'he
  type ('m, 'i, 's, 'ie) init    = 'm t -> 'i -> ('s, 'ie) Deferred.Result.t
  type ('m, 's, 'he) handle_call = 'm t -> 's -> 'm -> ('s, 'he) ret
  type ('he, 's) terminate       = 'he error -> 's -> unit Deferred.t
  type ('i, 's, 'm, 'ie, 'he) t  = { init        : ('m, 'i, 's, 'ie) init
				   ; handle_call : ('m, 's, 'he) handle_call
				   ; terminate   : ('he, 's) terminate
				   }
end

(*
 * Functor implementation!
 *)
module type GEN_SERVER = sig
  type state
  type msg
  type init_arg
  type init_err
  type err

  val init        : (msg, init_arg, state, init_err) Server.init
  val handle_call : (msg, state, err) Server.handle_call
  val terminate   : (err, state) Server.terminate
end

module Make : functor (Gs : GEN_SERVER) -> sig
  type t

  val start  : Gs.init_arg -> (t, [> Gs.init_err init_ret ]) Deferred.Result.t
  val stop   : t -> (unit, [> `Closed ]) Deferred.Result.t

  val send   : t -> Gs.msg -> (Gs.msg, [> send_ret ]) Deferred.Result.t
end

(*
 * Polymorphic API
 *)
val start  : 'i -> ('i, 's, 'm, 'ie, 'he) Server.t -> ('m t, [> 'ie init_ret ]) Deferred.Result.t
val stop   : 'm t ->  (unit, [> `Closed ]) Deferred.Result.t

val send   : 'm t -> 'm -> ('m, [> send_ret ]) Deferred.Result.t
