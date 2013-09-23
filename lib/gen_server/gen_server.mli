open Async.Std

type 'a t

type 'a _t = 'a t

module Response : sig
  type 'a t =
    | Stop
    | Ok of 'a
end

module Server : sig
  type 'a ret         = 'a Response.t Deferred.t
  type ('a, 'b, 'c) t = { init        : ('c _t -> 'a -> 'b ret)
			; handle_call : ('c _t -> 'b -> 'c -> 'b ret)
			; terminate   : ('b -> unit Deferred.t)
			}
end

val start  : 'a -> ('a, 'b, 'c) Server.t -> ('c t, unit) Deferred.Result.t
val stop   : 'a t -> unit Deferred.t

val send   : 'a t -> 'a -> unit Deferred.t

val return : 'a -> 'a Response.t Deferred.t
