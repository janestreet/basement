(** This module is reexported, with documentation, as [Base.Portable_lazy]; see that
    module for documentation this interface. *)

type 'a t

val from_val : 'a -> 'a t
val from_fun : (unit -> 'a) -> 'a t
val from_fun_fixed : ('a t -> 'a) -> 'a t

exception Undefined

val force : 'a t -> 'a
val map : 'a t -> f:('a -> 'b) -> 'b t
val bind : 'a t -> f:('a -> 'b t) -> 'b t
val compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int
val compare__local : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int
val equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val equal__local : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val globalize : _ -> 'a t -> 'a t
val is_val : 'a t -> bool
val peek : 'a t -> 'a option
