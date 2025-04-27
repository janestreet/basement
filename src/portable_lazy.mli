@@ portable

(** This module is reexported, with documentation, as [Base.Portable_lazy]; see that
    module for documentation this interface. *)

type 'a t : value mod contended portable

val from_val : 'a @ contended portable -> 'a t
val from_fun : (unit -> 'a @ contended portable) @ once portable -> 'a t
val from_fun_fixed : ('a t -> 'a @ contended portable) @ once portable -> 'a t

exception Undefined

val force : 'a t -> 'a @ contended portable

val map
  :  'a t
  -> f:('a @ contended portable -> 'b @ contended portable) @ once portable
  -> 'b t

val bind : 'a t -> f:('a @ contended portable -> 'b t) @ once portable -> 'b t
val compare : ('a : value mod contended). ('a -> 'a -> int) -> 'a t -> 'a t -> int

val compare__local
  : ('a : value mod contended).
  ('a @ local -> 'a @ local -> int) -> 'a t @ local -> 'a t @ local -> int

val equal : ('a : value mod contended). ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val equal__local
  : ('a : value mod contended).
  ('a @ local -> 'a @ local -> bool) -> 'a t @ local -> 'a t @ local -> bool

val globalize : _ -> 'a t @ local -> 'a t
val is_val : 'a t -> bool
val peek : 'a t -> 'a option @ contended portable
