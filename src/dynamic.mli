@@ portable

(** This module is reexported, with documentation, as [Base.Dynamic]; see that module for
    documentation on this interface. *)

type 'a t : value mod contended portable

val make : 'a @ contended portable -> 'a t
val get : 'a t -> 'a @ contended portable
val set_root : 'a t -> 'a @ contended portable -> unit

val with_temporarily
  : 'a ('b : value_or_null).
  'a t -> 'a @ contended portable -> f:(unit -> 'b) @ local once -> 'b
