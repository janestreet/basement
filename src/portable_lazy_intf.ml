(** [Portable_lazy] is reexported, with documentation, as [Base.Portable_lazy]; see that
    module for documentation of this interface.

    The implementation of [Portable_lazy] in this library is optimized for performance,
    but requires magic to expose a safe and expressive interface. The safety of the
    interface alone is justified by a non-magic implementation in the tests of this
    library, which implements [S] below in a slightly less memory efficient way. *)

module type S = sig
  type +'a t

  val from_val : 'a. 'a -> 'a t
  val from_fun : 'a. (unit -> 'a) -> 'a t
  val from_fun_fixed : 'a. ('a t -> 'a) -> 'a t
  val force : 'a. 'a t -> 'a
  val map : 'a 'b. 'a t -> f:('a -> 'b) -> 'b t
  val bind : 'a 'b. 'a t -> f:('a -> 'b t) -> 'b t
  val compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : 'a. ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val globalize : 'a 'b. 'b -> 'a t -> 'a t
  val is_val : 'a. 'a t -> bool
  val peek : 'a. 'a t -> 'a Or_null_shim.t
  val peek_opt : 'a. 'a t -> 'a option
end
