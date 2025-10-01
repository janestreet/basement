(** See {!Portable_kernel.Subatomic} for documentation. *)

type !'a t

external make : 'a. 'a -> ('a t[@local_opt]) = "%makemutable"
external make_contended : 'a. 'a -> ('a t[@local_opt]) = "caml_atomic_make_contended"
val get : 'a. 'a t -> 'a
val set : 'a. 'a t -> 'a -> unit

module Shared : sig
  val get : 'a. 'a t -> 'a
  val set : 'a. 'a t -> 'a -> unit
  val exchange : 'a. 'a t -> 'a -> 'a
  val compare_and_set : 'a. 'a t -> 'a -> 'a -> bool
  val compare_exchange : 'a. 'a t -> 'a -> 'a -> 'a
  val fetch_and_add : int t -> int -> int
  val add : int t -> int -> unit
  val sub : int t -> int -> unit
  val logand : int t -> int -> unit
  val logor : int t -> int -> unit
  val logxor : int t -> int -> unit
  val incr : int t -> unit
  val decr : int t -> unit
end
