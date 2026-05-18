@@ portable

(** This module contains miscellaneous machinery for interacting with the portability and
    contention modes while support is being finished in the compiler. Functions in this
    module that have "magic" in their name are unsafe, and must be used very carefully.
    Other functions are safe interfaces implemented in terms of unsafe logic; feel free to
    use these without concern.

    Prefer using functions from this module over [Stdlib.Obj.magic*] when the reason for
    the magic is simply a temporary lack of support for a feature in the compiler. This
    will allow us to later search for uses of the corresponding function when a feature is
    updated and remove the unnecessary magic. *)

(** A safe api for making values cross portability and contention when doing so requires
    not-yet-supported with-kinds. Example idiomatic usage:

    {[
      let use_portably (_ @ portable) = ()

      let f (x : (string * bool) list @@ nonportable) =
        Cross.Portable.(cross (list (tuple2 infer infer))) x |> use_portably
      ;;
    ]} *)
module Cross : sig
  module Portable : sig
    (** A ['a t] represents a proof that ['a] crosses portability.

        Its runtime representation carries no information; safety is enforced strictly via
        phantom typing. *)
    type 'a t : immediate

    (** Given a proof that ['a] crosses portability and a nonportable ['a], cross ['a] to
        be portable. *)
    val cross : 'a t -> 'a -> 'a @ portable

    (** Safely infer that a given ['a] crosses portability. *)
    val infer : ('a : value mod portable) t

    (** A proof that ['a option] crosses portability if ['a] does. *)
    val option : 'a t -> 'a option t

    (** A proof that ['a list] crosses portability if ['a] does. *)
    val list : 'a t -> 'a list t

    (** A proof that ['a array] crosses portability if ['a] does. *)
    val array : 'a t -> 'a array t

    (** A proof that ['a iarray] crosses portability if ['a] does. *)
    val iarray : 'a t -> 'a Stdlib_iarray_labels.t t

    (** A proof that ['a * 'b] crosses portability if both ['a] and ['b] do. *)
    val tuple2 : 'a t -> 'b t -> ('a * 'b) t

    (** A proof that ['a * 'b * 'c] crosses portability if all of ['a], ['b], and ['c] do. *)
    val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

    (** A proof that [extension_constructor] crosses portability. *)
    val extension_constructor : extension_constructor t

    (** A proof that [Stdlib.Random.State.t] crosses portability. *)
    val random_state : Stdlib.Random.State.t t

    (** Unsafely assert that ['a] crosses portability. *)
    val magic : 'a t
  end

  module Contended : sig
    (** A ['a t] represents a proof that ['a] crosses contention.

        Its runtime representation carries no information; safety is enforced strictly via
        phantom typing. *)
    type 'a t : immediate

    (** Given a proof that ['a] crosses contention and a contended ['a], cross ['a] to be
        uncontended. *)
    val cross : 'a t -> 'a @ contended -> 'a

    (** Safely infer that a given ['a] crosses contention. *)
    val infer : ('a : value mod contended) t

    (** A proof that ['a option] crosses contention if ['a] does. *)
    val option : 'a t -> 'a option t

    (** A proof that ['a list] crosses contention if ['a] does. *)
    val list : 'a t -> 'a list t

    (** A proof that ['a iarray] crosses portability if ['a] does. *)
    val iarray : 'a t -> 'a Stdlib_iarray_labels.t t

    (** A proof that ['a * 'b] crosses contention if both ['a] and ['b] do. *)
    val tuple2 : 'a t -> 'b t -> ('a * 'b) t

    (** A proof that ['a * 'b * 'c] crosses contention if all of ['a], ['b], and ['c] do. *)
    val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

    (** A proof that [extension_constructor] crosses contention. *)
    val extension_constructor : extension_constructor t

    (** Unsafely assert that ['a] crosses contention. *)
    val magic : 'a t
  end
end

(** Unsafely assert that a value is portable, but that showing such requires part of
    [Base] / [Core] to be portablized. This should be used very conservatively; uses of
    this function should be able to be deleted as soon as all of [Base] and [Core] are
    portablized. *)
external magic_portable__needs_base_and_core : 'a -> 'a @ portable = "%identity"

(** Unsafely assert that a value is uncontended, but that showing such requires part of
    [Base] / [Core] to be portablized. This should be used very conservatively; uses of
    this function should be able to be deleted as soon as all of [Base] and [Core] are
    portablized. *)
external magic_uncontended__needs_base_and_core : 'a @ contended -> 'a = "%identity"

(** Unsafely assert that a value is portable, but that showing such requires the ability
    to instantiate (portable) functors inside of a portable function. *)
external magic_portable__needs_portable_functors : 'a -> 'a @ portable = "%identity"

(** Unsafely assert that a value is uncontended because it is "deeply immutable". If
    support is added for [cocontended], this value should be able to be marked as
    [cocontended]. Do not use this unless you have a reasonably strong understanding of
    what it means to be [cocontended]. *)
external magic_uncontended__promise_deeply_immutable
  : ('a : value_or_null).
  'a @ contended -> 'a
  = "%identity"

(** Like [magic_uncontended__promise_deeply_immutable], but preserves portability.

    This function is not generally necessary, since in most cases you only need
    [magic_uncontended] *after* you are done using the portability of the value. If you
    are reaching for this in order to have a cleaner interface (where you don't need to
    mark a top-level value as [contended]), consider adding a portable [get_X] thunk that
    wraps the value and calls [magic_uncontended__promise_deeply_immutable] in order to
    avoid putting the magic at use-sites (see [Base.Avltree.get_empty], for example). *)
external magic_uncontended__promise_deeply_immutable__portable
  :  'a @ contended portable
  -> 'a @ portable
  = "%identity"

(** Similar to [magic_uncontended__promise_deeply_immutable] but for first-class modules.
    We distinguish the two as the work needed in the compiler to support them are
    different. *)
external magic_uncontended__promise_deeply_immutable_module
  :  'a @ contended
  -> 'a
  = "%identity"

(** Unsafely assert that a value is uncontended, and that operations on the value can
    cause data races, but that those data races cannot cause unsoundness because of the
    OCaml memory model. No long-term guarantees are made about whether or not this
    function is sound to use; in particular, data races can in fact cause torn reads or
    writes (in violation of the OCaml memory model) on unboxed products, and may be made
    more unsound at any point in the future. The point of this function is to document all
    of the places where we are depending on the safety provided by the OCaml memory model
    to intentionally race (for example, to do double-checked locking), so that we can
    replace all of those usages with a first-class language feature (such as an attribute
    tagging a mutable field as racy, or a racy array type) at some point in the future. *)
external magic_uncontended__racy_but_ok_because_memory_model
  :  'a @ contended
  -> 'a
  = "%identity"

external magic_uncontended__racy_but_ok_because_memory_model__portable
  :  'a @ contended portable
  -> 'a @ portable
  = "%identity"
