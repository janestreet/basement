type 'a t = { mutable contents : 'a }

external make : 'a. 'a -> ('a t[@local_opt]) = "%makemutable"
external make_contended : 'a. 'a -> ('a t[@local_opt]) = "caml_atomic_make_contended"
external get : 'a. 'a t -> 'a = "%field0"
external set : 'a. 'a t -> 'a -> unit = "%setfield0"

module Shared = struct
  external get : 'a. 'a t -> 'a = "%atomic_load"
  external set : 'a. 'a t -> 'a -> unit = "caml_atomic_set_stub"
  external exchange : 'a. 'a t -> 'a -> 'a = "%atomic_exchange"
  external compare_and_set : 'a. 'a t -> 'a -> 'a -> bool = "%atomic_cas"

  external compare_exchange
    : 'a.
    'a t -> 'a -> 'a -> 'a
    = "caml_atomic_compare_exchange_stub"

  external fetch_and_add : int t -> int -> int = "%atomic_fetch_add"
  external add : int t -> int -> unit = "caml_atomic_add_stub"
  external sub : int t -> int -> unit = "caml_atomic_sub_stub"
  external logand : int t -> int -> unit = "caml_atomic_land_stub"
  external logor : int t -> int -> unit = "caml_atomic_lor_stub"
  external logxor : int t -> int -> unit = "caml_atomic_lxor_stub"

  let incr r = add r 1
  let decr r = sub r 1
end

module Loc = struct
  type record
  type repr = record * int
  type !'a t

  external unsafe_of_atomic_loc
    : 'a.
    ('a Stdlib.Atomic.Loc.t[@local_opt]) -> ('a t[@local_opt])
    = "%identity"

  external to_repr : 'a. ('a t[@local_opt]) -> (repr[@local_opt]) = "%identity"
  external field : 'a. record -> int -> 'a = "%obj_field"

  let get t =
    let r, n = to_repr t in
    field (Sys.opaque_identity r) n
  ;;

  external set_field : 'a. record -> int -> 'a -> unit = "%obj_set_field"

  let set t v =
    let r, n = to_repr t in
    set_field (Sys.opaque_identity r) n v
  ;;

  module Shared = struct
    external unsafe_of_atomic_loc
      : 'a.
      ('a Stdlib.Atomic.Loc.t[@local_opt]) -> ('a t[@local_opt])
      = "%identity"

    external get : 'a. 'a t -> 'a = "%atomic_load_loc"
    external set : 'a. 'a t -> 'a -> unit = "caml_atomic_set_stub_loc"
    external exchange : 'a. 'a t -> 'a -> 'a = "%atomic_exchange_loc"
    external compare_and_set : 'a. 'a t -> 'a -> 'a -> bool = "%atomic_cas_loc"

    external compare_exchange
      : 'a.
      'a t -> 'a -> 'a -> 'a
      = "caml_atomic_compare_exchange_stub_loc"

    external fetch_and_add : int t -> int -> int = "%atomic_fetch_add_loc"
    external add : int t -> int -> unit = "caml_atomic_add_stub_loc"
    external sub : int t -> int -> unit = "caml_atomic_sub_stub_loc"
    external logand : int t -> int -> unit = "caml_atomic_land_stub_loc"
    external logor : int t -> int -> unit = "caml_atomic_lor_stub_loc"
    external logxor : int t -> int -> unit = "caml_atomic_lxor_stub_loc"

    let incr r = add r 1
    let decr r = sub r 1
  end
end
