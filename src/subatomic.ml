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
