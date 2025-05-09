type 'a t = 'a Stdlib_shim.Modes.Portable.t Atomic.t Stdlib_shim.Modes.Contended.t

external make : 'a -> ('a t[@local_opt]) = "%makemutable"
external make_contended : 'a -> ('a t[@local_opt]) = "caml_atomic_make_contended"
external get : 'a t -> 'a = "%atomic_load"
external set : 'a t -> 'a -> unit = "caml_atomic_set_stub"
external exchange : 'a t -> 'a -> 'a = "%atomic_exchange"
external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas"
external compare_exchange : 'a t -> 'a -> 'a -> 'a = "caml_atomic_compare_exchange_stub"
external fetch_and_add : int t -> int -> int = "%atomic_fetch_add"
external add : int t -> int -> unit = "caml_atomic_add_stub"
external sub : int t -> int -> unit = "caml_atomic_sub_stub"
external logand : int t -> int -> unit = "caml_atomic_land_stub"
external logor : int t -> int -> unit = "caml_atomic_lor_stub"
external logxor : int t -> int -> unit = "caml_atomic_lxor_stub"

let incr r = add r 1
let decr r = sub r 1

module Expert = struct
  external fenceless_get : 'a t -> 'a = "%field0"
  external fenceless_set : 'a t -> 'a -> unit = "%setfield0"
end
