open Modes
module Dynamic = Stdlib_shim.Dynamic

type 'a t =
  { root : 'a Modes.Portended.t Atomic.t
  ; current : 'a Dynamic.t
  }

let make a : _ t = { root = Atomic.make { portended = a }; current = Dynamic.make () }
let set_root (t : _ t) v = Atomic.set t.root { portended = v }

let get (t : _ t) =
  match Dynamic.get t.current with
  | This r -> r
  | Null -> (Atomic.get t.root).portended
;;

let with_temporarily (t : _ t) v ~f =
  (Dynamic.with_temporarily t.current v ~f:(fun () ->
     { global = { aliased_many = f () } }))
    .global
    .aliased_many
;;
