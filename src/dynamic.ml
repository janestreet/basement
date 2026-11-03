open Modes
module Dynamic = Stdlib_shim.Dynamic

type 'a t =
  { mutable root : 'a Modes.Portended.t [@atomic]
  ; current : 'a Dynamic.t
  }

let make a : _ t = { root = { portended = a }; current = Dynamic.make () }
let set_root (t : _ t) v = Atomic.Loc.set [%atomic.loc t.root] { portended = v }

let get (t : _ t) =
  match Dynamic.get t.current with
  | This r -> r
  | Null -> (Atomic.Loc.get [%atomic.loc t.root]).portended
;;

let with_temporarily (t : _ t) v ~f =
  (Dynamic.with_temporarily t.current v ~f:(fun () ->
     { global = { aliased_many = f () } }))
    .global
    .aliased_many
;;
