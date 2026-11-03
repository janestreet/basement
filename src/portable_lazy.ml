type ('a : value_or_null) portended = { portended : 'a @@ contended portable } [@@unboxed]
type ('a : value_or_null) t = 'a portended Atomic_lazy.t portended

let from_val value = { portended = Atomic_lazy.from_val__portable { portended = value } }

(* [portended] is unboxed, so a function returning ['a] has the representation of one
   returning ['a portended], and a function taking ['a t] that of one taking
   ['a portended Atomic_lazy.t]. Retyping the caller's function through these identities
   rather than wrapping it in a closure saves 4 words per lazy. The [portended] wrapper
   carries the mode-safety argument for the retyping; compiling the closure-based
   [Portable_lazy_proof_of_soundness] version in the tests of this library against this
   module's signature checks it.

   The function types are named because upstream OCaml counts every syntactic arrow of an
   external's type when computing its arity, parenthesised or not, and [%identity] must
   have arity one. *)

type ('a : value_or_null) thunk = unit -> 'a @ contended portable
type ('a : value_or_null) portended_thunk = unit -> 'a portended
type ('a : value_or_null) fixed_fun = 'a t -> 'a @ contended portable

type ('a : value_or_null) portended_fixed_fun =
  'a portended Atomic_lazy.t @ contended portable -> 'a portended

external portended_wrap_thunk
  : ('a : value_or_null).
  'a thunk @ once portable -> 'a portended_thunk @ once portable
  @@ portable
  = "%identity"

external portended_wrap_fixed_fun
  : ('a : value_or_null).
  'a fixed_fun @ once portable -> 'a portended_fixed_fun @ once portable
  @@ portable
  = "%identity"

let from_fun (type a : value_or_null) func =
  { portended =
      Atomic_lazy.from_fun__portable
        ((portended_wrap_thunk func : unit -> a portended)
         (* Arrow types unify only when their modes are equal, so the type above does not
            unify with the one below even though [a portended] crosses portability.
            Subtyping does apply mode crossing, so the coercion is accepted. *)
          :> unit -> a portended @ portable)
  }
;;

let from_fun_fixed (type a : value_or_null) func =
  { portended =
      Atomic_lazy.from_fun_fixed__portable
        ((portended_wrap_fixed_fun func
          : a portended Atomic_lazy.t @ contended portable -> a portended)
         (* Arrow types unify only when their modes are equal, so the type above does not
            unify with the one below even though [a portended] crosses portability.
            Subtyping does apply mode crossing, so the coercion is accepted. *)
          :> a portended Atomic_lazy.t @ contended portable -> a portended @ portable)
  }
;;

let force { portended = t } = (Atomic_lazy.force__contended t).portended
let map t ~f = from_fun (fun () -> f (force t))
let bind t ~f = from_fun (fun () -> force (f (force t)))
let compare compare_a t1 t2 = compare_a (force t1) (force t2)
let compare__local compare_a t1 t2 = compare_a (force t1) (force t2)
let equal equal_a t1 t2 = equal_a (force t1) (force t2)
let equal__local equal_a t1 t2 = equal_a (force t1) (force t2)
let globalize ga { portended = t } = { portended = Atomic_lazy.globalize__contended ga t }
let is_val { portended = t } = Atomic_lazy.is_val__contended t

let peek { portended = t } : _ Or_null_shim.t =
  match Atomic_lazy.peek__contended t with
  | This value -> This value.portended
  | Null -> Null
;;

let peek_opt { portended = t } =
  match Atomic_lazy.peek_opt__contended t with
  | Some value -> Some value.portended
  | None -> None
;;
