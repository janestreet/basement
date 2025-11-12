(* This file is written to justify the soundness of the mode annotations in [Atomic_lazy].
   Specifically, the main implementation only defines the nonportable fragment of the
   interface, and the portable fragment is produced by magicking the nonportable fragment.

   The below implementation produces both the nonportable and portable fragments without
   using this magic, but at the cost of a slightly less efficient representation. The
   implementations of each of the two cases are syntactically different, but these
   differences are transparent, and only used for typing shenanigans. This can be observed
   with careful inspection as well as by inspection of the produced assembly. *)

open Basement

module Atomic = struct
  include Stdlib.Atomic
  include Stdlib_shim.Atomic
end

module Contended_via_portable = struct
  type (+'a : value_or_null) t : value_or_null mod contended

  external wrap
    : ('a : value_or_null).
    ('a[@local_opt]) -> ('a t[@local_opt])
    @@ portable
    = "%identity"

  external unwrap
    : ('a : value_or_null).
    ('a t[@local_opt]) -> ('a[@local_opt])
    @@ portable
    = "%identity"

  (* You can't get a portable [Contended_via_portable.t] as it is abstract, does not
     mode cross portability, and neither of the functions above produce one. So, we
     can ignore any cases where we have such a value. *)
  let refute_portable (_ : 'a t @ contended portable) = assert false
end

type ('a : value_or_null) portable = { portable : 'a @@ portable } [@@unboxed]

type ('a : value_or_null, 'k) inner =
  | Uncomputed of (('a, 'k) unpacked -> 'a)
  | Computing
  | Computed of 'a
  | Error of exn

and ('a : value_or_null, 'k) inner_portable =
  | Uncomputed_portable of
      (('a, 'k) unpacked @ contended portable -> 'a @ portable) @@ portable
  | Computing_portable
  | Computed_portable of ('a portable, 'k) Capsule.Data.Or_null.t
  | Error_portable of exn

and ('a : value_or_null, 'k) unpacked =
  | Nonportable of ('a, 'k) inner Atomic.t Contended_via_portable.t
  | Portable of ('a, 'k) inner_portable Atomic.t

type ('a : value_or_null) t =
  | P :
      ('a : value_or_null) 'k.
      #('k Capsule.Access.t * ('a, 'k) unpacked) @@ global
      -> ('a : value_or_null) t
[@@unboxed] [@@unsafe_allow_any_mode_crossing]

let from_val x =
  let (P access) = Capsule.current () in
  P #(access, Nonportable (Contended_via_portable.wrap (Atomic.make (Computed x))))
;;

let from_val__portable x =
  let (P access) = Capsule.current () in
  P
    #( access
     , Portable
         (Atomic.make
            (Computed_portable (Capsule.Data.Or_null.wrap ~access { portable = x }))) )
;;

(* We manually enforce that the thunk stored inside an [Uncomputed] lazy is only called
   once, by only exposing an API that allows each function to be called once, and have
   manually verified (via careful code review and tests) that all possible thread
   interleavings of calls to [force] only call the function once. This property cannot be
   enforced by the type system - so we have to use magic here to assert that the function
   cannot be called more than once. *)

external magic_many_lazy_thunk : 'a @ once -> 'a @ many @@ portable = "%identity"

external magic_many_lazy_thunk_portable
  :  'a @ once portable
  -> 'a @ many portable
  @@ portable
  = "%identity"

let from_fun_fixed f =
  let (P access) = Capsule.current () in
  P
    #( access
     , Nonportable
         (Contended_via_portable.wrap
            (Atomic.make
               (Uncomputed
                  (magic_many_lazy_thunk (fun unpacked -> f (P #(access, unpacked)))))))
     )
;;

let from_fun_fixed__portable f =
  let (P access) = Capsule.current () in
  P
    #( access
     , Portable
         (Atomic.make
            (Uncomputed_portable
               (magic_many_lazy_thunk_portable (fun unpacked -> f (P #(access, unpacked))))))
     )
;;

let rec force (P #(access, unpacked) as t @ nonportable) =
  match unpacked with
  | Nonportable cvp ->
    let atomic = Contended_via_portable.unwrap cvp in
    (match Atomic.get atomic with
     | Computing ->
       Domain.cpu_relax ();
       force t
     | Computed value -> value
     | Error exn -> raise exn
     | Uncomputed f as uncomputed ->
       (match Atomic.compare_and_set atomic uncomputed Computing with
        | false -> force t
        | true ->
          let computed =
            try Computed (f (Nonportable cvp)) with
            | exn -> Error exn
          in
          Atomic.set atomic computed;
          force t))
  | Portable atomic ->
    (match Atomic.get atomic with
     | Computing_portable ->
       Domain.cpu_relax ();
       force t
     | Computed_portable capsule ->
       let data = Capsule.Data.Or_null.unwrap ~access capsule in
       data.portable
     | Error_portable exn -> raise exn
     | Uncomputed_portable f as uncomputed ->
       (match Atomic.compare_and_set atomic uncomputed Computing_portable with
        | false -> force t
        | true ->
          let computed =
            try
              Computed_portable
                (Capsule.Data.Or_null.create (fun () ->
                   { portable = f (Portable atomic) }))
            with
            | exn -> Error_portable exn
          in
          Atomic.set atomic computed;
          force t))
;;

let rec force__contended (P #(_access, unpacked) as t @ contended portable) =
  match unpacked with
  | Nonportable cvp -> Contended_via_portable.refute_portable cvp
  | Portable atomic ->
    (match Atomic.get atomic with
     | Computing_portable ->
       Domain.cpu_relax ();
       force__contended t
     | Computed_portable capsule ->
       let data = Capsule.Data.Or_null.project capsule in
       data.portable
     | Error_portable exn -> raise exn
     | Uncomputed_portable f as uncomputed ->
       (match Atomic.compare_and_set atomic uncomputed Computing_portable with
        | false -> force__contended t
        | true ->
          let computed =
            try
              Computed_portable
                (Capsule.Data.Or_null.create (fun () -> { portable = f unpacked }))
            with
            | exn -> Error_portable exn
          in
          Atomic.set atomic computed;
          force__contended t))
;;

let is_val (P #(_access, unpacked)) =
  match unpacked with
  | Nonportable cvp ->
    (match Atomic.get (Contended_via_portable.unwrap cvp) with
     | Computed _ -> true
     | Uncomputed _ | Computing | Error _ -> false)
  | Portable atomic ->
    (match Atomic.get atomic with
     | Computed_portable _ -> true
     | Uncomputed_portable _ | Computing_portable | Error_portable _ -> false)
;;

let is_val__contended (P #(_access, unpacked)) =
  match unpacked with
  | Nonportable cvp -> Contended_via_portable.refute_portable cvp
  | Portable atomic ->
    (match Atomic.get_contended atomic with
     | Computed_portable _ -> true
     | Uncomputed_portable _ | Computing_portable | Error_portable _ -> false)
;;

let peek (P #(access, unpacked)) : _ Or_null_shim.t =
  match unpacked with
  | Nonportable cvp ->
    (match Atomic.get (Contended_via_portable.unwrap cvp) with
     | Computed value -> This value
     | Uncomputed _ | Computing | Error _ -> Null)
  | Portable atomic ->
    (match Atomic.get atomic with
     | Computed_portable value ->
       This (Capsule.Data.Or_null.unwrap ~access value).portable
     | Uncomputed_portable _ | Computing_portable | Error_portable _ -> Null)
;;

let peek__contended (P #(_access, unpacked)) : _ Or_null_shim.t =
  match unpacked with
  | Nonportable cvp -> Contended_via_portable.refute_portable cvp
  | Portable atomic ->
    (match Atomic.get_contended atomic with
     | Computed_portable value -> This (Capsule.Data.Or_null.project value).portable
     | Uncomputed_portable _ | Computing_portable | Error_portable _ -> Null)
;;

let peek_opt (P #(access, unpacked)) =
  match unpacked with
  | Nonportable cvp ->
    (match Atomic.get (Contended_via_portable.unwrap cvp) with
     | Computed value -> Some value
     | Uncomputed _ | Computing | Error _ -> None)
  | Portable atomic ->
    (match Atomic.get atomic with
     | Computed_portable value ->
       Some (Capsule.Data.Or_null.unwrap ~access value).portable
     | Uncomputed_portable _ | Computing_portable | Error_portable _ -> None)
;;

let peek_opt__contended (P #(_access, unpacked)) =
  match unpacked with
  | Nonportable cvp -> Contended_via_portable.refute_portable cvp
  | Portable atomic ->
    (match Atomic.get_contended atomic with
     | Computed_portable value -> Some (Capsule.Data.Or_null.project value).portable
     | Uncomputed_portable _ | Computing_portable | Error_portable _ -> None)
;;

let from_fun f = from_fun_fixed (fun _ -> f ())
let from_fun__portable f = from_fun_fixed__portable (fun _ -> f ())
let globalize _ (P #(access, unpacked)) = P #(access, unpacked)
let globalize__contended _ (P #(access, unpacked)) = P #(access, unpacked)
