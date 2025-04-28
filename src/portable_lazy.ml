module Atomic = struct
  (* Use the multidomain-safe version of [Atomic] functions. *)
  include Atomic
  include Stdlib_shim.Atomic.Safe
end

module Domain : sig
  type id

  val equal_id : id -> id -> bool
  val cpu_relax : unit -> unit
  val self : unit -> id
end = struct
  type id = int

  let equal_id = Int.equal
  let cpu_relax = if Stdlib_shim.runtime5 () then Domain.cpu_relax else fun () -> ()

  let self =
    if Stdlib_shim.runtime5 () then fun () -> (Domain.self () :> id) else fun () -> 1
  ;;
end

exception Undefined
exception Force_raised of string

type 'a inner =
  | Uncomputed of ('a t -> 'a)
  | Computing of Domain.id
  | Computed of 'a
  | Error of string

and 'a t = { inner : 'a inner Atomic.t [@globalized] } [@@unboxed]

let from_val value = { inner = Atomic.make (Computed value) }

(* We manually enforce that the thunk stored inside an [Uncomputed] lazy is only called
   once, by only exposing an API that allows each function to be called once, and have
   manually verified (via careful code review and tests) that all possible thread
   interleavings of calls to [force] only call the function once. This property cannot be
   enforced by the type system - so we have to use magic here to assert that the function
   cannot be called more than once. *)
external magic_many_lazy_thunk : 'a -> 'a = "%identity"

let from_fun_fixed func =
  { inner = Atomic.make (Uncomputed (magic_many_lazy_thunk func)) }
;;

let from_fun func = from_fun_fixed (fun _ -> func ())

let rec force ({ inner = atomic } as t) =
  match Atomic.get atomic with
  | Computed value -> value
  | Error msg -> raise (Force_raised msg)
  | Computing by_domain ->
    if Domain.equal_id by_domain (Domain.self ())
    then
      (* The lazy is already being forced by this domain! That's an infinite loop; just
         raise. *)
      raise Undefined
    else (
      (* Someone else is forcing the lazy; relax then keep trying. *)
      Domain.cpu_relax ();
      force t)
  | Uncomputed f as uncomputed ->
    let computing = Computing (Domain.self ()) in
    (match Atomic.compare_and_set atomic uncomputed computing with
     | false ->
       (* Someone else beat us to starting the thunk! This can only happen to us once, so
         we just try again without any calls to cpu_relax. *)
       force t
     | true ->
       let computed =
         try Computed (f { inner = atomic }) with
         | exn -> Error (Printexc.to_string exn)
       in
       (* We know we must have been the one to set it to [Computing], so we can just [set]
         here to set it to the result. If we had relaxed stores, we'd use one here. *)
       Atomic.set atomic computed;
       (* We know that we set the value to either [Uncomputed] or [Error] - in either case
         we won't end up in [Uncomputed] again, so it's fine to just recurse here to
         either raise the error or return the result *)
       force t)
;;

let map t ~f = from_fun (fun () -> f (force t))
let bind t ~f = from_fun (fun () -> force (f (force t)))
let compare compare_a t1 t2 = compare_a (force t1) (force t2)
let compare__local compare_a t1 t2 = compare_a (force t1) (force t2)
let equal equal_a t1 t2 = equal_a (force t1) (force t2)
let equal__local equal_a t1 t2 = equal_a (force t1) (force t2)
let globalize _ { inner } = { inner }

let peek { inner } =
  match Atomic.get inner with
  | Computed x -> Some x
  | Computing _ | Uncomputed _ | Error _ -> None
;;

let is_val { inner } =
  match Atomic.get inner with
  | Computed _ -> true
  | Computing _ | Uncomputed _ | Error _ -> false
;;
