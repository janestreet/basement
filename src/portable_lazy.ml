module T = struct
  type ('a : value_or_null) inner =
    | Uncomputed of ('a t -> 'a @ contended portable) @@ portable
    | Computing
    | Computed of 'a @@ contended portable
    | Error of exn

  and ('a : value_or_null) t = { inner : 'a inner Portable_atomic.t @@ global }
  [@@unboxed]

  let from_val value = { inner = Portable_atomic.make (Computed value) }

  (* We manually enforce that the thunk stored inside an [Uncomputed] lazy is only called
     once, by only exposing an API that allows each function to be called once, and have
     manually verified (via careful code review and tests) that all possible thread
     interleavings of calls to [force] only call the function once. This property cannot be
     enforced by the type system - so we have to use magic here to assert that the function
     cannot be called more than once. *)
  external magic_many_lazy_thunk
    :  'a @ once portable
    -> 'a @ many portable
    @@ portable
    = "%identity"

  let from_fun_fixed func =
    { inner = Portable_atomic.make (Uncomputed (magic_many_lazy_thunk func)) }
  ;;

  let from_fun func = from_fun_fixed (fun _ -> func ())

  let rec force ({ inner = atomic } as t) =
    match Portable_atomic.get atomic with
    | Computed value -> value
    | Error exn -> raise exn
    | Computing ->
      (* Someone else is forcing the lazy; relax then keep trying. *)
      Domain.cpu_relax ();
      force t
    | Uncomputed f as uncomputed ->
      let computing = Computing in
      (match Portable_atomic.compare_and_set atomic uncomputed computing with
       | false ->
         (* Someone else beat us to starting the thunk! This can only happen to us once, so
            we just try again without any calls to cpu_relax. *)
         force t
       | true ->
         let computed =
           try Computed (f { inner = atomic }) with
           | exn -> Error exn
         in
         (* We know we must have been the one to set it to [Computing], so we can just [set]
            here to set it to the result. If we had relaxed stores, we'd use one here. *)
         Portable_atomic.set atomic computed;
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
  let globalize _ ({ inner } @ local) = { inner }

  let peek { inner } =
    match Portable_atomic.get inner with
    | Computed x -> Or_null_shim.This x
    | Computing | Uncomputed _ | Error _ -> Or_null_shim.Null
  ;;

  let peek_opt { inner } =
    match Portable_atomic.get inner with
    | Computed x -> Some x
    | Computing | Uncomputed _ | Error _ -> None
  ;;

  let is_val { inner } =
    match Portable_atomic.get inner with
    | Computed _ -> true
    | Computing | Uncomputed _ | Error _ -> false
  ;;
end

(* Below is unsafe magic, applied safely, to treat ['a t] as covariant
   in ['a], despite the ['a t] type not being inferrable as covariant.

   The safety of this magic hinges on the fact that the operations exposed
   in [S] are covariant in ['a]. (An example of an operation that wouldn't
   be covariant in ['a]: [val set : 'a t -> 'a -> unit], that sets the
   value of a lazy to something other than the computation it was originally
   specified as.)
*)

module type S = sig @@ portable
  type ('a : value_or_null) t : value mod contended portable

  val from_val : ('a : value_or_null). 'a @ contended portable -> 'a t

  val from_fun
    : ('a : value_or_null).
    (unit -> 'a @ contended portable) @ once portable -> 'a t

  val from_fun_fixed
    : ('a : value_or_null).
    ('a t -> 'a @ contended portable) @ once portable -> 'a t

  val force : ('a : value_or_null). 'a t @ local -> 'a @ contended portable

  val map
    : ('a : value_or_null) ('b : value_or_null).
    'a t -> f:('a @ contended portable -> 'b @ contended portable) @ once portable -> 'b t

  val bind
    : ('a : value_or_null) ('b : value_or_null).
    'a t -> f:('a @ contended portable -> 'b t) @ once portable -> 'b t

  val compare
    : ('a : value_or_null mod contended).
    ('a -> 'a -> int) -> 'a t -> 'a t -> int

  val compare__local
    : ('a : value_or_null mod contended).
    ('a @ local -> 'a @ local -> int) -> 'a t @ local -> 'a t @ local -> int

  val equal
    : ('a : value_or_null mod contended).
    ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val equal__local
    : ('a : value_or_null mod contended).
    ('a @ local -> 'a @ local -> bool) -> 'a t @ local -> 'a t @ local -> bool

  val globalize : ('a : value_or_null) ('b : value_or_null). 'b -> 'a t @ local -> 'a t
  val is_val : ('a : value_or_null). 'a t -> bool
  val peek : ('a : value). 'a t -> 'a Or_null_shim.t @ contended portable
  val peek_opt : ('a : value_or_null). 'a t -> 'a option @ contended portable
end

module type S_covariant = sig
  type (+'a : value_or_null) t : value mod contended portable

  (* For the below magic to be safe, it's important that no value bindings appear here
     besides those in [S] itself. *)

  include S with type ('a : value_or_null) t := 'a t
end

external magically_covariant : (module S) -> (module S_covariant) = "%identity"

include (val magically_covariant (module T : S))
