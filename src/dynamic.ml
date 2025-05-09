module Domain = struct
  include Domain
  include Stdlib_shim.Domain.Safe
end

(* NOTE: This module contains a "stub" implementation of dynamically scoped variables,
   intended as a safe stop-gap until we add support for native dynamic scoping in the
   runtime. It is currently unsafe to use this module in the presence of fibers. *)

(** [Cell.t] is an abstraction over a mutable cell (backed by an atomic) with simple
    permissioning. This makes it a little easier to understand how the fields of
    [Dynamic.t] are used, which can otherwise be a bit confusing due to an optimization
    described below. *)
module Cell : sig
  type ('a, +'perm) t

  val make : 'a. 'a -> ('a, [< `read | `write ]) t
  val make_readonly : 'a. 'a -> ('a, [< `read ]) t
  val get : 'a. ('a, [> `read ]) t -> 'a
  val set : 'a. ('a, [> `write ]) t -> 'a -> unit
end = struct
  type ('a, +_) t = 'a Portable_atomic.t

  let[@inline] make x = Portable_atomic.make x
  let[@inline] make_readonly x = make x
  let[@inline] get t = Portable_atomic.get t
  let[@inline] set t x = Portable_atomic.set t x
end

(** A ['a t] represents a dynamically scoped variable.

    Conceptually, this could just be
    {[
      type 'a t =
        { root : 'a Modes.Portable.t Atomic.t
        ; current : 'a Modes.Portable.t option Domain.DLS.key
        }
    ]}
    where the current value is [v] when [current = Some v], and [root] otherwise. This
    requires a conditional jump, though, which we avoid by having [current] be an alias
    for [root] rather than [None]. *)
type 'a t =
  { root : ('a, [ `write ]) Cell.t
  (** The default value when not inside a [with_temporarily]. This is shared by all
      domains and can be set at any time. *)
  ; current : ('a, [ `read ]) Cell.t Domain.DLS.key
  (** The value of the dynamic variable for the current domain (conceptually, fiber).
      Inside the scope of a [with_temporarily], [current] is set to a constant [Cell.t];
      outside the scope of all [with_temporarily]'s, [current] is an alias for [root]. *)
  }
[@@unsafe_allow_any_mode_crossing
  "TODO: illegal mode crossing on the current version of the compiler, but should be \
   legal."]

let make x =
  let root = Cell.make x in
  let current =
    Domain.DLS.new_key
      (fun () ->
        (* Any already-existing domains are initialized to point at [root]. *)
        root)
      ~split_from_parent:(fun (current : _ Cell.t) () ->
        (* If the dynamic is currently explicitly set by the user, then we want to
           preserve that set when we split. If it hasn't been set by the user, then
           [current] is just [root]. *)
        current)
  in
  { root; current }
;;

let get t = Domain.DLS.access (fun access -> Domain.DLS.get access t.current) |> Cell.get
let set_root t x = Cell.set t.root x

let with_temporarily t x ~f =
  let new_cell = Cell.make_readonly x in
  let restore_to =
    (* Set the [current] DLS value to [new_cell], and return the previous cell. *)
    Domain.DLS.access (fun access ->
      let restore_to = Domain.DLS.get access t.current in
      Domain.DLS.set access t.current new_cell;
      restore_to)
  in
  let restore () =
    (* Set the [current] DLS value back to [restore_to]. *)
    Domain.DLS.access (fun access -> Domain.DLS.set access t.current restore_to)
  in
  match f () with
  | res ->
    restore ();
    res
  | exception exn ->
    let bt = Stdlib.Printexc.get_raw_backtrace () in
    restore ();
    Stdlib.Printexc.raise_with_backtrace exn bt
;;
