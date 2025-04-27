open! Base
open Basement

(* Both [Mutex.t] and [Data.t] are [value mod portable contended]. *)

type 'k _mutex : value mod contended portable = 'k Capsule.Mutex.t
type ('a, 'k) _data : value mod contended portable = ('a, 'k) Capsule.Data.t

(* Packed mutexes are [value mod portable contended]. *)

type _packed : value mod contended portable = Capsule.Mutex.packed
type 'a myref = { mutable v : 'a }

module Cell = struct
  type 'a t = Mk : 'k Capsule.Mutex.t * ('a myref, 'k) Capsule.Data.t -> 'a t

  let create (type a : value mod contended portable) (x : a) : a t =
    let (P k) = Capsule.create () in
    let m = Capsule.Mutex.create k in
    let p = Capsule.Data.create (fun () -> { v = x }) in
    Mk (m, p)
  ;;

  let read (type a : value mod contended portable) (t : a t) : a =
    let (Mk (m, p)) = t in
    Capsule.Mutex.with_lock m ~f:(fun password ->
      let read' : a myref -> a @ contended portable @@ portable = fun r -> r.v in
      Capsule.Data.extract p ~password ~f:read')
  ;;

  let write (type a : value mod contended portable) (t : a t) (x : a) =
    let (Mk (m, p)) = t in
    Capsule.Mutex.with_lock m ~f:(fun password ->
      Capsule.Data.iter p ~password ~f:(fun r -> r.v <- x))
  ;;
end

let%expect_test "[Cell.create], [read], and [write]" =
  let ptr = Cell.create 42 in
  [%test_result: int] (Cell.read ptr) ~expect:42;
  Cell.write ptr 45;
  [%test_result: int] (Cell.read ptr) ~expect:45
;;

let ignore_initial_access : Capsule.initial Capsule.Access.t option @ local -> unit option
  = function
  | None -> None
  | Some _ -> Some ()
;;

let%expect_test "[get_initial] works in runtime4" =
  let () =
    Capsule.get_initial Stdlib_shim.Domain.Safe.DLS.Access.for_initial_domain
    |> ignore_initial_access
    |> Expect_test_helpers_base.require_some
  in
  [%expect {| |}]
;;

let%expect_test ("[get_initial] works in runtime5"
  [@tags "runtime5-only", "no-js", "no-wasm"])
  =
  let () =
    Capsule.get_initial Stdlib_shim.Domain.Safe.DLS.Access.for_initial_domain
    |> ignore_initial_access
    |> Expect_test_helpers_base.require_some
  in
  [%expect {| |}];
  let () =
    (Stdlib_shim.Domain.Safe.spawn [@alert "-unsafe_parallelism"]) (fun () ->
      Stdlib_shim.Domain.Safe.DLS.access (fun access ->
        (Capsule.get_initial access |> ignore_initial_access) [@nontail]))
    |> Domain.join
    |> Expect_test_helpers_base.require_none [%sexp_of: unit]
  in
  [%expect {| |}]
;;
