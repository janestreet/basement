open! Base
open Basement
open Blocking_sync [@@alert "-deprecated"]

(* Both [Mutex.t] and [Data.t] are [value mod portable contended]. *)

type 'k _mutex : value mod contended portable = 'k Mutex.t
type ('a, 'k) _data : value mod contended portable = ('a, 'k) Capsule.Data.t

(* Packed mutexes are [value mod portable contended]. *)

type _packed : value mod contended portable = Mutex.packed
type 'a myref = { mutable v : 'a }

module Cell = struct
  type 'a t = Mk : 'k Mutex.t * ('a myref, 'k) Capsule.Data.t -> 'a t

  let create (type a : value mod contended portable) (x : a) : a t =
    let (P k) = Capsule.create () in
    let m = Mutex.create k in
    let p = Capsule.Data.create (fun () -> { v = x }) in
    Mk (m, p)
  ;;

  let read (type a : value mod contended portable) (t : a t) : a =
    let (Mk (m, p)) = t in
    (Mutex.with_lock m ~f:(fun password ->
       let read' : (a myref -> a aliased @ contended once portable unique) @ portable =
         fun r -> { aliased = r.v }
       in
       Capsule.Data.extract p ~password ~f:read'))
      .aliased
  ;;

  let write (type a : value mod contended portable) (t : a t) (x : a) =
    let (Mk (m, p)) = t in
    Mutex.with_lock m ~f:(fun password ->
      Capsule.Data.iter p ~password ~f:(fun r -> r.v <- x))
  ;;
end

let%expect_test "[Cell.create], [read], and [write]" =
  let ptr = Cell.create 42 in
  [%test_result: int] (Cell.read ptr) ~expect:42;
  Cell.write ptr 45;
  [%test_result: int] (Cell.read ptr) ~expect:45
;;

let%expect_test "[access_initial] works in runtime4" =
  Capsule.access_initial (function
    | Some (_ : Capsule.initial Capsule.Access.boxed) -> ()
    | None -> failwith "Didn't get initial access");
  [%expect {| |}]
;;
