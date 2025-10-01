open! Base
open Basement
open Blocking_sync [@@alert "-deprecated"]
open Expect_test_helpers_base

let m =
  let (P k) = Capsule.create () in
  Mutex.P (Mutex.create k)
;;

let require_sys_error f =
  match f () with
  | exception Sys_error _ -> ()
  | exception exn ->
    print_cr
      [%message
        "Expected function to raise [Sys_error], but it raised a different exception"
          ~_:(exn : exn)]
  | _ ->
    print_cr [%message "Expected function to raise [Sys_error], but it did not raise"]
;;

let%expect_test "[Mutex] basics" =
  let x = ref 1 in
  let (P m) = m in
  Mutex.with_lock m ~f:(fun _ -> x := 2);
  [%test_result: int] !x ~expect:2
;;

let%expect_test ("Waiting on a condition raises [Sys_error] in single-threaded platforms"
  (* While runtime4 is single-domain, it is not single-threaded *)
  [@tags "js-only", "wasm-only"])
  =
  let (P m) = m in
  let c = Condition.create () in
  require_sys_error (fun () ->
    Mutex.with_key m ~f:(fun key -> #((), Condition.wait c ~mutex:m key)))
;;

(* Reset *)
let m =
  let (P k) = Capsule.create () in
  Mutex.P (Mutex.create k)
;;

let%expect_test "Exceptions propagate through [with_lock]" =
  let (P m) = m in
  match Mutex.with_lock m ~f:(fun _ -> raise Division_by_zero) with
  | exception Division_by_zero -> ()
  | _ -> assert false
;;

let%expect_test "Operations on poisoned mutex raise [Poisoned]" =
  let (P m) = m in
  match Mutex.with_lock m ~f:(fun _ -> raise Division_by_zero) with
  | exception Mutex.Poisoned -> ()
  | _ -> assert false
;;

(* Reset *)
let m =
  let (P k) = Capsule.create () in
  Mutex.P (Mutex.create k)
;;

let%expect_test "Reset mutex works normally" =
  let x = ref 1 in
  let (P m) = m in
  Mutex.with_lock m ~f:(fun _ -> x := 2);
  [%test_result: int] !x ~expect:2
;;

let%expect_test "Destroying mutex returns a key" =
  let (P m) = m in
  let _k : _ Capsule.Key.t = Mutex.destroy m in
  ()
;;

let%expect_test "Destroyed mutex is [Poisoned]" =
  let (P m) = m in
  match Mutex.with_lock m ~f:(fun _ -> raise Division_by_zero) with
  | exception Mutex.Poisoned -> ()
  | _ -> assert false
;;

let%expect_test "Destroying a poisoned mutex raises [Poisoned]" =
  let (P m) = m in
  match Mutex.destroy m with
  | exception Mutex.Poisoned -> ()
  | _ -> assert false
;;
