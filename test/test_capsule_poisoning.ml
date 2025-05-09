open! Base
open Basement
open Expect_test_helpers_base

let m =
  let (P k) = Capsule.create () in
  Capsule.Mutex.P (Capsule.Mutex.create k)
;;

let rw =
  let (P k) = Capsule.create () in
  Capsule.Rwlock.P (Capsule.Rwlock.create k)
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

let%expect_test "[Capsule.Mutex] basics" =
  let x = ref 1 in
  let (P m) = m in
  Capsule.Mutex.with_lock m ~f:(fun _ -> x := 2);
  [%test_result: int] !x ~expect:2
;;

let%expect_test "Re-locking already locked [Mutex.t] raises [Sys_error]" =
  let (P m) = m in
  Capsule.Mutex.with_lock m ~f:(fun _ ->
    require_sys_error (fun () -> Capsule.Mutex.with_lock m ~f:(fun _ -> ())))
;;

let%expect_test "Write-locking already write locked [Rwlock.t] raises [Sys_error]" =
  let (P rw) = rw in
  Capsule.Rwlock.with_write_lock rw ~f:(fun _ ->
    require_sys_error (fun () -> Capsule.Rwlock.with_write_lock rw ~f:(fun _ -> ())))
;;

let%expect_test "Read-locking already write locked [Rwlock.t] raises [Sys_error]" =
  let (P rw) = rw in
  Capsule.Rwlock.with_write_lock rw ~f:(fun _ ->
    require_sys_error (fun () -> Capsule.Rwlock.with_read_lock rw ~f:(fun _ -> ())))
;;

let%expect_test "Read-locking already read locked [Rwlock.t] does nothing" =
  let (P rw) = rw in
  Capsule.Rwlock.with_read_lock rw ~f:(fun _ ->
    require_does_not_raise (fun () -> Capsule.Rwlock.with_read_lock rw ~f:(fun _ -> ()));
    require_does_not_raise (fun () -> Capsule.Rwlock.with_read_lock rw ~f:(fun _ -> ())))
;;

let%expect_test ("Write-locking already read locked [Rwlock.t] raises [Sys_error]"
  (* On native, this deadlocks *)
  [@tags "js-only", "wasm-only"])
  =
  let (P rw) = rw in
  Capsule.Rwlock.with_read_lock rw ~f:(fun _ ->
    require_does_not_raise (fun () -> Capsule.Rwlock.with_read_lock rw ~f:(fun _ -> ()));
    require_sys_error (fun () -> Capsule.Rwlock.with_write_lock rw ~f:(fun _ -> ())))
;;

let%expect_test ("Waiting on a condition raises [Sys_error] in single-threaded platforms"
  (* While runtime4 is single-domain, it is not single-threaded *)
  [@tags "js-only", "wasm-only"])
  =
  let (P m) = m in
  let c = Capsule.Condition.create () in
  Capsule.Mutex.with_lock m ~f:(fun password ->
    require_sys_error (fun () ->
      (Capsule.Condition.wait [@alert "-unsafe"]) c ~password ~mutex:m)
    [@nontail])
;;

let%expect_test "Exceptions propagate through [with_lock]" =
  let (P m) = m in
  match Capsule.Mutex.with_lock m ~f:(fun _ -> raise Division_by_zero) with
  | exception Division_by_zero -> ()
  | _ -> assert false
;;

let%expect_test "Operations on poisoned mutex raise [Poisoned]" =
  let (P m) = m in
  match Capsule.Mutex.with_lock m ~f:(fun _ -> raise Division_by_zero) with
  | exception Capsule.Mutex.Poisoned -> ()
  | _ -> assert false
;;

(* Reset *)
let m =
  let (P k) = Capsule.create () in
  Capsule.Mutex.P (Capsule.Mutex.create k)
;;

let%expect_test "Reset mutex works normally" =
  let x = ref 1 in
  let (P m) = m in
  Capsule.Mutex.with_lock m ~f:(fun _ -> x := 2);
  [%test_result: int] !x ~expect:2
;;

let%expect_test "Destroying mutex returns a key" =
  let (P m) = m in
  let _k : _ Capsule.Key.t = Capsule.Mutex.destroy m in
  ()
;;

let%expect_test "Destroyed mutex is [Poisoned]" =
  let (P m) = m in
  match Capsule.Mutex.with_lock m ~f:(fun _ -> raise Division_by_zero) with
  | exception Capsule.Mutex.Poisoned -> ()
  | _ -> assert false
;;

let%expect_test "Destroying a poisoned mutex raises [Poisoned]" =
  let (P m) = m in
  match Capsule.Mutex.destroy m with
  | exception Capsule.Mutex.Poisoned -> ()
  | _ -> assert false
;;
