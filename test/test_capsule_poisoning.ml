open! Base
open Basement

let m =
  let (P k) = Capsule.create () in
  Capsule.Mutex.P (Capsule.Mutex.create k)
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
    match Capsule.Mutex.with_lock m ~f:(fun _ -> ()) with
    | exception Sys_error _ -> ()
    | _ -> assert false)
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
