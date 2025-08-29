open! Base
open! Expect_test_helpers_base
open Basement
open Portable_test_helpers

let%expect_test "set_root from two threads" =
  let dynamic = Dynamic.make 0 in
  let barrier = Barrier.create 2 in
  let done_barrier = Barrier.create 3 in
  let result1 : Sexp.t Stdlib.Atomic.t = Stdlib.Atomic.make (Sexp.Atom "start") in
  let result2 : Sexp.t Stdlib.Atomic.t = Stdlib.Atomic.make (Sexp.Atom "start") in
  (match
     Multicore.spawn
       (fun () ->
         Barrier.await barrier;
         let before_d1_set = Dynamic.get dynamic (* 0 *) in
         Dynamic.set_root dynamic 1;
         let after_d1_set = Dynamic.get dynamic (* 1 *) in
         Barrier.await barrier;
         Barrier.await barrier;
         let after_d2_set = Dynamic.get dynamic (* 2 *) in
         Stdlib.Atomic.Contended.set
           result1
           [%message (before_d1_set : int) (after_d1_set : int) (after_d2_set : int)];
         Barrier.await done_barrier)
       ()
   with
   | Spawned -> ()
   | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt);
  (match
     Multicore.spawn
       (fun () ->
         let before_d1_set = Dynamic.get dynamic (* 0 *) in
         Barrier.await barrier;
         Barrier.await barrier;
         let after_d1_set = Dynamic.get dynamic (* 1 *) in
         Dynamic.set_root dynamic 2;
         Barrier.await barrier;
         let after_d2_set = Dynamic.get dynamic (* 2 *) in
         Stdlib.Atomic.Contended.set
           result2
           [%message (before_d1_set : int) (after_d1_set : int) (after_d2_set : int)];
         Barrier.await done_barrier)
       ()
   with
   | Spawned -> ()
   | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt);
  Barrier.await done_barrier;
  let results_from_d1 = Stdlib.Atomic.get result1 in
  let results_from_d2 = Stdlib.Atomic.get result2 in
  let value_in_initial_thread = Dynamic.get dynamic in
  print_s
    [%message
      (results_from_d1 : Sexp.t)
        (results_from_d2 : Sexp.t)
        (value_in_initial_thread : int)];
  [%expect
    {|
    ((results_from_d1 (
       (before_d1_set 0)
       (after_d1_set  1)
       (after_d2_set  2)))
     (results_from_d2 (
       (before_d1_set 0)
       (after_d1_set  1)
       (after_d2_set  2)))
     (value_in_initial_thread 2))
    |}]
;;

let%expect_test "with_temporarily from two threads" =
  let dynamic = Dynamic.make 0 in
  let barrier = Barrier.create 2 in
  let done_barrier = Barrier.create 3 in
  let result1 = Stdlib.Atomic.make (0, 0) in
  let result2 = Stdlib.Atomic.make (0, 0) in
  let spawn_thread_with_value_with_temporarily_to (result : (int * int) Stdlib.Atomic.t) v
    =
    match
      Multicore.spawn
        (fun () ->
          Barrier.await barrier;
          let value_in_with_temporarily =
            Dynamic.with_temporarily dynamic v ~f:(fun () -> Dynamic.get dynamic)
          in
          let value_after_with_temporarily = Dynamic.get dynamic in
          Stdlib.Atomic.Contended.set
            result
            (value_in_with_temporarily, value_after_with_temporarily);
          Barrier.await done_barrier)
        ()
    with
    | Spawned -> ()
    | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt
  in
  spawn_thread_with_value_with_temporarily_to result1 1;
  spawn_thread_with_value_with_temporarily_to result2 2;
  Barrier.await done_barrier;
  let value_in_with_temporarily_from_d1, value_after_with_temporarily_from_d1 =
    Stdlib.Atomic.get result1
  in
  let value_in_with_temporarily_from_d2, value_after_with_temporarily_from_d2 =
    Stdlib.Atomic.get result2
  in
  let value_in_initial_thread = Dynamic.get dynamic in
  print_s
    [%message
      (value_in_with_temporarily_from_d1 : int)
        (value_after_with_temporarily_from_d1 : int)
        (value_in_with_temporarily_from_d2 : int)
        (value_after_with_temporarily_from_d2 : int)
        (value_in_initial_thread : int)];
  [%expect
    {|
    ((value_in_with_temporarily_from_d1    1)
     (value_after_with_temporarily_from_d1 0)
     (value_in_with_temporarily_from_d2    2)
     (value_after_with_temporarily_from_d2 0)
     (value_in_initial_thread              0))
    |}]
;;
