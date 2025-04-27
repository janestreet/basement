open! Base
open! Expect_test_helpers_base
open Basement
open Portable_test_helpers

[@@@alert "-unsafe_parallelism"]

(* Temporary portable redefinitions of functions from Base used by the tests in this
   module *)
include struct
  module Domain = struct
    include Domain
    include Stdlib_shim.Domain.Safe
  end

  let sexp_of_int = Stdlib.Obj.magic_portable sexp_of_int
  let print_s = Stdlib.Obj.magic_portable print_s
end

module%test [@name "single-domain behavior tests"] _ = struct
  module Operation = struct
    type t =
      | Get
      | Set_root of int
      | With_temporarily of int * t list

    let perform ts ~init =
      let dynamic = Dynamic.make init in
      let rec loop = function
        | Get -> [%message "get" ~_:(Dynamic.get dynamic : int)]
        | Set_root to_ ->
          Dynamic.set_root dynamic to_;
          [%message "set_root" ~_:(to_ : int)]
        | With_temporarily (to_, ts) ->
          let body =
            Dynamic.with_temporarily dynamic to_ ~f:(fun () -> List.map ts ~f:loop)
          in
          [%message "with_temporarily" ~_:(to_ : int) ~_:(body : Sexp.t list)]
      in
      List.map ts ~f:loop |> List.iter ~f:print_s
    ;;
  end

  let%expect_test "initial value" =
    Operation.perform ~init:0 [ Get ];
    [%expect {| (get 0) |}]
  ;;

  let%expect_test "[set_root] and [get]" =
    Operation.perform ~init:0 [ Set_root 1; Get; Set_root 2; Get ];
    [%expect
      {|
      (set_root 1)
      (get 1)
      (set_root 2)
      (get 2)
      |}]
  ;;

  let%expect_test "[with_temporarily]" =
    Operation.perform
      ~init:0
      [ Get
      ; With_temporarily (1, [ Get ])
      ; Get
      ; With_temporarily (2, [ Get; Set_root 3; Get ])
      ; Get
      ; With_temporarily
          ( 4
          , [ Get
            ; With_temporarily (5, [ Get ])
            ; Get
            ; With_temporarily (6, [ Get ])
            ; Get
            ] )
      ; Get
      ; With_temporarily (7, [ Get; With_temporarily (8, [ Get; Set_root 9; Get ]); Get ])
      ; Get
      ];
    [%expect
      {|
      (get 0)
      (with_temporarily 1 ((get 1)))
      (get 0)
      (with_temporarily 2 (
        (get      2)
        (set_root 3)
        (get      2)))
      (get 3)
      (with_temporarily 4 (
        (get 4)
        (with_temporarily 5 ((get 5)))
        (get 4)
        (with_temporarily 6 ((get 6)))
        (get 4)))
      (get 3)
      (with_temporarily 7 (
        (get 7)
        (with_temporarily 8 (
          (get      8)
          (set_root 9)
          (get      8)))
        (get 7)))
      (get 9)
      |}]
  ;;
end

let%expect_test ("set_root from two domains" [@tags "runtime5-only", "no-js", "no-wasm"]) =
  let dynamic = Dynamic.make 0 in
  let barrier = Barrier.create 2 in
  let d1 =
    Domain.spawn (fun () ->
      Barrier.await barrier;
      let before_d1_set = Dynamic.get dynamic (* 0 *) in
      Dynamic.set_root dynamic 1;
      let after_d1_set = Dynamic.get dynamic (* 1 *) in
      Barrier.await barrier;
      Barrier.await barrier;
      let after_d2_set = Dynamic.get dynamic (* 2 *) in
      [%message (before_d1_set : int) (after_d1_set : int) (after_d2_set : int)])
  in
  let d2 =
    Domain.spawn (fun () ->
      let before_d1_set = Dynamic.get dynamic (* 0 *) in
      Barrier.await barrier;
      Barrier.await barrier;
      let after_d1_set = Dynamic.get dynamic (* 1 *) in
      Dynamic.set_root dynamic 2;
      Barrier.await barrier;
      let after_d2_set = Dynamic.get dynamic (* 2 *) in
      [%message (before_d1_set : int) (after_d1_set : int) (after_d2_set : int)])
  in
  let results_from_d1 = Domain.join d1 in
  let results_from_d2 = Domain.join d2 in
  let value_in_initial_domain = Dynamic.get dynamic in
  print_s
    [%message
      (results_from_d1 : Sexp.t)
        (results_from_d2 : Sexp.t)
        (value_in_initial_domain : int)];
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
     (value_in_initial_domain 2))
    |}]
;;

let%expect_test ("with_temporarily from two domains"
  [@tags "runtime5-only", "no-js", "no-wasm"])
  =
  let dynamic = Dynamic.make 0 in
  let barrier = Barrier.create 2 in
  let spawn_domain_with_value_with_temporarily_to v =
    Domain.spawn (fun () ->
      Barrier.await barrier;
      let value_in_with_temporarily =
        Dynamic.with_temporarily dynamic v ~f:(fun () -> Dynamic.get dynamic)
      in
      let value_after_with_temporarily = Dynamic.get dynamic in
      value_in_with_temporarily, value_after_with_temporarily)
  in
  let d1 = spawn_domain_with_value_with_temporarily_to 1 in
  let d2 = spawn_domain_with_value_with_temporarily_to 2 in
  let value_in_with_temporarily_from_d1, value_after_with_temporarily_from_d1 =
    Domain.join d1
  in
  let value_in_with_temporarily_from_d2, value_after_with_temporarily_from_d2 =
    Domain.join d2
  in
  let value_in_initial_domain = Dynamic.get dynamic in
  print_s
    [%message
      (value_in_with_temporarily_from_d1 : int)
        (value_after_with_temporarily_from_d1 : int)
        (value_in_with_temporarily_from_d2 : int)
        (value_after_with_temporarily_from_d2 : int)
        (value_in_initial_domain : int)];
  [%expect
    {|
    ((value_in_with_temporarily_from_d1    1)
     (value_after_with_temporarily_from_d1 0)
     (value_in_with_temporarily_from_d2    2)
     (value_after_with_temporarily_from_d2 0)
     (value_in_initial_domain              0))
    |}]
;;

let%expect_test ("interleaved with_temporarily and set_root"
  [@tags "runtime5-only", "no-js", "no-wasm"])
  =
  let dynamic = Dynamic.make 0 in
  let barrier = Barrier.create 2 in
  let d1 =
    Domain.spawn (fun () ->
      Barrier.await barrier;
      let before_set_root = Dynamic.get dynamic in
      Barrier.await barrier;
      Dynamic.set_root dynamic 1;
      let after_set_root = Dynamic.get dynamic in
      Barrier.await barrier;
      [%message (before_set_root : int) (after_set_root : int)])
  in
  let d2 =
    Domain.spawn (fun () ->
      Barrier.await barrier;
      let before_set_root = Dynamic.get dynamic in
      let before_set_root_in_with_temporarily, after_set_root_in_with_temporarily =
        Dynamic.with_temporarily dynamic 2 ~f:(fun () ->
          let before_set_root_in_with_temporarily = Dynamic.get dynamic in
          Barrier.await barrier;
          Barrier.await barrier;
          let after_set_root_in_with_temporarily = Dynamic.get dynamic in
          before_set_root_in_with_temporarily, after_set_root_in_with_temporarily)
      in
      let after_with_temporarily = Dynamic.get dynamic in
      [%message
        (before_set_root : int)
          (before_set_root_in_with_temporarily : int)
          (after_set_root_in_with_temporarily : int)
          (after_with_temporarily : int)])
  in
  let results_from_d1 = Domain.join d1 in
  let results_from_d2 = Domain.join d2 in
  print_s [%message (results_from_d1 : Sexp.t) (results_from_d2 : Sexp.t)];
  [%expect
    {|
    ((results_from_d1 (
       (before_set_root 0)
       (after_set_root  1)))
     (results_from_d2 (
       (before_set_root                     0)
       (before_set_root_in_with_temporarily 2)
       (after_set_root_in_with_temporarily  2)
       (after_with_temporarily              1))))
    |}]
;;
