open! Base
open Expect_test_helpers_base
open Basement

let%expect_test _ =
  let t = Portable_lazy.from_val 1 in
  print_s [%message (Portable_lazy.is_val t : bool)];
  print_s [%message (Portable_lazy.force t : int)];
  print_s [%message (Portable_lazy.is_val t : bool)];
  [%expect
    {|
    ("Portable_lazy.is_val t" true)
    ("Portable_lazy.force t" 1)
    ("Portable_lazy.is_val t" true)
    |}]
;;

let%expect_test _ =
  let t = Portable_lazy.from_fun (fun () -> 2 + 2) in
  print_s [%message (Portable_lazy.is_val t : bool)];
  print_s [%message (Portable_lazy.force t : int)];
  print_s [%message (Portable_lazy.is_val t : bool)];
  [%expect
    {|
    ("Portable_lazy.is_val t" false)
    ("Portable_lazy.force t" 4)
    ("Portable_lazy.is_val t" true)
    |}]
;;

let%expect_test "force always returns the same value" =
  let t = Portable_lazy.from_fun (fun () -> Portable_atomic.make 0) in
  let force1 = Portable_lazy.force t in
  let force2 = Portable_lazy.force t in
  require (phys_equal force1 force2);
  [%expect {| |}]
;;

let%expect_test "peek" =
  let t = Portable_lazy.from_fun (fun () -> 1) in
  let result = Portable_lazy.peek t in
  print_s [%message "" ~peek:(result : int option)];
  print_s [%message "" ~force:(Portable_lazy.force t : int)];
  let result = Portable_lazy.peek t in
  print_s [%message "" ~peek:(result : int option)];
  [%expect
    {|
    (peek ())
    (force 1)
    (peek (1))
    |}]
;;
