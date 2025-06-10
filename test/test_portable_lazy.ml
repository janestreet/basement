open! Base
open Expect_test_helpers_base
open Portable_test_helpers
open Basement

(* Disable alert on [Domain.spawn] since we are explicitly testing its behavior. *)
[@@@alert "-unsafe_parallelism"]

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

let%expect_test ("if multiple domains force at the same time, the lazy still always \
                  returns the same value" [@tags "runtime5-only", "no-js", "no-wasm"])
  =
  let t = Portable_lazy.from_fun (fun () -> Portable_atomic.make 0) in
  let ndomains = 32 in
  let barrier = Barrier.create ndomains in
  let domains =
    List.init ndomains ~f:(fun _ ->
      Domain.Safe.spawn (fun () ->
        Barrier.await barrier;
        Portable_atomic.incr (Portable_lazy.force t)))
  in
  List.iter domains ~f:Domain.join;
  require_equal (module Int) ndomains (Portable_atomic.get (Portable_lazy.force t))
;;

let%expect_test ("if multiple domains force at the same time, the thunk is only called \
                  once" [@tags "runtime5-only", "no-js", "no-wasm"])
  =
  let times_force_called = Portable_atomic.make 0 in
  let t = Portable_lazy.from_fun (fun () -> Portable_atomic.incr times_force_called) in
  let ndomains = 32 in
  let barrier = Barrier.create ndomains in
  let domains =
    List.init ndomains ~f:(fun _ ->
      Domain.Safe.spawn (fun () ->
        Barrier.await barrier;
        Portable_lazy.force t))
  in
  List.iter domains ~f:Domain.join;
  require_equal (module Int) 1 (Portable_atomic.get times_force_called)
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
