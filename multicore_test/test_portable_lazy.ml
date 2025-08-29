open! Base
open Expect_test_helpers_base
open Portable_test_helpers
open Basement

let%expect_test "if multiple threads force at the same time, the lazy still always \
                 returns the same value"
  =
  let t = Portable_lazy.from_fun (fun () -> Portable_atomic.make 0) in
  let nthreads = 32 in
  let barrier = Barrier.create nthreads in
  let count = Stdlib.Atomic.make 0 in
  for _ = 1 to nthreads do
    match
      Multicore.spawn
        (fun () ->
          Barrier.await barrier;
          Portable_atomic.incr (Portable_lazy.force t);
          Stdlib.Atomic.incr count)
        ()
    with
    | Spawned -> ()
    | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt
  done;
  while Stdlib.Atomic.get count < nthreads do
    Stdlib_shim.Domain.cpu_relax ()
  done;
  require_equal (module Int) nthreads (Portable_atomic.get (Portable_lazy.force t))
;;

let%expect_test "if multiple threads force at the same time, the thunk is only called \
                 once"
  =
  let times_force_called = Portable_atomic.make 0 in
  let t = Portable_lazy.from_fun (fun () -> Portable_atomic.incr times_force_called) in
  let nthreads = 32 in
  let barrier = Barrier.create nthreads in
  let count = Stdlib.Atomic.make 0 in
  for _ = 1 to nthreads do
    match
      Multicore.spawn
        (fun () ->
          Barrier.await barrier;
          Portable_lazy.force t;
          Stdlib.Atomic.incr count)
        ()
    with
    | Spawned -> ()
    | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt
  done;
  while Stdlib.Atomic.get count < nthreads do
    Stdlib_shim.Domain.cpu_relax ()
  done;
  require_equal (module Int) 1 (Portable_atomic.get times_force_called)
;;
