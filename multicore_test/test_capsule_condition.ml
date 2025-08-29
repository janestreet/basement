open! Base
open Basement
module Atomic = Stdlib.Atomic

external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"

let cpu_relax = Stdlib_shim.Domain.cpu_relax

let%expect_test "signal" =
  let (P k) = Capsule.create () in
  let mutex = Capsule.Mutex.create k in
  let cond = Capsule.Condition.create () in
  let go = Capsule.Data.create (fun () -> ref true) in
  let wait = Atomic.make true in
  let going = Atomic.make true in
  (match
     Multicore.spawn
       (fun () ->
         Capsule.Mutex.with_key mutex ~f:(fun key ->
           Atomic.Contended.set wait false;
           let rec loop key =
             let res, key =
               Capsule.Key.access key ~f:(fun access -> !(Capsule.Data.unwrap ~access go))
             in
             match res with
             | true ->
               let key = Capsule.Condition.wait cond ~mutex key in
               loop key
             | false -> key
           in
           (), loop key);
         Atomic.Contended.set going false)
       ()
   with
   | Spawned -> ()
   | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt);
  while Atomic.get wait do
    cpu_relax ()
  done;
  Capsule.Mutex.with_lock mutex ~f:(fun password ->
    Capsule.Data.iter go ~password ~f:(fun go -> go := false);
    Capsule.Condition.signal cond);
  while Atomic.get going do
    cpu_relax ()
  done
;;

let%expect_test "broadcast" =
  let (P k) = Capsule.create () in
  let mutex = Capsule.Mutex.create k in
  let cond = Capsule.Condition.create () in
  let go = Capsule.Data.create (fun () -> ref true) in
  let ready = Atomic.make 0 in
  let finished = Atomic.make 0 in
  for _ = 1 to 4 do
    match
      Multicore.spawn
        (fun () ->
          Capsule.Mutex.with_key mutex ~f:(fun key ->
            Atomic.incr ready;
            let rec loop key =
              let res, key =
                Capsule.Key.access key ~f:(fun access ->
                  !(Capsule.Data.unwrap ~access go))
              in
              match res with
              | true ->
                let key = Capsule.Condition.wait cond ~mutex key in
                loop key
              | false -> key
            in
            (), loop key);
          Atomic.incr finished)
        ()
    with
    | Spawned -> ()
    | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt
  done;
  while Atomic.get ready < 4 do
    cpu_relax ()
  done;
  Capsule.Mutex.with_lock mutex ~f:(fun password ->
    Capsule.Data.iter go ~password ~f:(fun go -> go := false);
    Capsule.Condition.broadcast cond);
  while Atomic.get finished < 4 do
    cpu_relax ()
  done
;;

let%expect_test "wait on a poisoned mutex" =
  let (P k) = Capsule.create () in
  let mutex = Capsule.Mutex.create k in
  let cond = Capsule.Condition.create () in
  let key = Capsule.Mutex.destroy mutex in
  try
    let (_ : _ Capsule.Key.t) = Capsule.Condition.wait cond ~mutex key in
    failwith "Expected Mutex.Poisoned exception, but no exception was raised"
  with
  | Capsule.Mutex.Poisoned -> [%expect {| |}]
;;

let%expect_test "mutex poisoned during condition wait" =
  let (P k) = Capsule.create () in
  let mutex = Capsule.Mutex.create k in
  let cond = Capsule.Condition.create () in
  let poisoned = Atomic.make false in
  let ready = Atomic.make false in
  let going = Atomic.make true in
  (match
     Multicore.spawn
       (fun () ->
         (try
            Capsule.Mutex.with_key mutex ~f:(fun key ->
              Atomic.Contended.set ready true;
              (* Raises [poisoned] after waiting. *)
              let key = Capsule.Condition.wait cond ~mutex key in
              Capsule.Key.access key ~f:(fun _ : unit -> failwith "can't reach this"))
          with
          | Capsule.Mutex.Poisoned -> Atomic.Contended.set poisoned true);
         Atomic.Contended.set going false)
       ()
   with
   | Spawned -> ()
   | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt);
  while not (Atomic.get ready) do
    cpu_relax ()
  done;
  (* Poison the mutex while the other domain is waiting *)
  let _key = Capsule.Mutex.destroy mutex in
  Capsule.Condition.signal cond;
  while Atomic.get going do
    cpu_relax ()
  done;
  [%test_result: bool] (Atomic.get poisoned) ~expect:true;
  [%expect {| |}]
;;
