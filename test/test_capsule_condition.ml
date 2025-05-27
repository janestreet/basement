open! Base
open Basement

module Domain = struct
  include Domain
  include Stdlib_shim_upstream.Domain.Safe
end

[@@@ocaml.alert "-unsafe_parallelism"]
[@@@ocaml.alert "-unsafe_multidomain"]
[@@@ocaml.alert "-unsafe"]

external ref : 'a -> 'a ref @@ portable = "%makemutable"
external ( ! ) : 'a ref -> 'a @@ portable = "%field0"
external ( := ) : 'a ref -> 'a -> unit @@ portable = "%setfield0"

let%expect_test ("signal" [@tags "runtime5-only", "no-js", "no-wasm"]) =
  let (P k) = Capsule.create () in
  let mutex = Capsule.Mutex.create k in
  let cond = Capsule.Condition.create () in
  let go = Capsule.Data.create (fun () -> ref true) in
  let wait = Atomic.make true in
  let domain =
    Domain.spawn (fun () ->
      Capsule.Mutex.with_key mutex ~f:(fun key ->
        Atomic.set wait false;
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
        (), loop key))
  in
  while Atomic.get wait do
    ()
  done;
  Capsule.Mutex.with_lock mutex ~f:(fun password ->
    Capsule.Data.iter go ~password ~f:(fun go -> go := false);
    Capsule.Condition.signal cond);
  Domain.join domain
;;

let%expect_test ("broadcast" [@tags "runtime5-only", "no-js", "no-wasm"]) =
  let (P k) = Capsule.create () in
  let mutex = Capsule.Mutex.create k in
  let cond = Capsule.Condition.create () in
  let go = Capsule.Data.create (fun () -> ref true) in
  let ready = Atomic.make 0 in
  let domains =
    List.init 4 ~f:(fun _ ->
      Domain.spawn (fun () ->
        Capsule.Mutex.with_key mutex ~f:(fun key ->
          Atomic.incr ready;
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
          (), loop key)))
  in
  while Atomic.get ready < 4 do
    ()
  done;
  Capsule.Mutex.with_lock mutex ~f:(fun password ->
    Capsule.Data.iter go ~password ~f:(fun go -> go := false);
    Capsule.Condition.broadcast cond);
  List.iter ~f:Domain.join domains
;;

let%expect_test ("wait on a poisoned mutex" [@tags "runtime5-only", "no-js", "no-wasm"]) =
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

let%expect_test ("mutex poisoned during condition wait"
  [@tags "runtime5-only", "no-js", "no-wasm"])
  =
  let (P k) = Capsule.create () in
  let mutex = Capsule.Mutex.create k in
  let cond = Capsule.Condition.create () in
  let poisoned = Atomic.make false in
  let ready = Atomic.make false in
  let domain =
    Domain.spawn (fun () ->
      try
        Capsule.Mutex.with_key mutex ~f:(fun key ->
          Atomic.set ready true;
          (* Raises [poisoned] after waiting. *)
          let key = Capsule.Condition.wait cond ~mutex key in
          Capsule.Key.access key ~f:(fun _ : unit -> failwith "can't reach this"))
      with
      | Capsule.Mutex.Poisoned -> Atomic.set poisoned true)
  in
  while not (Atomic.get ready) do
    ()
  done;
  (* Poison the mutex while the other domain is waiting *)
  let _key = Capsule.Mutex.destroy mutex in
  Capsule.Condition.signal cond;
  Domain.join domain;
  [%test_result: bool] (Atomic.get poisoned) ~expect:true;
  [%expect {| |}]
;;
