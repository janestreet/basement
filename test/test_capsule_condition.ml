open! Basement

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
      Capsule.Mutex.with_lock mutex ~f:(fun password ->
        Atomic.set wait false;
        while Capsule.Data.extract go ~password ~f:(fun go : bool -> !go) do
          Capsule.Condition.wait cond ~mutex ~password
        done))
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
    List.init 4 (fun _ ->
      Domain.spawn (fun () ->
        Capsule.Mutex.with_lock mutex ~f:(fun password ->
          Atomic.incr ready;
          while Capsule.Data.extract go ~password ~f:(fun go : bool -> !go) do
            Capsule.Condition.wait cond ~mutex ~password
          done)))
  in
  while Atomic.get ready < 4 do
    ()
  done;
  Capsule.Mutex.with_lock mutex ~f:(fun password ->
    Capsule.Data.iter go ~password ~f:(fun go -> go := false);
    Capsule.Condition.broadcast cond);
  List.iter Domain.join domains
;;
