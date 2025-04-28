open! Base
open Stdlib
open Basement
module Data = Capsule.Data

type 'a myref = { mutable v : 'a }

let mk_ref : 'a -> 'a myref = fun v -> { v }
let read_ref : 'a. 'a myref -> 'a = fun r -> r.v
let write_ref : 'a. 'a -> 'a myref -> unit = fun v r -> r.v <- v

type 'a guarded = Mk : 'k Capsule.Rwlock.t * ('a, 'k) Data.Shared.t -> 'a guarded

type ('a, 'b) func =
  { f : 'k. 'k Capsule.Password.Shared.t -> ('a, 'k) Data.Shared.t -> 'b }

let with_guarded (Mk (rw, p)) (f : ('a, 'b) func) =
  Capsule.Rwlock.with_read_lock rw ~f:(fun k -> f.f k p)
;;

exception E

let%expect_test ("shared data API" [@tags "runtime5-only", "no-js", "no-wasm"]) =
  (* Create a reference in Data.Shared *)
  let ptr =
    let (Capsule.Key.P brand) = Capsule.create () in
    let rw = Capsule.Rwlock.create brand in
    Mk (rw, Data.Shared.create (fun () -> mk_ref 42))
  in
  (* Extract a value. *)
  let () =
    with_guarded
      ptr
      { f =
          (fun password p ->
            [%test_result: int] (Data.Shared.extract p ~password ~f:read_ref) ~expect:42)
      }
  in
  (* Create another pointer. *)
  let ptr' =
    let (Mk (rw, _)) = ptr in
    Mk (rw, Data.Shared.create (fun () -> mk_ref 10))
  in
  (* Write via iter, then read via extract. *)
  let () =
    with_guarded
      ptr'
      { f = (fun password p -> Data.Shared.iter p ~password ~f:(write_ref 25)) }
  in
  let () =
    with_guarded
      ptr'
      { f =
          (fun password p ->
            [%test_result: int] (Data.Shared.extract p ~password ~f:read_ref) ~expect:25)
      }
  in
  (* Test [map]. *)
  let _ptr2 =
    let (Mk (rw, p)) = ptr in
    let p' =
      Capsule.Rwlock.with_read_lock rw ~f:(fun password ->
        Data.Shared.map p ~password ~f:(fun _ -> mk_ref 3))
    in
    Mk (rw, p')
  in
  (* Test [both], [fst], [snd]. *)
  let () =
    let (Mk (rw, p)) = ptr' in
    with_guarded
      (Mk (rw, p))
      { f =
          (fun password ptr_left ->
            let ptr_right = Data.Shared.create (fun () -> mk_ref 999) in
            let both_ptr = Data.Shared.both ptr_left ptr_right in
            let left_part = Data.Shared.fst both_ptr in
            let right_part = Data.Shared.snd both_ptr in
            let left_val = Data.Shared.extract left_part ~password ~f:read_ref in
            let right_val = Data.Shared.extract right_part ~password ~f:read_ref in
            [%test_result: int] left_val ~expect:25;
            [%test_result: int] right_val ~expect:999)
      }
  in
  (* Test [inject], [project]. *)
  let () =
    let i = Data.Shared.inject 123 in
    [%test_result: int] (Data.Shared.project i) ~expect:123;
    let both_ = Data.Shared.both i i in
    [%test_result: int] (Data.Shared.project (Data.Shared.fst both_)) ~expect:123;
    [%test_result: int] (Data.Shared.project (Data.Shared.snd both_)) ~expect:123
  in
  (* Test [bind]. *)
  let bind_test =
    let (Capsule.Key.P brand) = Capsule.create () in
    let rw = Capsule.Rwlock.create brand in
    Mk (rw, Data.Shared.inject 7)
  in
  let () =
    with_guarded
      bind_test
      { f =
          (fun password p ->
            let p2 =
              Data.Shared.bind p ~password ~f:(fun x -> Data.Shared.inject (x * 10))
            in
            [%test_result: int] (Data.Shared.project p2) ~expect:70)
      }
  in
  (* Ensure that the Rwlock is frozen on exceptions. *)
  let poison_test =
    let (Capsule.Key.P brand) = Capsule.create () in
    let rw = Capsule.Rwlock.create brand in
    Mk (rw, Data.Shared.inject ())
  in
  let () =
    match with_guarded poison_test { f = (fun _ _ -> raise E) } with
    | exception E -> () (* Original exception *)
    | _ -> assert false
  in
  (* Verify Rwlock is now frozen. *)
  let () =
    let (Mk (rw, _)) = poison_test in
    match Capsule.Rwlock.with_write_lock rw ~f:(fun _k -> ()) with
    | exception Capsule.Rwlock.Frozen -> ()
    | _ -> assert false
  in
  ()
;;
