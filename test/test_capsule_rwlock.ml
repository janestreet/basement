open! Base
open Stdlib
open Basement

(* [Rwlock.t] and [Data.t] are [value mod portable contended]. *)

type 'k _rwlock = 'k Capsule.Rwlock.t
type ('a, 'k) _data = ('a, 'k) Capsule.Data.t

(* Packed rwlocks are [value mod portable contended]. *)

type _packed = Capsule.Rwlock.packed

type 'a myref = { mutable v : 'a }
[@@unsafe_allow_any_mode_crossing "TODO: This can go away once we have with-kinds"]

module RwCell = struct
  type 'a t = Mk : 'k Capsule.Rwlock.t * ('a myref, 'k) Capsule.Data.t -> 'a t

  let create (type a) (x : a) : a t =
    let (P k) = Capsule.create () in
    let m = Capsule.Rwlock.create k in
    let p = Capsule.Data.create (fun () -> { v = x }) in
    Mk (m, p)
  ;;

  let read (type a) (t : a t) : a =
    let (Mk (m, p)) = t in
    Capsule.Rwlock.with_read_lock m ~f:(fun password ->
      let read' : a myref -> a = fun r -> r.v in
      Capsule.Data.extract_shared p ~password ~f:read')
  ;;

  let write (type a) (t : a t) (x : a) =
    let (Mk (m, p)) = t in
    Capsule.Rwlock.with_write_lock m ~f:(fun password ->
      Capsule.Data.iter p ~password ~f:(fun r -> r.v <- x))
  ;;

  let copy (type a) (t : a t) : a t =
    let (Mk (m, p)) = t in
    Capsule.Rwlock.with_read_lock m ~f:(fun password ->
      let p =
        Capsule.Data.map_shared p ~password ~f:(fun r ->
          let v : a = r.v in
          { v })
      in
      Mk (m, p))
  ;;
end

let%expect_test "[RwCell] basics" =
  let ptr = RwCell.create 42 in
  let ptr' = RwCell.copy ptr in
  [%test_result: int] (RwCell.read ptr) ~expect:42;
  RwCell.write ptr 43;
  [%test_result: int] (RwCell.read ptr) ~expect:43;
  [%test_result: int] (RwCell.read ptr') ~expect:42
;;

(** Testing individual capsule operations over a password captured by a rwlock *)

external ( + ) : int -> int -> int = "%addint"
external reraise : exn -> 'a = "%reraise"

type 'a guarded = Mk : 'k Capsule.Rwlock.t * ('a, 'k) Capsule.Data.t -> 'a guarded
type ('a, 'b) func = { f : 'k. 'k Capsule.Password.t -> ('a, 'k) Capsule.Data.t -> 'b }

let with_write_guarded x (f : ('a, 'b) func) =
  let (Mk (m, p)) = x in
  Capsule.Rwlock.with_write_lock m ~f:(fun k -> f.f k p)
;;

type ('a, 'b) func' =
  { f : 'k. 'k Capsule.Password.Shared.t -> ('a, 'k) Capsule.Data.t -> 'b }

let with_read_guarded x (f : ('a, 'b) func') =
  let (Mk (m, p)) = x in
  Capsule.Rwlock.with_read_lock m ~f:(fun k -> f.f k p)
;;

(* reading from myref with the expected modes *)
let read_ref : 'a. 'a myref -> 'a = fun r -> r.v

(* writing to myref with the expected modes *)
let write_ref : 'a. 'a -> 'a myref -> unit = fun v r -> r.v <- v

exception Leak of int myref
exception ReadLockTestException of int myref

type lost_capsule = |

let%expect_test "rwlock API" =
  (* [create]. *)
  let ptr =
    let (P k) = Capsule.create () in
    let m = Capsule.Rwlock.create k in
    Mk (m, Capsule.Data.create (fun () -> { v = 42 }))
  in
  (* [extract]. *)
  let () =
    with_write_guarded
      ptr
      { f =
          (fun password p ->
            [%test_result: int] (Capsule.Data.extract p ~password ~f:read_ref) ~expect:42)
      }
  in
  let ptr' =
    let (Mk (m, _p)) = ptr in
    Mk (m, Capsule.Data.create (fun () -> { v = 2 }))
  in
  (* [iter]. *)
  let () =
    with_write_guarded
      ptr
      { f = (fun password p -> Capsule.Data.iter p ~password ~f:(write_ref 15)) }
  in
  let () =
    with_write_guarded
      ptr
      { f =
          (fun password p ->
            [%test_result: int] (Capsule.Data.extract p ~password ~f:read_ref) ~expect:15)
      }
  in
  let () =
    with_write_guarded
      ptr'
      { f =
          (fun password p ->
            [%test_result: int] (Capsule.Data.extract p ~password ~f:read_ref) ~expect:2)
      }
  in
  (* [extract_shared]. *)
  let () =
    with_read_guarded
      ptr
      { f =
          (fun password p ->
            [%test_result: int]
              (Capsule.Data.extract_shared p ~password ~f:read_ref)
              ~expect:15)
      }
  in
  (* [map_shared]. *)
  let ptr2 =
    let (Mk (m, p)) = ptr in
    let p' =
      Capsule.Rwlock.with_read_lock m ~f:(fun password ->
        Capsule.Data.map_shared p ~password ~f:(fun r ->
          let v : int = r.v in
          { v = v + 2 }))
    in
    Mk (m, p')
  in
  (* [map_shared] and [extract_shared]. *)
  let () =
    with_read_guarded
      ptr2
      { f =
          (fun password p ->
            let ptr' =
              Capsule.Data.map_shared p ~password ~f:(fun r -> { v = r.v + 3 })
            in
            [%test_result: int]
              (Capsule.Data.extract_shared ptr' ~password ~f:read_ref)
              ~expect:20)
      }
  in
  (* Using a Password.t as a Password.Shared.t *)
  let () =
    with_write_guarded
      ptr
      { f =
          (fun password p ->
            [%test_result: int]
              (Capsule.Data.extract_shared
                 p
                 ~password:(Capsule.Password.shared password)
                 ~f:read_ref)
              ~expect:15)
      }
  in
  (* [access_shared] and [unwrap_shared]. *)
  let () =
    with_read_guarded
      ptr2
      { f =
          (fun password p ->
            Capsule.access_shared ~password ~f:(fun access ->
              let r = Capsule.Data.unwrap_shared ~access p in
              let ptr' = Capsule.Data.Shared.wrap ~access { v = r.v + 3 } in
              let res = Capsule.Data.Shared.extract ptr' ~password ~f:read_ref in
              assert (res = 20))
            [@nontail])
      }
  in
  (* An exception raised from [iter] is marked as [contended]: *)
  let () =
    with_write_guarded
      ptr
      { f =
          (fun (type k) (password : k Capsule.Password.t) p ->
            match Capsule.Data.iter p ~password ~f:(fun r -> reraise (Leak r)) with
            | exception Capsule.Encapsulated (id, exn_data) ->
              (match
                 Capsule.Password.Id.equality_witness id (Capsule.Password.id password)
               with
               | Some Equal ->
                 Capsule.Data.iter exn_data ~password ~f:(function
                   | Leak _r -> ()
                   | _ -> assert false)
               | None -> assert false)
            | _ -> assert false)
      }
  in
  (* [map], [both]. *)
  let ptr2 =
    let (Mk (m, p)) = ptr in
    let p' =
      Capsule.Rwlock.with_write_lock m ~f:(fun password ->
        Capsule.Data.map p ~password ~f:(fun _ -> { v = 3 }))
    in
    Mk (m, Capsule.Data.both p p')
  in
  (* [destroy]. *)
  let () =
    let (Mk (m, p)) = ptr2 in
    let access = Capsule.Key.destroy (Capsule.Rwlock.destroy m) in
    let r1, r2 = Capsule.Data.unwrap ~access p in
    [%test_result: int] (read_ref r1) ~expect:15;
    [%test_result: int] (read_ref r2) ~expect:3
  in
  let () =
    match with_write_guarded ptr { f = (fun _ _ -> ()) } with
    | exception Capsule.Rwlock.Poisoned -> ()
    | _ -> assert false
  in
  let () =
    match with_write_guarded ptr' { f = (fun _ _ -> ()) } with
    | exception Capsule.Rwlock.Poisoned -> ()
    | _ -> assert false
  in
  let () =
    match with_write_guarded ptr2 { f = (fun _ _ -> ()) } with
    | exception Capsule.Rwlock.Poisoned -> ()
    | _ -> assert false
  in
  (* [inject], [project]. *)
  let () =
    let ptr = Capsule.Data.inject 100 in
    [%test_result: int] (Capsule.Data.project ptr) ~expect:100
  in
  (* [bind]. *)
  let ptr' : (int, lost_capsule) Capsule.Data.t =
    let (P k) = Capsule.create () in
    let m = Capsule.Rwlock.create k in
    let ptr = Capsule.Data.inject 100 in
    Capsule.Rwlock.with_write_lock m ~f:(fun password ->
      Capsule.Data.bind ptr ~password ~f:(fun x -> Capsule.Data.inject ((( + ) x) 11)))
  in
  let () = [%test_result: int] (Capsule.Data.project ptr') ~expect:111 in
  (* [freeze]. *)
  let () =
    let (P k) = Capsule.create () in
    let m = Capsule.Rwlock.create k in
    let data = Capsule.Data.create (fun () -> { v = 42 }) in
    with_write_guarded
      (Mk (m, data))
      { f = (fun password p -> Capsule.Data.iter p ~password ~f:(fun r -> r.v <- 999)) };
    let _freeze_key = Capsule.Rwlock.freeze m in
    with_read_guarded
      (Mk (m, data))
      { f =
          (fun password p ->
            [%test_result: int]
              (Capsule.Data.extract_shared p ~password ~f:(fun r -> r.v))
              ~expect:999)
      };
    match
      with_write_guarded
        (Mk (m, data))
        { f = (fun password p -> Capsule.Data.iter p ~password ~f:(fun r -> r.v <- 123)) }
    with
    | exception Capsule.Rwlock.Frozen -> ()
    | _ -> assert false
  in
  (* Exceptions raised from [with_read_lock]. *)
  let () =
    let ptr =
      let (P k) = Capsule.create () in
      let m = Capsule.Rwlock.create k in
      Mk (m, Capsule.Data.create (fun () -> { v = 999 }))
    in
    with_read_guarded
      ptr
      { f =
          (fun (type k) (password : k Capsule.Password.Shared.t) p ->
            match
              Capsule.Data.extract_shared p ~password ~f:(fun r ->
                reraise (ReadLockTestException r))
            with
            | exception Capsule.Encapsulated_shared (id, exn_data) ->
              (match
                 Capsule.Password.Id.equality_witness
                   id
                   (Capsule.Password.Shared.id password)
               with
               | Some Equal ->
                 Capsule.Data.Shared.iter exn_data ~password ~f:(function
                   | ReadLockTestException { v = 999 } -> ()
                   | _ -> assert false)
               | None -> assert false)
            | _ -> assert false)
      }
  in
  ()
;;
