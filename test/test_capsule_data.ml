open! Base
open Basement

external reraise : exn -> 'a = "%reraise"

type 'a myref = { mutable v : 'a }

let mk_ref : 'a -> 'a myref = fun v -> { v }

(* We need ['a] to be [portable] to return a [portable] value from the read.
   The return value is marked as [contended] because our callsites require that,
   but the typechecker does not implicitly downcast the result. *)
let read_ref : 'a. 'a myref -> 'a aliased = fun r -> { aliased = r.v }

(* We need ['a] to be [portable] and [uncontended] to capture it in
   a [portable] closure like this.*)
let write_ref : 'a. 'a -> 'a myref -> unit = fun v r -> r.v <- v

type 'a guarded = Mk : 'k Capsule.Mutex.t * ('a, 'k) Capsule.Data.t -> 'a guarded
type ('a, 'b) func = { f : 'k. 'k Capsule.Password.t -> ('a, 'k) Capsule.Data.t -> 'b }

let with_guarded x (f : ('a, 'b) func) =
  let (Mk (m, p)) = x in
  (Capsule.Mutex.with_lock m ~f:(fun k -> { aliased = f.f k p })).aliased
;;

let ptr =
  let (P k) = Capsule.create () in
  let m = Capsule.Mutex.create k in
  Mk (m, Capsule.Data.create (fun () -> mk_ref 42))
;;

let%expect_test "[Data.create] and [extract]" =
  with_guarded
    ptr
    { f =
        (fun password p ->
          [%test_result: int]
            (Capsule.Data.extract p ~password ~f:read_ref).aliased
            ~expect:42)
    }
;;

let ptr' =
  let (Mk (m, _p)) = ptr in
  Mk (m, Capsule.Data.create (fun () -> mk_ref 2))
;;

let _ptr'' =
  let (Mk (m, p)) = ptr in
  let p' =
    (Capsule.Mutex.with_lock m ~f:(fun password ->
       Capsule.access ~password ~f:(fun access ->
         let x = Capsule.Data.unwrap ~access p in
         x.v <- 45;
         { many = { aliased = Capsule.Data.wrap ~access x } })))
      .many
      .aliased
  in
  Mk (m, p')
;;

let%expect_test "[wrap], [unwrap], [iter]" =
  with_guarded
    ptr
    { f = (fun password p -> Capsule.Data.iter ~password ~f:(write_ref 15) p) }
;;

let%expect_test "Modified value is accessible afterward with [extract]" =
  with_guarded
    ptr
    { f =
        (fun password p ->
          [%test_result: int]
            (Capsule.Data.extract p ~password ~f:read_ref).aliased
            ~expect:15)
    }
;;

let%expect_test "Other capsules unaffected by modifications" =
  with_guarded
    ptr'
    { f =
        (fun password p ->
          [%test_result: int]
            (Capsule.Data.extract p ~password ~f:read_ref).aliased
            ~expect:2)
    }
;;

exception Leak of int myref

let%expect_test "Raised exceptions are marked as contended" =
  with_guarded
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
;;

let%expect_test "Raised exceptions are marked as contended" =
  with_guarded
    ptr
    { f =
        (fun (type k) (password : k Capsule.Password.t) (p : _ Capsule.Data.t) ->
          match
            Capsule.access ~password ~f:(fun access ->
              reraise (Leak (Capsule.Data.unwrap ~access p)))
          with
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
;;

let ptr2 =
  let (Mk (m, p)) = ptr in
  let p' =
    (Capsule.Mutex.with_lock m ~f:(fun password ->
       { many = { aliased = Capsule.Data.map ~password ~f:(fun _ -> mk_ref 3) p } }))
      .many
      .aliased
  in
  Mk (m, Capsule.Data.both p p')
;;

let%expect_test "[map], [both], [fst], [snd]" =
  let a = 1 in
  let b = 2 in
  let tup = Capsule.Data.create (fun () -> a, b) in
  let a_cap = Capsule.Data.fst tup in
  let b_cap = Capsule.Data.snd tup in
  [%test_result: int] (Capsule.Data.project a_cap) ~expect:a;
  [%test_result: int] (Capsule.Data.project b_cap) ~expect:b
;;

let%expect_test "[Capsule.Key.destroy]" =
  let (Mk (m, p)) = ptr2 in
  let access = Capsule.Key.destroy (Capsule.Mutex.destroy m) in
  let r1, r2 = Capsule.Data.unwrap ~access p in
  [%test_result: int] (read_ref r1).aliased ~expect:15;
  [%test_result: int] (read_ref r2).aliased ~expect:3
;;

let%expect_test "[Mutex] operations on poisoned mutex raise [Poisoned]" =
  match with_guarded ptr { f = (fun _ _ -> ()) } with
  | exception Capsule.Mutex.Poisoned -> ()
  | _ -> assert false
;;

let%expect_test "[Mutex] operations on poisoned mutex raise [Poisoned]" =
  match with_guarded ptr' { f = (fun _ _ -> ()) } with
  | exception Capsule.Mutex.Poisoned -> ()
  | _ -> assert false
;;

let%expect_test "[Mutex] operations on poisoned mutex raise [Poisoned]" =
  match with_guarded ptr2 { f = (fun _ _ -> ()) } with
  | exception Capsule.Mutex.Poisoned -> ()
  | _ -> assert false
;;

let%expect_test "[Data.inject] and [Data.project]" =
  let ptr = Capsule.Data.inject 100 in
  [%test_result: int] (Capsule.Data.project ptr) ~expect:100
;;

external ( + ) : int -> int -> int = "%addint"

type lost_capsule = |

let ptr' : (int, lost_capsule) Capsule.Data.t =
  let (P k) = Capsule.create () in
  let m = Capsule.Mutex.create k in
  let ptr = Capsule.Data.inject 100 in
  (Capsule.Mutex.with_lock m ~f:(fun password ->
     { many =
         { aliased =
             Capsule.Data.bind ptr ~password ~f:(fun x ->
               Capsule.Data.inject ((( + ) x) 11))
         }
     }))
    .many
    .aliased
;;

let%expect_test "[Data.bind]" =
  [%test_result: int] (Capsule.Data.project ptr') ~expect:111
;;
