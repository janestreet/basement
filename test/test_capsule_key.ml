open! Base
open Basement
open Blocking_sync [@@alert "-deprecated"]

external ( = ) : 'a @ local shared -> 'a @ local shared -> bool @@ portable = "%equal"
external raise : exn -> 'a @ portable unique @@ portable = "%reraise"

type nothing = |
type 'a aliased_many = AliasedMany of 'a @@ aliased many [@@unboxed]

let%expect_test "[Capsule.Key] basics" =
  (* [Capsule.create] creates a new capsule with an unique key. *)
  let packed : Capsule.Key.packed @ unique = Capsule.create () in
  let (P k) = packed in
  let #(AliasedMany x, k) =
    Capsule.Key.access k ~f:(fun access ->
      AliasedMany (Capsule.Data.wrap ~access (ref "Hello world!")))
  in
  let #((), _k) =
    Capsule.Key.with_password k ~f:(fun password ->
      Capsule.Data.iter x ~password ~f:(fun s -> assert (!s = "Hello world!")))
  in
  ()
;;

let%expect_test "[Key]s and [Mutex]es" =
  let (P k) = Capsule.create () in
  let #(AliasedMany x, k) =
    Capsule.Key.access k ~f:(fun access ->
      AliasedMany (Capsule.Data.wrap ~access (ref "My value")))
  in
  (* [Mutex.create] takes the key. *)
  let m = Mutex.create k in
  let () =
    Mutex.with_lock m ~f:(fun password ->
      Capsule.Data.iter x ~password ~f:(fun s -> s := "Another value"))
  in
  (* [Mutex.destroy] returns the key. *)
  let k = Mutex.destroy m in
  (* [Capsule.Key.destroy] merges the capsule into the current one. *)
  let access = Capsule.Key.destroy k in
  [%test_result: string] !(Capsule.Data.unwrap ~access x) ~expect:"Another value"
;;

let%expect_test "Exceptions are re-raised and destroy the key" =
  let exception E of string in
  let (P k) = Capsule.create () in
  match Capsule.Key.access k ~f:(fun _ -> raise (E "Exception!")) with
  | #((_ : nothing), _k) -> .
  | exception E s -> [%test_result: string] s ~expect:"Exception!"
;;

let%expect_test "Encapsulated exceptions from inside [with_password] are unwrapped" =
  let exception F of int in
  let (P k) = Capsule.create () in
  match
    Capsule.Key.with_password k ~f:(fun password : nothing ->
      Capsule.access ~password ~f:(fun _ -> raise (F 1)))
  with
  | #((_ : nothing), _k) -> .
  | exception F n -> [%test_result: int] n ~expect:1
;;

let%expect_test "[with_password_local] destroys the key but returns a local value" =
  let (P k) = Capsule.create () in
  let x, password =
    Capsule.Key.with_password_local k ~f:(fun password -> exclave_
      ( (Capsule.access_local ~password ~f:(fun access ->
           { aliased = Capsule.Data.wrap ~access (ref "Local value") }))
          .aliased
      , Capsule.Password.box password ))
  in
  (* We can still access the data through the password. *)
  let { aliased = s } =
    Capsule.Data.Local.extract x ~password:(Capsule.Password.unbox password) ~f:(fun x ->
      { aliased = (x.contents : string @ portable) })
  in
  assert (s = "Local value")
;;

let%expect_test "Exceptions are re-raised and destroy the key when using \
                 [with_password_local]"
  =
  let exception E of string in
  let (P k) = Capsule.create () in
  match Capsule.Key.with_password_local k ~f:(fun _ -> raise (E "Exception!")) with
  | (_ : nothing) -> .
  | exception E s -> [%test_result: string] s ~expect:"Exception!"
;;

let%expect_test "Encapsulated exceptions from inside with_password_local are unwrapped" =
  let exception F of int in
  let (P k) = Capsule.create () in
  match
    Capsule.Key.with_password_local
      k
      ~f:(fun (type k) (password : k Capsule.Password.t) : nothing ->
        Capsule.access_local ~password ~f:(fun _ -> raise (F 2)))
  with
  | (_ : nothing) -> .
  | exception F n -> [%test_result: int] n ~expect:2
;;

let%expect_test "Read-only access to the capsule" =
  let (P k) @ aliased = Capsule.create () in
  let x = Capsule.Data.create (fun () -> "Shared value") in
  (* Requires the type to be portable. *)
  Capsule.Key.with_password_shared k ~f:(fun password ->
    Capsule.Data.extract_shared x ~password ~f:(fun s -> assert (s = "Shared value")))
;;

let%expect_test "Exceptions with access_shared are re-raised as is" =
  let exception E of string in
  let (P k) = Capsule.create () in
  match
    Capsule.Key.access_shared k ~f:(fun _ -> raise (E "Exception with access_shared"))
  with
  | (_ : nothing) -> .
  | exception E s -> [%test_result: string] s ~expect:"Exception with access_shared"
;;

let%expect_test "Exceptions with with_password_shared are re-raised as is" =
  let exception E of string in
  let (P k) = Capsule.create () in
  match
    Capsule.Key.with_password_shared k ~f:(fun _ ->
      raise (E "Exception with with_password_shared"))
  with
  | (_ : nothing) -> .
  | exception E s ->
    [%test_result: string] s ~expect:"Exception with with_password_shared"
;;

let%expect_test "Exceptions with with_password_shared_local are re-raised as is" =
  let exception E of string in
  let (P k) = Capsule.create () in
  match
    Capsule.Key.with_password_shared_local k ~f:(fun _ ->
      raise (E "Exception with with_password_shared_local"))
  with
  | (_ : nothing) -> .
  | exception E s ->
    [%test_result: string] s ~expect:"Exception with with_password_shared_local"
;;

let%expect_test "Encapsulated exceptions from inside [with_password_shared] are unwrapped"
  =
  let exception F of int in
  let (P k) = Capsule.create () in
  match
    Capsule.Key.with_password_shared
      k
      ~f:(fun (type k) (password : k Capsule.Password.Shared.t) : nothing ->
        Capsule.access_shared ~password ~f:(fun _ -> raise (F 2)))
  with
  | (_ : nothing) -> .
  | exception F n -> [%test_result: int] n ~expect:2
;;

let%expect_test "Encapsulated exceptions from inside [with_password_shared_local] are \
                 unwrapped"
  =
  let exception F of int in
  let (P k) = Capsule.create () in
  match
    Capsule.Key.with_password_shared_local
      k
      ~f:(fun (type k) (password : k Capsule.Password.Shared.t) : nothing ->
        Capsule.access_shared_local ~password ~f:(fun _ -> raise (F 2)))
  with
  | (_ : nothing) -> .
  | exception F n -> [%test_result: int] n ~expect:2
;;

let%expect_test "[Mutex.with_key] provides direct access to the key" =
  let (P k) = Capsule.create () in
  let #(AliasedMany x, k) =
    Capsule.Key.access k ~f:(fun access ->
      AliasedMany (Capsule.Data.wrap ~access (ref "Initial value")))
  in
  let m = Mutex.create k in
  let result =
    Mutex.with_key m ~f:(fun key ->
      let #((), key) =
        Capsule.Key.with_password key ~f:(fun password ->
          Capsule.Data.iter x ~password ~f:(fun s -> s := "Modified with key"))
      in
      #("Success", key))
  in
  [%test_result: string] result ~expect:"Success";
  let k = Mutex.destroy m in
  let access = Capsule.Key.destroy k in
  [%test_result: string] !(Capsule.Data.unwrap ~access x) ~expect:"Modified with key"
;;

let%expect_test "Exceptions in [Mutex.with_key] are re-raised and poison the mutex" =
  let exception Key_exception of string in
  let (P k) = Capsule.create () in
  let m = Mutex.create k in
  match
    Mutex.with_key m ~f:(fun _key ->
      match raise (Key_exception "Exception from with_key!") with
      | (_ : nothing) -> .)
  with
  | _result -> assert false
  | exception Key_exception s ->
    [%test_result: string] s ~expect:"Exception from with_key!";
    (match Mutex.with_key m ~f:(fun k -> #("No exception", k)) with
     | _result -> assert false
     | exception Mutex.Poisoned -> ())
;;
