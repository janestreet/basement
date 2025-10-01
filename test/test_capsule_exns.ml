open Basement
open Blocking_sync [@@alert "-deprecated"]
open Expect_test_helpers_base

let show_backtrace = false

let%expect_test "[with_password] unencapsulated" =
  require_does_raise ~show_backtrace (fun () ->
    let (P key) = Capsule.create () in
    let (_ : _) =
      Capsule.Key.with_password key ~f:(fun _ ->
        let (_ : _) = failwith "fail" in
        ())
    in
    ());
  [%expect {| (Failure fail) |}]
;;

let%expect_test "[with_password] nested" =
  require_does_raise ~show_backtrace (fun () ->
    let (P key) = Capsule.create () in
    let (_ : _) =
      Capsule.Key.with_password key ~f:(fun password ->
        let d = Capsule.Data.create (fun () -> ()) in
        let (_ : _) = Capsule.Data.map d ~password ~f:(fun _ -> failwith "fail") in
        ())
    in
    ());
  [%expect {| (Failure fail) |}]
;;

let%expect_test "[with_password] shared" =
  require_does_raise ~show_backtrace (fun () ->
    let (P key) = Capsule.create () in
    let (_ : _) =
      Capsule.Key.with_password key ~f:(fun password ->
        let d = Capsule.Data.create (fun () -> ()) in
        let password = Capsule.Password.shared password in
        let (_ : _) = Capsule.Data.map_shared d ~password ~f:(fun _ -> failwith "fail") in
        ())
    in
    ());
  [%expect {| (Failure fail) |}]
;;

let%expect_test "[with_password] other capsule" =
  let (P key) = Capsule.create () in
  let (_ : _) =
    Capsule.Key.with_password key ~f:(fun password ->
      require_does_raise ~show_backtrace (fun () ->
        let (P key) = Capsule.create () in
        let (_ : _) =
          Capsule.Key.with_password key ~f:(fun _ ->
            let d = Capsule.Data.create (fun () -> ()) in
            let (_ : _) = Capsule.Data.map d ~password ~f:(fun _ -> failwith "fail") in
            ())
        in
        ())
      [@nontail])
  in
  ();
  [%expect {| (Failure fail) |}]
;;

let%expect_test "[with_password_shared] unencapsulated" =
  require_does_raise ~show_backtrace (fun () ->
    let (P key) = Capsule.create () in
    let (_ : _) =
      Capsule.Key.with_password_shared key ~f:(fun _ ->
        let (_ : _) = failwith "fail" in
        ())
    in
    ());
  [%expect {| (Failure fail) |}]
;;

let%expect_test "[with_password_shared] nested shared" =
  require_does_raise ~show_backtrace (fun () ->
    let (P key) = Capsule.create () in
    let (_ : _) =
      Capsule.Key.with_password_shared key ~f:(fun password ->
        let d = Capsule.Data.create (fun () -> ()) in
        let (_ : _) = Capsule.Data.map_shared d ~password ~f:(fun _ -> failwith "fail") in
        ())
    in
    ());
  [%expect {| (Failure fail) |}]
;;

let%expect_test "[with_password_shared] other capsule" =
  let (P key) = Capsule.create () in
  let (_ : _) =
    Capsule.Key.with_password_shared key ~f:(fun password ->
      require_does_raise ~show_backtrace (fun () ->
        let (P key) = Capsule.create () in
        let (_ : _) =
          Capsule.Key.with_password_shared key ~f:(fun _ ->
            let d = Capsule.Data.create (fun () -> ()) in
            let (_ : _) =
              Capsule.Data.map_shared d ~password ~f:(fun _ -> failwith "fail")
            in
            ())
        in
        ())
      [@nontail])
  in
  ();
  [%expect {| (Failure fail) |}]
;;

let%expect_test "[with_lock] unencapsulated" =
  require_does_raise ~show_backtrace (fun () ->
    let (P key) = Capsule.create () in
    let mut = Mutex.create key in
    Mutex.with_lock mut ~f:(fun _ ->
      let (_ : _) = failwith "fail" in
      ()));
  [%expect {| (Failure fail) |}]
;;

let%expect_test "[with_lock] encapsulated" =
  require_does_raise ~show_backtrace (fun () ->
    let (P key) = Capsule.create () in
    let mut = Mutex.create key in
    Mutex.with_lock mut ~f:(fun password ->
      let d = Capsule.Data.create (fun () -> ()) in
      let (_ : _) = Capsule.Data.map d ~password ~f:(fun _ -> failwith "fail") in
      ()));
  [%expect {| (Failure fail) |}]
;;

let%expect_test "[with_lock] encapsulated shared" =
  require_does_raise ~show_backtrace (fun () ->
    let (P key) = Capsule.create () in
    let mut = Mutex.create key in
    Mutex.with_lock mut ~f:(fun password ->
      let d = Capsule.Data.create (fun () -> ()) in
      let password = Capsule.Password.shared password in
      let (_ : _) = Capsule.Data.map_shared d ~password ~f:(fun _ -> failwith "fail") in
      ()));
  [%expect {| (Failure fail) |}]
;;

let%expect_test "[with_lock] other capsule" =
  let (P key) = Capsule.create () in
  let (_ : _) =
    Capsule.Key.with_password key ~f:(fun password ->
      require_does_raise ~show_backtrace (fun () ->
        let (P key) = Capsule.create () in
        let mut = Mutex.create key in
        Mutex.with_lock mut ~f:(fun _ ->
          let d = Capsule.Data.create (fun () -> ()) in
          let (_ : _) = Capsule.Data.map d ~password ~f:(fun _ -> failwith "fail") in
          ())
        [@nontail])
      [@nontail])
  in
  ();
  [%expect {| (Failure fail) |}]
;;
