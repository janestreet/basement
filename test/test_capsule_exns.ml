open Basement

let show_backtrace = false

let%expect_test "[with_password] unencapsulated" =
  Expect_test_helpers_base.require_does_raise ~show_backtrace (fun () ->
    let (P key) = Capsule.create () in
    let (_ : _) =
      Capsule.Key.with_password key ~f:(fun _ ->
        let (_ : _) = failwith "fail" in
        ())
    in
    ());
  [%expect {| (Failure fail) |}]
;;

let%expect_test "[with_password] encapsulated" =
  Expect_test_helpers_base.require_does_raise ~show_backtrace (fun () ->
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

let%expect_test "[with_password] encapsulated shared" =
  Expect_test_helpers_base.require_does_raise ~show_backtrace (fun () ->
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
      Expect_test_helpers_base.require_does_raise ~show_backtrace (fun () ->
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
  [%expect {| ("Capsule.Encapsulated(_)") |}]
;;

let%expect_test "[with_password_shared] unencapsulated" =
  Expect_test_helpers_base.require_does_raise ~show_backtrace (fun () ->
    let (P key) = Capsule.create () in
    let (_ : _) =
      Capsule.Key.with_password_shared key ~f:(fun _ ->
        let (_ : _) = failwith "fail" in
        ())
    in
    ());
  [%expect {| (Failure fail) |}]
;;

let%expect_test "[with_password_shared] encapsulated shared" =
  Expect_test_helpers_base.require_does_raise ~show_backtrace (fun () ->
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
      Expect_test_helpers_base.require_does_raise ~show_backtrace (fun () ->
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
  [%expect {| ("Capsule.Encapsulated_shared(_)") |}]
;;

let%expect_test "[with_lock] unencapsulated" =
  Expect_test_helpers_base.require_does_raise ~show_backtrace (fun () ->
    let (P key) = Capsule.create () in
    let mut = Capsule.Mutex.create key in
    Capsule.Mutex.with_lock mut ~f:(fun _ ->
      let (_ : _) = failwith "fail" in
      ()));
  [%expect {| (Failure fail) |}]
;;

let%expect_test "[with_lock] encapsulated" =
  Expect_test_helpers_base.require_does_raise ~show_backtrace (fun () ->
    let (P key) = Capsule.create () in
    let mut = Capsule.Mutex.create key in
    Capsule.Mutex.with_lock mut ~f:(fun password ->
      let d = Capsule.Data.create (fun () -> ()) in
      let (_ : _) = Capsule.Data.map d ~password ~f:(fun _ -> failwith "fail") in
      ()));
  [%expect {| (Failure fail) |}]
;;

let%expect_test "[with_lock] encapsulated shared" =
  Expect_test_helpers_base.require_does_raise ~show_backtrace (fun () ->
    let (P key) = Capsule.create () in
    let mut = Capsule.Mutex.create key in
    Capsule.Mutex.with_lock mut ~f:(fun password ->
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
      Expect_test_helpers_base.require_does_raise ~show_backtrace (fun () ->
        let (P key) = Capsule.create () in
        let mut = Capsule.Mutex.create key in
        Capsule.Mutex.with_lock mut ~f:(fun _ ->
          let d = Capsule.Data.create (fun () -> ()) in
          let (_ : _) = Capsule.Data.map d ~password ~f:(fun _ -> failwith "fail") in
          ())
        [@nontail])
      [@nontail])
  in
  ();
  [%expect {| ("Capsule.Encapsulated(_)") |}]
;;

let%expect_test ("[with_write_lock] unencapsulated"
  [@tags "runtime5-only", "no-js", "no-wasm"])
  =
  Expect_test_helpers_base.require_does_raise ~show_backtrace (fun () ->
    let (P key) = Capsule.create () in
    let rw = Capsule.Rwlock.create key in
    Capsule.Rwlock.with_write_lock rw ~f:(fun _ ->
      let (_ : _) = failwith "fail" in
      ()));
  [%expect {| (Failure fail) |}]
;;

let%expect_test ("with_write_lock encapsulated"
  [@tags "runtime5-only", "no-js", "no-wasm"])
  =
  Expect_test_helpers_base.require_does_raise ~show_backtrace (fun () ->
    let (P key) = Capsule.create () in
    let rw = Capsule.Rwlock.create key in
    Capsule.Rwlock.with_write_lock rw ~f:(fun password ->
      let d = Capsule.Data.create (fun () -> ()) in
      let (_ : _) = Capsule.Data.map d ~password ~f:(fun _ -> failwith "fail") in
      ()));
  [%expect {| (Failure fail) |}]
;;

let%expect_test ("with_write_lock encapsulated shared"
  [@tags "runtime5-only", "no-js", "no-wasm"])
  =
  Expect_test_helpers_base.require_does_raise ~show_backtrace (fun () ->
    let (P key) = Capsule.create () in
    let rw = Capsule.Rwlock.create key in
    Capsule.Rwlock.with_write_lock rw ~f:(fun password ->
      let d = Capsule.Data.create (fun () -> ()) in
      let password = Capsule.Password.shared password in
      let (_ : _) = Capsule.Data.map_shared d ~password ~f:(fun _ -> failwith "fail") in
      ()));
  [%expect {| (Failure fail) |}]
;;

let%expect_test ("with_write_lock other capsule"
  [@tags "runtime5-only", "no-js", "no-wasm"])
  =
  let (P key) = Capsule.create () in
  let (_ : _) =
    Capsule.Key.with_password key ~f:(fun password ->
      Expect_test_helpers_base.require_does_raise ~show_backtrace (fun () ->
        let (P key) = Capsule.create () in
        let rw = Capsule.Rwlock.create key in
        Capsule.Rwlock.with_write_lock rw ~f:(fun _ ->
          let d = Capsule.Data.create (fun () -> ()) in
          let (_ : _) = Capsule.Data.map d ~password ~f:(fun _ -> failwith "fail") in
          ())
        [@nontail])
      [@nontail])
  in
  ();
  [%expect {| ("Capsule.Encapsulated(_)") |}]
;;

let%expect_test ("with_read_lock unencapsulated"
  [@tags "runtime5-only", "no-js", "no-wasm"])
  =
  Expect_test_helpers_base.require_does_raise ~show_backtrace (fun () ->
    let (P key) = Capsule.create () in
    let rw = Capsule.Rwlock.create key in
    Capsule.Rwlock.with_read_lock rw ~f:(fun _ ->
      let (_ : _) = failwith "fail" in
      ()));
  [%expect {| (Failure fail) |}]
;;

let%expect_test ("with_read_lock encapsulated shared"
  [@tags "runtime5-only", "no-js", "no-wasm"])
  =
  Expect_test_helpers_base.require_does_raise ~show_backtrace (fun () ->
    let (P key) = Capsule.create () in
    let rw = Capsule.Rwlock.create key in
    Capsule.Rwlock.with_read_lock rw ~f:(fun password ->
      let d = Capsule.Data.create (fun () -> ()) in
      let (_ : _) = Capsule.Data.map_shared d ~password ~f:(fun _ -> failwith "fail") in
      ()));
  [%expect {| (Failure fail) |}]
;;

let%expect_test ("with_read_lock other capsule"
  [@tags "runtime5-only", "no-js", "no-wasm"])
  =
  let (P key) = Capsule.create () in
  let (_ : _) =
    Capsule.Key.with_password key ~f:(fun password ->
      Expect_test_helpers_base.require_does_raise ~show_backtrace (fun () ->
        let (P key) = Capsule.create () in
        let rw = Capsule.Rwlock.create key in
        Capsule.Rwlock.with_read_lock rw ~f:(fun _ ->
          let d = Capsule.Data.create (fun () -> ()) in
          let (_ : _) = Capsule.Data.map d ~password ~f:(fun _ -> failwith "fail") in
          ())
        [@nontail])
      [@nontail])
  in
  ();
  [%expect {| ("Capsule.Encapsulated(_)") |}]
;;
