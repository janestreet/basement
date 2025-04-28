module Global = struct
  type 'a t = { global : 'a } [@@unboxed]
end

open Global

module Access : sig
  (* TODO: this should have layout [void], but
     [void] can't be used for function argument and return types yet. *)
  type 'k t [@@immediate]
  type packed = P : 'k t -> packed [@@unboxed]

  (* Can break soundness. *)
  val unsafe_mk : unit -> 'k t
  val equality_witness : 'k t -> 'j t -> ('k, 'j) Type.eq
end = struct
  type dummy
  type 'k t = T : dummy t
  type packed = P : 'k t -> packed [@@unboxed]

  external unsafe_rebrand : 'k t -> 'j t = "%identity"

  let[@inline] unsafe_mk (type k) () : k t = unsafe_rebrand T

  let[@inline] equality_witness (type k j) (T : k t) (T : j t) : (k, j) Type.eq =
    Type.Equal
  ;;
end

let[@inline] current () = Access.P (Access.unsafe_mk ())

type initial

let initial = Access.unsafe_mk ()

let get_initial =
  if Stdlib_shim.runtime5 ()
  then
    fun [@inline] _ ->
    if Stdlib.Domain.is_main_domain () then Some (Access.unsafe_mk ()) else None
  else fun [@inline] _ -> Some (Access.unsafe_mk ())
;;

module Password : sig
  type 'k t

  module Id : sig
    type 'k t [@@immediate]

    val equality_witness : 'k1 t -> 'k2 t -> ('k1, 'k2) Type.eq option
  end

  (* Can break the soundness of the API. *)
  val unsafe_mk : unit -> 'k t
  val id : 'k t -> 'k Id.t

  module Shared : sig
    type 'k t

    (* Can break the soundness of the API. *)
    val unsafe_mk : unit -> 'k t
    val id : 'k t -> 'k Id.t
  end

  val shared : 'k t -> 'k Shared.t
  val with_current : 'k Access.t -> ('k t -> 'a) -> 'a
end = struct
  module Id = struct
    type 'k t = int

    let uninitialized = 0
    let ctr = Stdlib_shim.Atomic.Safe.make (uninitialized + 1)
    let[@inline] unsafe_mk () = Stdlib_shim.Atomic.Safe.fetch_and_add ctr 1

    let[@inline] equality_witness t1 t2 =
      if Int.equal t1 t2 then Some (Obj.magic Type.Equal) else None
    ;;
  end

  type 'k t = int Atomic.t

  let[@inline] unsafe_mk () = Stdlib_shim.Atomic.Safe.make Id.uninitialized

  let[@inline] id (t : 'k t) =
    (* Safe since [Password.t] is only ever accessible by 1 fiber. *)
    match Stdlib_shim.Atomic.Expert.fenceless_get t with
    | id when id = Id.uninitialized ->
      let set_id = Id.unsafe_mk () in
      Stdlib_shim.Atomic.Expert.fenceless_set t set_id;
      set_id
    | set_id -> set_id
  ;;

  module Shared = struct
    type 'k t = int Atomic.t

    (* Multiple fibers can access the same [Password.Shared.t] concurrently.
       Therefore, we use atomic operations. *)
    let[@inline] id (t : 'k t) =
      match Stdlib_shim.Atomic.Safe.get t with
      | id when id = Id.uninitialized ->
        let new_id = Id.unsafe_mk () in
        (match Stdlib_shim.Atomic.Safe.compare_exchange t Id.uninitialized new_id with
         | id when id = Id.uninitialized -> new_id
         | already_set_id -> already_set_id)
      | set_id -> set_id
    ;;

    let[@inline] unsafe_mk () = Stdlib_shim.Atomic.Safe.make Id.uninitialized
  end

  let[@inline] shared t = t
  let[@inline] with_current _ f = f (unsafe_mk ()) [@nontail]
end

(* Like [Stdlib.raise], but [portable], and the value it never returns is also [portable unique] *)
external reraise : exn -> 'a = "%reraise"

external raise_with_backtrace
  :  exn
  -> Printexc.raw_backtrace
  -> 'a
  = "%raise_with_backtrace"

external get_raw_backtrace
  :  unit
  -> Printexc.raw_backtrace
  = "caml_get_exception_raw_backtrace"

module Data = struct
  type ('a, 'k) t

  exception Encapsulated : 'k Password.Id.t * (exn, 'k) t -> exn

  external unsafe_mk : ('a[@local_opt]) -> (('a, 'k) t[@local_opt]) = "%identity"
  external unsafe_get : (('a, 'k) t[@local_opt]) -> ('a[@local_opt]) = "%identity"

  let[@inline] wrap ~access:_ t = unsafe_mk t
  let[@inline] unwrap ~access:_ t = unsafe_get t
  let[@inline] unwrap_shared ~access:_ t = unsafe_get t
  let[@inline] create f = unsafe_mk (f ())

  let[@inline never] reraise_encapsulated password exn =
    raise_with_backtrace
      (Encapsulated (Password.id password, unsafe_mk exn))
      (get_raw_backtrace ())
  ;;

  let[@inline] map ~password ~f t =
    let v = unsafe_get t in
    match f v with
    | res -> unsafe_mk res
    | exception exn -> reraise_encapsulated password exn
  ;;

  let[@inline] fst t =
    let t1, _ = unsafe_get t in
    unsafe_mk t1
  ;;

  let[@inline] snd t =
    let _, t2 = unsafe_get t in
    unsafe_mk t2
  ;;

  let[@inline] both t1 t2 = unsafe_mk (unsafe_get t1, unsafe_get t2)

  let[@inline] extract ~password ~f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise_encapsulated password exn
  ;;

  let inject = unsafe_mk
  let project = unsafe_get

  let[@inline] bind ~password ~f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise_encapsulated password exn
  ;;

  let[@inline] iter ~password ~f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise_encapsulated password exn
  ;;

  module Shared = struct
    type ('a, 'k) data = ('a, 'k) t
    type ('a, 'k) t = ('a, 'k) data

    exception Encapsulated_shared : 'k Password.Id.t * (exn, 'k) t -> exn

    let[@inline never] reraise_encapsulated_shared p e =
      raise_with_backtrace
        (Encapsulated_shared (Password.Shared.id p, unsafe_mk e))
        (get_raw_backtrace ())
    ;;

    let[@inline] wrap ~access:_ v = unsafe_mk v
    let[@inline] unwrap ~access:_ t = unsafe_get t
    let[@inline] expose ~key:_ t = unsafe_get t
    let[@inline] create f = unsafe_mk (f ())

    let[@inline] map ~password ~f t =
      let v = unsafe_get t in
      match f v with
      | r -> unsafe_mk r
      | exception exn -> reraise_encapsulated_shared password exn
    ;;

    let[@inline] both t1 t2 = unsafe_mk (unsafe_get t1, unsafe_get t2)

    let[@inline] fst t =
      let x, _ = unsafe_get t in
      unsafe_mk x
    ;;

    let[@inline] snd t =
      let _, y = unsafe_get t in
      unsafe_mk y
    ;;

    let[@inline] extract ~password ~f t =
      let v = unsafe_get t in
      try f v with
      | exn -> reraise_encapsulated_shared password exn
    ;;

    let[@inline] inject v = unsafe_mk v
    let[@inline] project t = unsafe_get t

    let[@inline] bind ~password ~f t =
      let v = unsafe_get t in
      try f v with
      | exn -> reraise_encapsulated_shared password exn
    ;;

    let[@inline] iter ~password ~f t =
      let v = unsafe_get t in
      try f v with
      | exn -> reraise_encapsulated_shared password exn
    ;;

    let[@inline] map_into ~password ~f t =
      let v = unsafe_get t in
      match f v with
      | res -> unsafe_mk res
      | exception exn -> reraise_encapsulated_shared password exn
    ;;

    module Local = struct
      let[@inline] wrap ~access:_ v = unsafe_mk v
      let[@inline] unwrap ~access:_ t = unsafe_get t
      let[@inline] create f = unsafe_mk (f ())

      let[@inline] map ~password ~f t =
        let v = unsafe_get t in
        match f v with
        | r -> unsafe_mk r
        | exception exn -> reraise_encapsulated_shared password exn
      ;;

      let[@inline] both t1 t2 = unsafe_mk (unsafe_get t1, unsafe_get t2)

      let[@inline] fst t =
        let x, _ = unsafe_get t in
        unsafe_mk x
      ;;

      let[@inline] snd t =
        let _, y = unsafe_get t in
        unsafe_mk y
      ;;

      let[@inline] extract ~password ~f t =
        let v = unsafe_get t in
        try f v with
        | exn -> reraise_encapsulated_shared password exn
      ;;

      let[@inline] inject v = unsafe_mk v
      let[@inline] project t = unsafe_get t

      let[@inline] bind ~password ~f t =
        let v = unsafe_get t in
        try f v with
        | exn -> reraise_encapsulated_shared password exn
      ;;

      let[@inline] iter ~password ~f t =
        let v = unsafe_get t in
        try f v with
        | exn -> reraise_encapsulated_shared password exn
      ;;

      let[@inline] map_into ~password ~f t =
        let v = unsafe_get t in
        match f v with
        | res -> unsafe_mk res
        | exception exn -> reraise_encapsulated_shared password exn
      ;;
    end
  end

  let reraise_encapsulated_shared = Shared.reraise_encapsulated_shared

  let[@inline] map_shared ~password ~f t =
    let v = unsafe_get t in
    match f v with
    | res -> unsafe_mk res
    | exception exn -> reraise_encapsulated_shared password exn
  ;;

  let[@inline] extract_shared ~password ~f t =
    let v = unsafe_get t in
    try f v with
    | exn -> reraise_encapsulated_shared password exn
  ;;

  module Local = struct
    let[@inline] wrap ~access:_ t = unsafe_mk t
    let[@inline] unwrap ~access:_ t = unsafe_get t
    let[@inline] unwrap_shared ~access:_ t = unsafe_get t
    let[@inline] create f = unsafe_mk (f ())

    let[@inline] map ~password ~f t =
      let v = unsafe_get t in
      match f v with
      | res -> unsafe_mk res
      | exception exn -> reraise_encapsulated password exn
    ;;

    let[@inline] fst t =
      let t1, _ = unsafe_get t in
      unsafe_mk t1
    ;;

    let[@inline] snd t =
      let _, t2 = unsafe_get t in
      unsafe_mk t2
    ;;

    let[@inline] both t1 t2 = unsafe_mk (unsafe_get t1, unsafe_get t2)

    let[@inline] extract ~password ~f t =
      let v = unsafe_get t in
      try f v with
      | exn -> reraise_encapsulated password exn
    ;;

    let[@inline] inject v = unsafe_mk v
    let[@inline] project t = unsafe_get t

    let[@inline] bind ~password ~f t =
      let v = unsafe_get t in
      try f v with
      | exn -> reraise_encapsulated password exn
    ;;

    let[@inline] iter ~password ~f t =
      let v = unsafe_get t in
      try f v with
      | exn -> reraise_encapsulated password exn
    ;;

    let[@inline] map_shared ~password ~f t =
      let v = unsafe_get t in
      match f v with
      | res -> unsafe_mk res
      | exception exn -> reraise_encapsulated_shared password exn
    ;;

    let[@inline] extract_shared ~password ~f t =
      let v = unsafe_get t in
      try f v with
      | exn -> reraise_encapsulated_shared password exn
    ;;
  end
end

exception Encapsulated = Data.Encapsulated
exception Encapsulated_shared = Data.Shared.Encapsulated_shared

let () =
  (* Hide the password id from the exception as it is generally useless and makes
     tests unstable. *)
  Stdlib_shim.Printexc.Safe.register_printer (function
    | Encapsulated _ -> Some "Capsule.Encapsulated(_)"
    | Encapsulated_shared _ -> Some "Capsule.Encapsulated_shared(_)"
    | _ -> None)
;;

module Key : sig
  type 'k t [@@immediate]
  type packed = P : 'k t -> packed [@@unboxed]

  val unsafe_mk : unit -> 'k t
  val with_password : 'k t -> f:('k Password.t -> 'a) -> 'a * 'k t
  val with_password_local : 'k t -> f:('k Password.t -> 'a) -> 'a
  val with_password_shared : 'k t -> f:('k Password.Shared.t -> 'a) -> 'a
  val with_password_shared_local : 'k t -> f:('k Password.Shared.t -> 'a) -> 'a
  val access : 'k t -> f:('k Access.t -> 'a) -> 'a * 'k t
  val access_local : 'k t -> f:('k Access.t -> 'a) -> 'a * 'k t
  val access_shared : 'k t -> f:('k Access.t -> 'a) -> 'a
  val access_shared_local : 'k t -> f:('k Access.t -> 'a) -> 'a
  val globalize_unique : 'k t -> 'k t
  val destroy : 'k t -> 'k Access.t
end = struct
  type 'k t = unit
  type packed = P : 'k t -> packed [@@unboxed]

  let[@inline] unsafe_mk () = ()

  let[@inline never] reraise_encapsulated (type k) (password : k Password.t) = function
    | Encapsulated (id, data) as exn ->
      (match Password.Id.equality_witness (Password.id password) id with
       | Some Equal -> raise_with_backtrace (Data.unsafe_get data) (get_raw_backtrace ())
       | None -> reraise exn)
    | Encapsulated_shared (id, data) as exn ->
      (match Password.Id.equality_witness (Password.id password) id with
       | Some Equal -> raise_with_backtrace (Data.unsafe_get data) (get_raw_backtrace ())
       | None -> reraise exn)
    | exn -> reraise exn
  ;;

  let[@inline never] reraise_encapsulated_shared (type k) (password : k Password.Shared.t)
    = function
    | Encapsulated_shared (id, data) as exn ->
      (match Password.Id.equality_witness (Password.Shared.id password) id with
       | Some Equal -> raise_with_backtrace (Data.unsafe_get data) (get_raw_backtrace ())
       | None -> reraise exn)
    | exn -> reraise exn
  ;;

  let[@inline] with_password_shared (type k) _ ~f =
    let password : k Password.Shared.t = Password.Shared.unsafe_mk () in
    try f password with
    | exn -> reraise_encapsulated_shared password exn [@nontail]
  ;;

  let[@inline] with_password_shared_local (type k) _ ~f =
    let password : k Password.Shared.t = Password.Shared.unsafe_mk () in
    try f password with
    | exn -> reraise_encapsulated_shared password exn [@nontail]
  ;;

  let[@inline] with_password (type k) k ~f =
    let password : k Password.t = Password.unsafe_mk () in
    try f password, k with
    | exn -> reraise_encapsulated password exn [@nontail]
  ;;

  let[@inline] with_password_local (type k) _ ~f =
    let password : k Password.t = Password.unsafe_mk () in
    try f password with
    | exn -> reraise_encapsulated password exn [@nontail]
  ;;

  let[@inline] access k ~f = f (Access.unsafe_mk ()), k
  let[@inline] access_local k ~f = f (Access.unsafe_mk ()), k

  let[@inline] access_shared _ ~f =
    let c : 'k Access.t = Access.unsafe_mk () in
    f c
  ;;

  let[@inline] access_shared_local _ ~f =
    let c : 'k Access.t = Access.unsafe_mk () in
    f c
  ;;

  let[@inline] globalize_unique k = k
  let[@inline] destroy _ = Access.unsafe_mk ()
end

let[@inline] create () = Key.P (Key.unsafe_mk ())

let[@inline] access_local (type k) ~(password : k Password.t) ~f =
  let c : k Access.t = Access.unsafe_mk () in
  match f c with
  | res -> res
  | exception exn -> Data.reraise_encapsulated password exn
;;

let[@inline] access ~password ~f =
  (access_local ~password ~f:(fun access -> { global = f access })).global
;;

let[@inline] access_shared_local (type k) ~(password : k Password.Shared.t) ~f =
  let c : k Access.t = Access.unsafe_mk () in
  match f c with
  | res -> res
  | exception exn -> Data.reraise_encapsulated_shared password exn
;;

let[@inline] access_shared ~password ~f =
  (access_shared_local ~password ~f:(fun access -> { global = f access })).global
;;

(* Like [Stdlib.Mutex], but [portable]. *)
module M = struct
  type t

  external create : unit -> t = "caml_capsule_mutex_new"
  external lock : t -> unit = "caml_capsule_mutex_lock"
  external unlock : t -> unit = "caml_capsule_mutex_unlock"
end

(* Reader writer lock *)
module Rw = struct
  type t

  external create : unit -> t = "caml_capsule_rwlock_new"
  external lock_read : t -> unit = "caml_capsule_rwlock_rdlock"
  external lock_write : t -> unit = "caml_capsule_rwlock_wrlock"
  external unlock : t -> unit = "caml_capsule_rwlock_unlock"
end

module Mutex = struct
  type mutex =
    { mutex : M.t
    ; mutable poisoned : bool
    }
  [@@unsafe_allow_any_mode_crossing
    "Unsafe mode crossing by design. The mutable [poisoned] field is protected by the \
     [mutex]. "]

  type 'k t = mutex

  type packed = P : 'k t -> packed
  [@@unboxed]
  [@@unsafe_allow_any_mode_crossing
    "TODO layouts v2.8: illegal mode crossing on the current version of the compiler, \
     but should be legal."]

  let[@inline] create _ = { mutex = M.create (); poisoned = false }

  exception Poisoned

  let[@inline never] poison_and_reraise : type k. k t -> pw:k Password.t -> exn:exn -> 'a =
    fun t ~pw ~exn ->
    t.poisoned <- true;
    M.unlock t.mutex;
    match exn with
    | Encapsulated (id, data) ->
      (match Password.Id.equality_witness (Password.id pw) id with
       | Some Equal -> raise_with_backtrace (Data.unsafe_get data) (get_raw_backtrace ())
       | None -> reraise exn)
    | Encapsulated_shared (id, data) ->
      (match Password.Id.equality_witness (Password.id pw) id with
       | Some Equal -> raise_with_backtrace (Data.unsafe_get data) (get_raw_backtrace ())
       | None -> reraise exn)
    | _ -> reraise exn
  ;;

  let[@inline] with_lock : type k. k t -> f:(k Password.t -> 'a) -> 'a =
    fun t ~f ->
    M.lock t.mutex;
    match t.poisoned with
    | true ->
      M.unlock t.mutex;
      reraise Poisoned
    | false ->
      let pw : k Password.t = Password.unsafe_mk () in
      (match f pw with
       | x ->
         M.unlock t.mutex;
         x
       | exception exn -> poison_and_reraise t ~pw ~exn [@nontail])
  ;;

  let[@inline] destroy t =
    M.lock t.mutex;
    match t.poisoned with
    | true ->
      M.unlock t.mutex;
      reraise Poisoned
    | false ->
      t.poisoned <- true;
      M.unlock t.mutex;
      Key.unsafe_mk ()
  ;;
end

module Rwlock = struct
  type state =
    | Open
    | Frozen
    | Poisoned

  type rwlock =
    { rwlock : Rw.t
    ; mutable state : state
    }
  [@@unsafe_allow_any_mode_crossing
    "Unsafe mode crossing by design. The mutable [state] field is protected by the \
     [rwlock]. "]

  type 'k t = rwlock

  type packed = P : 'k t -> packed
  [@@unboxed]
  [@@unsafe_allow_any_mode_crossing
    "TODO layouts v2.8: This can go away once we have proper mode crossing inference for \
     GADT constructors "]

  let[@inline] create _ = { rwlock = Rw.create (); state = Open }

  exception Frozen
  exception Poisoned

  let[@inline never] poison_and_reraise : type k. k t -> pw:k Password.t -> exn:exn -> 'a =
    fun t ~pw ~exn ->
    t.state <- Poisoned;
    Rw.unlock t.rwlock;
    match exn with
    | Encapsulated (id, data) ->
      (match Password.Id.equality_witness (Password.id pw) id with
       | Some Equal -> raise_with_backtrace (Data.unsafe_get data) (get_raw_backtrace ())
       | None -> reraise exn)
    | Encapsulated_shared (id, data) ->
      (match Password.Id.equality_witness (Password.id pw) id with
       | Some Equal -> raise_with_backtrace (Data.unsafe_get data) (get_raw_backtrace ())
       | None -> reraise exn)
    | _ -> reraise exn
  ;;

  let[@inline] with_write_lock : type k. k t -> f:(k Password.t -> 'a) -> 'a =
    fun t ~f ->
    Rw.lock_write t.rwlock;
    match t.state with
    | Poisoned ->
      Rw.unlock t.rwlock;
      reraise Poisoned
    | Frozen ->
      Rw.unlock t.rwlock;
      reraise Frozen
    | Open ->
      let pw : k Password.t = Password.unsafe_mk () in
      (match f pw with
       | x ->
         Rw.unlock t.rwlock;
         x
       | exception exn -> poison_and_reraise t ~pw ~exn [@nontail])
  ;;

  let[@inline never] freeze_and_reraise
    : type k. k t -> pw:k Password.Shared.t -> exn:exn -> 'a
    =
    fun t ~pw ~exn ->
    (* This racy write is ok according to the memory model, because:
       1. All threads which race to write here are writing [Frozen],
           and the only other write to this field in the program was
           the initial write setting it to [Open].
       2. All threads which race to read here do not distinguish between
           [Open] and [Frozen].
       All operations that distinguish between [Open] and [Frozen]
       are protected by the write lock. *)
    t.state <- Frozen;
    Rw.unlock t.rwlock;
    match exn with
    | Encapsulated_shared (id, data) ->
      (match Password.Id.equality_witness (Password.Shared.id pw) id with
       | Some Equal -> raise_with_backtrace (Data.unsafe_get data) (get_raw_backtrace ())
       | None -> reraise exn)
    | _ -> reraise exn
  ;;

  let[@inline] with_read_lock : type k. k t -> f:(k Password.Shared.t -> 'a) -> 'a =
    fun t ~f ->
    Rw.lock_read t.rwlock;
    match t.state with
    | Poisoned ->
      Rw.unlock t.rwlock;
      reraise Poisoned
    | Open | Frozen ->
      let pw : k Password.Shared.t = Password.Shared.unsafe_mk () in
      (match f pw with
       | x ->
         Rw.unlock t.rwlock;
         x
       | exception exn -> freeze_and_reraise t ~pw ~exn [@nontail])
  ;;

  let[@inline] freeze t =
    Rw.lock_read t.rwlock;
    match t.state with
    | Poisoned -> reraise Poisoned
    | Open | Frozen ->
      t.state <- Frozen;
      Rw.unlock t.rwlock;
      Key.unsafe_mk ()
  ;;

  let[@inline] destroy t =
    Rw.lock_write t.rwlock;
    match t.state with
    | Poisoned ->
      Rw.unlock t.rwlock;
      reraise Poisoned
    | Frozen ->
      Rw.unlock t.rwlock;
      reraise Frozen
    | Open ->
      t.state <- Poisoned;
      Rw.unlock t.rwlock;
      Key.unsafe_mk ()
  ;;
end

module Condition = struct
  type 'k t

  external create : unit -> 'k t = "caml_capsule_condition_new"
  external wait : 'k t -> M.t -> unit = "caml_capsule_condition_wait"
  external signal : 'k t -> unit = "caml_capsule_condition_signal"
  external broadcast : 'k t -> unit = "caml_capsule_condition_broadcast"

  let[@inline] wait t ~(mutex : 'k Mutex.t) ~password:_ =
    (* [mutex] is locked, so we know it is not poisoned. *)
    wait t mutex.mutex
  ;;
end
