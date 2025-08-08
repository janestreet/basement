type 'a global = { global : 'a } [@@unboxed]

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
  type 'k t [@@immediate]

  (* Can break the soundness of the API. *)
  val unsafe_mk : unit -> 'k t

  module Shared : sig
    type 'k t [@@immediate]

    (* Can break the soundness of the API. *)
    val unsafe_mk : unit -> 'k t
  end

  val shared : 'k t -> 'k Shared.t
  val with_current : 'k Access.t -> ('k t -> 'a) -> 'a
end = struct
  type 'k t = unit

  let[@inline] unsafe_mk () = ()

  module Shared = struct
    type 'k t = unit

    let[@inline] unsafe_mk () = ()
  end

  let[@inline] shared t = t
  let[@inline] with_current _ f = f (unsafe_mk ()) [@nontail]
end

(* Like [Stdlib.raise], but [portable], and the value it never returns is also [portable unique] *)
external reraise : 'a. exn -> 'a = "%reraise"

module Data = struct
  type ('a, 'k) t

  external unsafe_mk : ('a[@local_opt]) -> (('a, 'k) t[@local_opt]) = "%identity"
  external unsafe_get : (('a, 'k) t[@local_opt]) -> ('a[@local_opt]) = "%identity"
  external unsafe_mk_unique : ('a[@local_opt]) -> (('a, 'k) t[@local_opt]) = "%identity"
  external unsafe_get_unique : (('a, 'k) t[@local_opt]) -> ('a[@local_opt]) = "%identity"
  external unsafe_mk_once : ('a[@local_opt]) -> (('a, 'k) t[@local_opt]) = "%identity"
  external unsafe_get_once : (('a, 'k) t[@local_opt]) -> ('a[@local_opt]) = "%identity"

  external unsafe_mk_once_unique
    :  ('a[@local_opt])
    -> (('a, 'k) t[@local_opt])
    = "%identity"

  external unsafe_get_once_unique
    :  (('a, 'k) t[@local_opt])
    -> ('a[@local_opt])
    = "%identity"

  let[@inline] wrap ~access:_ t = unsafe_mk t
  let[@inline] unwrap ~access:_ t = unsafe_get t
  let[@inline] wrap_unique ~access:_ t = unsafe_mk_unique t
  let[@inline] unwrap_unique ~access:_ t = unsafe_get_unique t
  let[@inline] wrap_once ~access:_ t = unsafe_mk_once t
  let[@inline] unwrap_once ~access:_ t = unsafe_get_once t
  let[@inline] wrap_once_unique ~access:_ t = unsafe_mk_once_unique t
  let[@inline] unwrap_once_unique ~access:_ t = unsafe_get_once_unique t
  let[@inline] unwrap_shared ~access:_ t = unsafe_get t
  let[@inline] create f = unsafe_mk (f ())
  let[@inline] create_once f = unsafe_mk_once (f ())
  let[@inline] map ~password:_ ~f t = unsafe_mk (f (unsafe_get t))

  let[@inline] fst t =
    let t1, _ = unsafe_get t in
    unsafe_mk t1
  ;;

  let[@inline] snd t =
    let _, t2 = unsafe_get t in
    unsafe_mk t2
  ;;

  let[@inline] both t1 t2 = unsafe_mk (unsafe_get t1, unsafe_get t2)
  let[@inline] extract ~password:_ ~f t = f (unsafe_get t)
  let inject = unsafe_mk
  let project = unsafe_get
  let[@inline] project_shared ~key:_ t = unsafe_get t
  let[@inline] bind ~password:_ ~f t = f (unsafe_get t)
  let[@inline] iter ~password:_ ~f t = f (unsafe_get t)

  module Shared = struct
    type ('a, 'k) data = ('a, 'k) t
    type ('a, 'k) t = ('a, 'k) data

    let[@inline] wrap ~access:_ v = unsafe_mk v
    let[@inline] unwrap ~access:_ t = unsafe_get t
    let[@inline] expose ~key:_ t = unsafe_get t
    let[@inline] create f = unsafe_mk (f ())
    let[@inline] map ~password:_ ~f t = unsafe_mk (f (unsafe_get t))
    let[@inline] both t1 t2 = unsafe_mk (unsafe_get t1, unsafe_get t2)

    let[@inline] fst t =
      let x, _ = unsafe_get t in
      unsafe_mk x
    ;;

    let[@inline] snd t =
      let _, y = unsafe_get t in
      unsafe_mk y
    ;;

    let[@inline] extract ~password:_ ~f t = f (unsafe_get t)
    let[@inline] inject v = unsafe_mk v
    let[@inline] project t = unsafe_get t
    let[@inline] bind ~password:_ ~f t = f (unsafe_get t)
    let[@inline] iter ~password:_ ~f t = f (unsafe_get t)
    let[@inline] map_into ~password:_ ~f t = unsafe_mk (f (unsafe_get t))

    module Local = struct
      let[@inline] wrap ~access:_ v = unsafe_mk v
      let[@inline] unwrap ~access:_ t = unsafe_get t
      let[@inline] create f = unsafe_mk (f ())
      let[@inline] map ~password:_ ~f t = unsafe_mk (f (unsafe_get t))
      let[@inline] both t1 t2 = unsafe_mk (unsafe_get t1, unsafe_get t2)

      let[@inline] fst t =
        let x, _ = unsafe_get t in
        unsafe_mk x
      ;;

      let[@inline] snd t =
        let _, y = unsafe_get t in
        unsafe_mk y
      ;;

      let[@inline] extract ~password:_ ~f t = f (unsafe_get t)
      let[@inline] inject v = unsafe_mk v
      let[@inline] project t = unsafe_get t
      let[@inline] bind ~password:_ ~f t = f (unsafe_get t)
      let[@inline] iter ~password:_ ~f t = f (unsafe_get t) [@nontail]
      let[@inline] map_into ~password:_ ~f t = unsafe_mk (f (unsafe_get t))
    end
  end

  let[@inline] map_shared ~password:_ ~f t = unsafe_mk (f (unsafe_get t))
  let[@inline] extract_shared ~password:_ ~f t = f (unsafe_get t)

  module Local = struct
    let[@inline] wrap ~access:_ t = unsafe_mk t
    let[@inline] unwrap ~access:_ t = unsafe_get t
    let[@inline] wrap_unique ~access:_ t = unsafe_mk_unique t
    let[@inline] unwrap_unique ~access:_ t = unsafe_get_unique t
    let[@inline] unwrap_shared ~access:_ t = unsafe_get t
    let[@inline] create f = unsafe_mk (f ())
    let[@inline] map ~password:_ ~f t = unsafe_mk (f (unsafe_get t))

    let[@inline] fst t =
      let t1, _ = unsafe_get t in
      unsafe_mk t1
    ;;

    let[@inline] snd t =
      let _, t2 = unsafe_get t in
      unsafe_mk t2
    ;;

    let[@inline] both t1 t2 = unsafe_mk (unsafe_get t1, unsafe_get t2)
    let[@inline] extract ~password:_ ~f t = f (unsafe_get t)
    let[@inline] inject v = unsafe_mk v
    let[@inline] project t = unsafe_get t
    let[@inline] project_shared ~key:_ t = unsafe_get t
    let[@inline] bind ~password:_ ~f t = f (unsafe_get t)
    let[@inline] iter ~password:_ ~f t = f (unsafe_get t) [@nontail]
    let[@inline] map_shared ~password:_ ~f t = unsafe_mk (f (unsafe_get t))
    let[@inline] extract_shared ~password:_ ~f t = f (unsafe_get t)
  end
end

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

  let[@inline] with_password_shared (type k) _ ~f =
    let password : k Password.Shared.t = Password.Shared.unsafe_mk () in
    f password [@nontail]
  ;;

  let[@inline] with_password_shared_local (type k) _ ~f =
    let password : k Password.Shared.t = Password.Shared.unsafe_mk () in
    f password
  ;;

  let[@inline] with_password (type k) k ~f =
    let password : k Password.t = Password.unsafe_mk () in
    f password, k
  ;;

  let[@inline] with_password_local (type k) _ ~f =
    let password : k Password.t = Password.unsafe_mk () in
    f password
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

let[@inline] access_local ~password:_ ~f =
  let c : _ Access.t = Access.unsafe_mk () in
  f c
;;

let[@inline] access ~password ~f =
  (access_local ~password ~f:(fun access -> { global = f access })).global
;;

let[@inline] access_shared_local ~password:_ ~f =
  let c : _ Access.t = Access.unsafe_mk () in
  f c
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
  type packed = P : 'k t -> packed [@@unboxed]

  let[@inline] create _ = { mutex = M.create (); poisoned = false }

  exception Poisoned

  let[@inline never] poison_and_reraise : type a k. k t -> exn:exn -> a =
    fun t ~exn ->
    t.poisoned <- true;
    M.unlock t.mutex;
    reraise exn
  ;;

  let[@inline] with_lock : type a k. k t -> f:(k Password.t -> a) -> a =
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
       | exception exn -> poison_and_reraise t ~exn [@nontail])
  ;;

  let[@inline] with_key : type a k. k t -> f:(k Key.t -> a * k Key.t) -> a =
    fun t ~f ->
    M.lock t.mutex;
    match t.poisoned with
    | true ->
      M.unlock t.mutex;
      reraise Poisoned
    | false ->
      let key : k Key.t = Key.unsafe_mk () in
      (match f key with
       | x, _key ->
         M.unlock t.mutex;
         x
       | exception exn ->
         t.poisoned <- true;
         M.unlock t.mutex;
         reraise exn)
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

  let[@inline never] poison_and_reraise : type k. k t -> exn:exn -> 'a =
    fun t ~exn ->
    t.state <- Poisoned;
    Rw.unlock t.rwlock;
    reraise exn
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
       | exception exn -> poison_and_reraise t ~exn [@nontail])
  ;;

  let[@inline never] freeze_and_reraise : type k. k t -> exn:exn -> 'a =
    fun t ~exn ->
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
    reraise exn
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
       | exception exn -> freeze_and_reraise t ~exn [@nontail])
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

  let[@inline] wait t ~(mutex : 'k Mutex.t) key =
    (* Check that the mutex is not poisoned. It's safe to do so without locking:
       either we hold the [key] because it's locked, or because it's poisoned. *)
    match mutex.poisoned with
    | true -> raise Mutex.Poisoned
    | false ->
      wait t mutex.mutex;
      (* Check that the mutex wasn't poisoned again while we were waiting.
         If it was, we can't return the key. *)
      (match mutex.poisoned with
       | true -> raise Mutex.Poisoned
       | false -> key)
  ;;
end
