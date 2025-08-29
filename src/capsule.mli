(** Capsules are a mechanism for safely having [uncontended] access to mutable data from
    multiple domains. The interface in this module ensures that only one domain can have
    [uncontended] access to that data at a time.

    We consider every piece of mutable data in the program to live inside of some capsule.
    This might be an explicit capsule created by the user using this interface, or an
    implicit capsule created when a new domain is started. Whenever some thread is
    executing a function it has uncontended access to a single capsule and any new mutable
    data it creates is created within that capsule. We say that the function is "running
    within" the capsule.

    Capsules are only ever associated with threads from one domain at a time, which
    ensures there are no data races in the program. The implicit capsules created when a
    new domain is started are only ever associated with threads in that domain. Explicit
    capsules can change which domains have uncontended access to them using
    synchronization primitives.

    Each explicit capsule is associated with a type "brand" -- written ['k] throughout
    this interface -- which allows us to statically reason about access to the capsule
    within the type system. In the documentation of this interface we will often use ['k]
    to refer to the capsule associated with that brand.

    When you create a new capsule ['k] via [create], the library returns a unique
    ['k Key.t] that grants you exclusive ownership of that capsule. You can temporarily
    hand out a [Password.t] (or [Password.Shared.t]) to functions or threads that need to
    access the capsule. This ensures that mutable data is only ever accessed in accordance
    with the capsule's concurrency rules.

    A ['k Key.t @ unique] can be consumed to create a ['k Mutex.t] or a ['k Rwlock.t] that
    allows dynamic access to the same capsule from different threads. Mutexes support
    exclusive write access from one thread, while rw-locks support either write access
    from one thread or read access from multiple. Both mutexes and rw-locks can be
    destroyed to get the ['k Key.t @ unique] back.

    Using a ['k Key.t] more than once turns it into an [aliased] key. Aliased global keys
    indicate that the corresponding capsule has been permanently shared across threads
    with read-only access.

    {1 Exceptions}

    Currently, it is possible to break the soundness guarantees of the capsule API by
    defining an exception which contains mutable state or nonportable functions, and
    "smuggling" values out of a capsule by raising that exception out of one of the
    callbacks in this module. In the medium-term, we plan to distinguish portable
    exception constructors, whose contents cross portability and contention, from
    nonportable exception constructors. In the meantime we are consciously leaving this
    soundness gap for the sake of improved ergonomics over encapsulating exceptions. *)

(** An [Access.t] allows wrapping and unwrapping [Data.t] values from the current capsule. *)
module Access : sig
  (** ['k t] represents access to the current capsule, allowing wrapping and unwrapping
      [Data.t] values. An [uncontended] ['k t] indicates that ['k] is the current capsule.
      A [shared] ['k t] indicates that ['k] is the current capsule but that it may be
      shared with other domains.

      ['k t]s captured in a [portable] closure become [contended], which prevents sharing
      them with other domains. *)
  type 'k t [@@immediate]

  (** [packed] is the type of access to some unknown capsule. Unpacking one provides a
      ['k t] together with a fresh existential type brand for ['k]. *)
  type packed = P : 'k t -> packed [@@unboxed]

  (** [equality_witness a b] returns a witness that the brands of [a] and [b] are equal.
      This must be true since they are both present uncontended within the same capsule. *)
  val equality_witness : 'k t -> 'j t -> ('k, 'j) Type.eq
end

(** Obtain an [Access.t] for the current capsule. Since we do not know the brand for the
    current capsule, we receive a fresh one. *)
val current : unit -> Access.packed

(** The brand for the initial capsule. *)
type initial

(** An [Access.t] for the initial capsule *)
val initial : initial Access.t

(** Dynamically get an [Access.t] for the initial capsule within a portable context. This
    function requires a [DLS.Access.t] to ensure that the current capsule is a domain
    capsule, and returns [None] if the current domain is not the initial domain. *)
val get_initial : Stdlib_shim.Domain.Safe.DLS.Access.t -> initial Access.t option

(** Passwords represent permission to get access to a capsule. *)
module Password : sig
  (** ['k t] is the type of "passwords" representing permission for the current domain to
      have [uncontended] access to the capsule ['k]. They are only ever available locally,
      so cannot move between domains.

      Obtaining a ['k t] requires exclusive access to ['k], either through a
      ['k Key.t @ unique] or indirectly through acquiring a mutex or rw-lock associated
      with ['k]. The mode system prevents retaining the ['k t] after releasing access to
      the capsule. This guarantees that uncontended access to the capsule is only granted
      to one domain at a time. *)
  type 'k t [@@immediate]

  (** Shared passwords represent permission to get shared access to a capsule. *)
  module Shared : sig
    (** ['k t] is the type of "shared passwords" representing permission for the current
        domain to have [shared] access to the capsule ['k]. They are only ever available
        locally, and so cannot move between domains.

        Obtaining a ['k t] requires a ['k Key.t @ aliased] or read-acquiring the
        reader-writer lock associated with ['k]. *)
    type 'k t [@@immediate]
  end

  (** [shared t] downgrades a ['k] password to a ['k] shared password. *)
  val shared : 'k t -> 'k Shared.t

  (** [with_current k f] calls [f] with a password for the current capsule [k]. *)
  val with_current : 'k Access.t -> ('k t -> 'a) -> 'a
end

(** Keys represent the ownership of the capsule. *)
module Key : sig
  (** ['k t @ unique] represents the exclusive ownership of the capsule ['k]. The
      [unique]ness of ['k t] guarantees that only one domain can access the capsule at a
      time.

      ['k t @ aliased] indicates that the key has been permanently shared, since it's
      avaiable [aliased] and therefore not available [unique]ly. Therefore, we can allow
      all domains read access to ['k]. *)
  type 'k t [@@immediate]

  type packed = P : 'k t -> packed [@@unboxed]

  (** [with_password k ~f] runs [f], providing it a password for ['k], and returns the
      result of [f] together with the key.

      If [f] raises an exception, the key is destroyed, leaking the contents of the
      capsule. *)
  val with_password : 'k t -> f:('k Password.t -> 'a) -> 'a * 'k t

  (** [with_password_local k ~f] runs [f], providing it a password for ['k], and returns
      the result of [f]. The key is destroyed, but the local password can be returned to
      provide access to the capsule. *)
  val with_password_local : 'k t -> f:('k Password.t -> 'a) -> 'a

  (** [with_password_shared k ~f] runs [f], providing it a shared password for ['k], and
      returns the result of [f]. *)
  val with_password_shared : 'k t -> f:('k Password.Shared.t -> 'a) -> 'a

  (** As [with_password_shared], but returns a local value. *)
  val with_password_shared_local : 'k t -> f:('k Password.Shared.t -> 'a) -> 'a

  (** [access k ~f] runs [f], providing it access to the capsule ['k], and returns the
      result of [f] together with the key.

      If [f] raises an exception, the key is destroyed, leaking the contents of the
      capsule, and the exception is reraised. *)
  val access : 'k t -> f:('k Access.t -> 'a) -> 'a * 'k t

  (** As [access], but local. *)
  val access_local : 'k t -> f:('k Access.t -> 'a) -> 'a * 'k t

  (** [access_shared k ~f] runs [f], providing it a shared access to ['k], and returns the
      result of [f]. Exceptions raised from [f] are re-raised. *)
  val access_shared : 'k t -> f:('k Access.t -> 'a) -> 'a

  (** As [access_shared], but returns a local value. *)
  val access_shared_local : 'k t -> f:('k Access.t -> 'a) -> 'a

  (** [globalize_unique k] promotes a local unique key to a global one. *)
  val globalize_unique : 'k t -> 'k t

  (** [destroy k] returns ['k Access.t] for ['k], merging it with the current capsule. The
      key is destroyed. *)
  val destroy : 'k t -> 'k Access.t

  (** [unsafe_mk ()] unsafely makes a unique key for an arbitrary capsule. *)
  val unsafe_mk : unit -> 'k t
end

(** [create ()] creates a new capsule with an associated key. *)
val create : unit -> Key.packed

(** [access ~password ~f] runs [f] within the capsule ['k], providing it with an
    {!Access.t} for ['k]. The result is within ['k] so it must be [portable] and it is
    marked [contended]. *)
val access : password:'k Password.t -> f:('k Access.t -> 'a) -> 'a

(** As [access], but returns a local value. *)
val access_local : password:'k Password.t -> f:('k Access.t -> 'a) -> 'a

(** [shared_access ~password ~f] runs [f] within the capsule ['k], providing it with a
    shared {!Access.t} for ['k]. The result is within ['k] so it must be [portable] and it
    is marked [contended]. *)
val access_shared : password:'k Password.Shared.t -> f:('k Access.t -> 'a) -> 'a

(** As [access_shared], but returns a local value. *)
val access_shared_local : password:'k Password.Shared.t -> f:('k Access.t -> 'a) -> 'a

module Mutex : sig
  (** Private implementation type exposed to allow packing mutexes into [@@unboxed]
      records/variants. *)
  type mutex

  (** ['k t] is the type of the mutex that controls access to the capsule ['k]. A mutex
      can be created from a ['k Key @ unique]. *)
  type 'k t = private mutex

  (** [packed] is the type of a mutex for some unknown capsule. Unpacking one provides a
      ['k Mutex.t] together with a fresh existential type brand for ['k]. *)
  type packed = P : 'k t -> packed [@@unboxed]

  (** [create k] creates a new mutex for the capsule ['k], consuming its key. *)
  val create : 'k Key.t -> 'k t

  (** Raising an uncaught exception while holding the lock poisons the mutex. All
      operations on a poisoned mutex raise the [Poisoned] exception. *)
  exception Poisoned

  (** [with_lock m ~f] tries to acquire the mutex [m]. If [m] is already locked, blocks
      the current thread until it's unlocked. If successful, provides [f] a password for
      the capsule ['k] associated with [m].

      If [f] raises an exception, the mutex is marked as poisoned and the exception is
      reraised.

      If [m] is already locked by the current thread, raises [Sys_error]. *)
  val with_lock : 'a 'k. 'k t -> f:('k Password.t -> 'a) -> 'a

  (** [with_key m ~f] tries to acquire the mutex [m]. If [m] is already locked, blocks the
      current thread until it's unlocked. If successful, provides [f] the key for the
      capsule ['k] associated with [m].

      If [f] raises an exception, the mutex is marked as poisoned and the exception is
      reraised.

      If [m] is already locked by the current thread, raises [Sys_error]. *)
  val with_key : 'a 'k. 'k t -> f:('k Key.t -> 'a * 'k Key.t) -> 'a

  (** [destroy m] acquires the mutex [m] and returns the key representing ownership of
      ['k]. The mutex is marked as poisoned. *)
  val destroy : 'k t -> 'k Key.t
end

module Rwlock : sig
  (** Private implementation type exposed to allow packing rwlocks into [@@unboxed]
      records/variants. *)
  type rwlock

  (** ['k t] is the type of the reader-writer lock that controls read and write access to
      the capsule ['k]. A reader-writer lock can be created from a ['k Key.t @ unique]. *)
  type 'k t = private rwlock

  (** [packed] is the type of a reader-writer lock for some unknown capsule. Unpacking one
      provides a ['k Rwlock.t] together with a fresh existential type brand for ['k]. *)
  type packed = P : 'k t -> packed
  [@@unboxed]
  [@@unsafe_allow_any_mode_crossing
    "TODO: This can go away once we have proper mode crossing inference for GADT \
     constructors "]

  (** [create k] creates a new reader-writer lock for the capsule ['k], consuming its key. *)
  val create : 'k Key.t -> 'k t

  (** Reader-writer locks can be marked as frozen. Write operations on a frozen
      reader-writer lock raise the [Frozen] exception. *)
  exception Frozen

  (** Raising an uncaught exception while holding the write lock poisons the reader-writer
      lock. All operations on a poisoned reader-writer lock raise the [Poisoned]
      exception. *)
  exception Poisoned

  (** [with_write_lock rw ~f] tries to write-acquire the rwlock [rw]. If [rw] is already
      write or read locked, blocks the current thread until it's unlocked. If successful,
      provides [f] a password for the capsule ['k] associated with [rw].

      If [f] raises an exception, the reader-writer lock is marked as poisoned and the
      exception is reraised.

      If [m] is already write-locked by the current thread, raises [Sys_error]. If [m] is
      already read-locked by the current thread, deadlocks. *)
  val with_write_lock : 'k t -> f:('k Password.t -> 'a) -> 'a

  (** [with_read_lock rw ~f] tries to read-acquire the rwlock [rw]. If [rw] is already
      write-locked, blocks the current thread until it's unlocked. If [rw] is already
      read-locked, increases the reader count by one for the duration of [f]. If
      successful, provides [f] a password for the capsule ['k] associated with [rw].

      If [f] raises an exception, the reader-writer lock is marked as frozen. The reader
      count is decreased and the exception is reraised.

      If [m] is already write-locked by the current thread, raises [Sys_error]. If [m] is
      already read-locked by the current thread, nothing happens. *)
  val with_read_lock : 'k t -> f:('k Password.Shared.t -> 'a) -> 'a

  (** [freeze rw] read-acquires the rwlock [rw] and freezes it, rendering ['k] immutable,
      and returns the [global aliased] key representing that.

      Has no effect if the rwlock is already frozen. *)
  val freeze : 'k t -> 'k Key.t

  (** [destroy rw] write-acquires the rwlock [rw] and returns the key representing the
      ownership of ['k]. It marks the reader-writer lock as poisoned.

      If [rw] is already poisoned, raises [Poisoned]. *)
  val destroy : 'k t -> 'k Key.t
end

module Condition : sig
  (** ['k t] is the type of a condition variable associated with the capsule ['k]. This
      condition may only be used with the matching ['k Mutex.t]. *)
  type 'k t

  (** [create ()] creates a new condition variable associated with the matching
      ['k Mutex.t] and with a certain property {i P} that is protected by the mutex. *)
  val create : unit -> 'k t

  (** [wait t ~mutex key] atomically unlocks the [mutex] and blocks the current thread on
      the condition variable [t]. To ensure exception safety, it takes hold of the
      ['k Key.t] associated with the mutex, provided by [Mutex.with_key].

      This thread will be woken up when the condition variable [t] has been signaled via
      {!signal} or {!broadcast}. [mutex] is locked again before [wait] returns.

      However, this thread can also be woken up for no reason. One cannot assume that the
      property {i P} associated with the condition variable [c] holds when [wait] returns;
      one must explicitly test whether {i P} holds after calling [wait].

      If called on an already poisoned [mutex], raises [Mutex.Poisoned]. *)
  val wait : 'k t -> mutex:'k Mutex.t -> 'k Key.t -> 'k Key.t

  (** [signal t] wakes up one of the threads waiting on the condition variable [t], if
      there is one. If there is none, this call has no effect. It is recommended to call
      [signal t] inside a critical section, that is, while the mutex associated with [t]
      is locked. *)
  val signal : 'k t -> unit

  (** [broadcast t] wakes up all threads waiting on the condition variable [t]. If there
      are none, this call has no effect. It is recommended to call [broadcast t] inside a
      critical section, that is, while the mutex associated with [t] is locked. *)
  val broadcast : 'k t -> unit
end

(** Pointers to data within a capsule. *)
module Data : sig
  (** [('a, 'k) t] is the type of ['a]s within the capsule ['k]. It can be passed between
      domains. Operations on [('a, 'k) t] require a ['k Password.t] associated with the
      capsule ['k]. *)
  type ('a, 'k) t

  (** [wrap ~access v] returns a pointer to the value [v], which lives in the capsule
      ['k]. ['k] is always the current capsule. *)
  val wrap : access:'k Access.t -> 'a -> ('a, 'k) t

  (** [unwrap ~access t] returns the value of [t], which lives in the capsule ['k]. ['k]
      is always the current capsule. *)
  val unwrap : access:'k Access.t -> ('a, 'k) t -> 'a

  (** Like [wrap], but for unique values. *)
  val wrap_unique : access:'k Access.t -> 'a -> ('a, 'k) t

  (** Like [unwrap], but for unique values. *)
  val unwrap_unique : access:'k Access.t -> ('a, 'k) t -> 'a

  (** Like [wrap], but for once values. *)
  val wrap_once : access:'k Access.t -> 'a -> ('a, 'k) t

  (** Like [unwrap], but for once values. *)
  val unwrap_once : access:'k Access.t -> ('a, 'k) t -> 'a

  (** Like [wrap], but for once unique values. *)
  val wrap_once_unique : access:'k Access.t -> 'a -> ('a, 'k) t

  (** Like [unwrap], but for once unique values. *)
  val unwrap_once_unique : access:'k Access.t -> ('a, 'k) t -> 'a

  (** [unwrap_shared ~access t] returns the shared value of [t], which lives in the
      capsule ['k]. ['k] is always the current capsule. Since ['a] may have been shared
      with other domains, ['a] must cross portability. *)
  val unwrap_shared : 'a 'k. access:'k Access.t -> ('a, 'k) t -> 'a

  (** [create f] runs [f] within the capsule ['k] and returns a pointer to the result of
      [f]. *)
  val create : (unit -> 'a) -> ('a, 'k) t

  (** Like [create], but for once values. *)
  val create_once : (unit -> 'a) -> ('a, 'k) t

  (** [map ~password ~f t] applies [f] to the value of [p] within the capsule ['k] and
      returns a pointer to the result. *)
  val map : password:'k Password.t -> f:('a -> 'b) -> ('a, 'k) t -> ('b, 'k) t

  (** [both t1 t2] is a pointer to a pair of the values of [t1] and [t2]. *)
  val both : ('a, 'k) t -> ('b, 'k) t -> ('a * 'b, 'k) t

  (** [fst t] gives a pointer to the first value inside [t] *)
  val fst : ('a * 'b, 'k) t -> ('a, 'k) t

  (** [snd t] gives a pointer to the second value inside [t] *)
  val snd : ('a * 'b, 'k) t -> ('b, 'k) t

  (** [extract ~password ~f t] applies [f] to the value of [t] within the capsule ['k] and
      returns the result. The result is within ['k] so must be [portable] and is marked
      [contended]. *)
  val extract : password:'k Password.t -> f:('a -> 'b) -> ('a, 'k) t -> 'b

  (** [inject v] creates a pointer to a value [v] injected into the capsule ['k]. It's a
      specialization of [create] to values that are always [uncontended]. *)
  val inject : 'a 'k. 'a -> ('a, 'k) t

  (** [project t] returns the value of [t]. The result is within ['k], so is marked
      [contended]. The value is required to always be [portable], so unlike [extract],
      [project] does not require permission to access ['k]. This is safe because all
      accesses to the value happen only after it's marked [contended]. *)
  val project : 'a 'k. ('a, 'k) t -> 'a

  (** [project_shared ~key t] is like [project t], but since [t] is a capsule associated
      with a [key @ aliased global], the contents can be returned at [shared]. *)
  val project_shared : 'a 'k. key:'k Key.t -> ('a, 'k) t -> 'a

  (** [bind ~password ~f t] is [project (map ~password ~f t)]. *)
  val bind : password:'k Password.t -> f:('a -> ('b, 'j) t) -> ('a, 'k) t -> ('b, 'j) t

  (** [iter] is [extract] with result type specialized to [unit]. *)
  val iter : password:'k Password.t -> f:('a -> unit) -> ('a, 'k) t -> unit

  (** [map_shared ~password ~f t] applies [f] to the shared parts of [t] within the
      capsule ['k] and returns a pointer to the result. Since ['a] may have been shared
      with other domains, ['a] must cross portability. *)
  val map_shared
    : 'a 'b 'k.
    password:'k Password.Shared.t -> f:('a -> 'b) -> ('a, 'k) t -> ('b, 'k) t

  (** [extract_shared ~password ~f t] applies [f] to the shared parts of [t] within the
      capsule ['k] and returns the result. The result is within ['k] so must be portable
      and is marked contended. Since ['a] may have been shared with other domains, ['a]
      must cross portability. *)
  val extract_shared
    : 'a 'b 'k.
    password:'k Password.Shared.t -> f:('a -> 'b) -> ('a, 'k) t -> 'b

  module Shared : sig
    type ('a, 'k) data = ('a, 'k) t

    (** [('a, 'k) t] is the type of ['a]s in a "sub-capsule" of ['k] that has a [shared]
        view of ['k] and [uncontended] access to the sub-capsule enclosing ['a].

        Both read and write operations only require ['k Access.t @ shared]. However,
        unlike [('a, 'k) Data.t], [('a, 'k) Data.Shared.t] does not cross contention. *)
    type ('a, 'k) t

    (** [wrap ~access v] returns a pointer to the value [v], which lives in the
        sub-capsule of ['k]. ['k] is always the current capsule. *)
    val wrap : access:'k Access.t -> 'a -> ('a, 'k) t

    (** [unwrap ~access t] returns the value of [t], which lives in the sub-capsule of
        ['k]. ['k] is always the current capsule. *)
    val unwrap : access:'k Access.t -> ('a, 'k) t -> 'a

    (** A ['k Key.t @ global aliased] indicates that all capsules have permanent read-only
        access to ['k]. Therefore, [('a, 'k) t] can be safely [expose]d. *)
    val expose : key:'k Key.t -> ('a, 'k) t -> 'a

    (** [create f] runs [f] within the sub-capsule of ['k] and returns a pointer to the
        result. *)
    val create : (unit -> 'a) -> ('a, 'k) t

    (** [map ~password ~f t] applies [f] to the value of [p] within the sub-capsule of
        ['k] and returns a pointer to the result. *)
    val map : password:'k Password.Shared.t -> f:('a -> 'b) -> ('a, 'k) t -> ('b, 'k) t

    (** [both t1 t2] is a pointer to a pair of the values of [t1] and [t2]. *)
    val both : ('a, 'k) t -> ('b, 'k) t -> ('a * 'b, 'k) t

    (** [fst t] gives a pointer to the first value inside [t] *)
    val fst : ('a * 'b, 'k) t -> ('a, 'k) t

    (** [snd t] gives a pointer to the second value inside [t] *)
    val snd : ('a * 'b, 'k) t -> ('b, 'k) t

    (** [extract ~password ~f t] applies [f] to the value of [t] within the sub-capsule of
        ['k] and returns the result. The result has access to ['k] so must be [portable]
        and is marked [contended]. *)
    val extract : password:'k Password.Shared.t -> f:('a -> 'b) -> ('a, 'k) t -> 'b

    (** [inject v] is a pointer to an value [v] injected into the capsule ['k]. It's a
        specialization of [create] to values that are always [uncontended]. *)
    val inject : 'a 'k. 'a -> ('a, 'k) t

    (** [project t] returns the value of [t]. The result is within ['k], so must be
        [portable] and is marked [contended]. Since it's always [portable], unlike with
        [extract], we don't need exclusive access to ['k]: all accesses to the value
        happen only after it's marked [contended]. *)
    val project : 'a 'k. ('a, 'k) t -> 'a

    (** [bind ~password ~f t] is [project (map ~password ~f t)]. *)
    val bind
      :  password:'k Password.Shared.t
      -> f:('a -> ('b, 'j) t)
      -> ('a, 'k) t
      -> ('b, 'j) t

    (** [iter] is [extract] with result type specialized to [unit]. *)
    val iter : password:'k Password.Shared.t -> f:('a -> unit) -> ('a, 'k) t -> unit

    (** [map_into] is like [Capsule.map_shared] but returns a [Shared.t]. *)
    val map_into
      : 'a 'b 'k.
      password:'k Password.Shared.t -> f:('a -> 'b) -> ('a, 'k) data -> ('b, 'k) t

    (** Functions to work with [('a, 'k) t @ local]. *)
    module Local : sig
      (** [wrap ~access v] returns a pointer to the local value [v], which lives in the
          sub-capsule of ['k]. ['k] is always the current capsule. *)
      val wrap : access:'k Access.t -> 'a -> ('a, 'k) t

      (** [unwrap ~access t] returns the value of [t], which lives in the sub-capsule of
          ['k]. ['k] is always the current capsule. *)
      val unwrap : access:'k Access.t -> ('a, 'k) t -> 'a

      (** [create f] runs [f] within the sub-capsule of ['k] and returns a local pointer
          to the result. *)
      val create : (unit -> 'a) -> ('a, 'k) t

      (** [map ~pasword ~f t] applies [f] to the value of [p] within the sub-capsule of
          ['k] and returns a local pointer to the result. *)
      val map : password:'k Password.Shared.t -> f:('a -> 'b) -> ('a, 'k) t -> ('b, 'k) t

      (** [both t1 t2] is a pointer to a pair of the values of [t1] and [t2]. *)
      val both : ('a, 'k) t -> ('b, 'k) t -> ('a * 'b, 'k) t

      (** [fst t] gives a pointer to the first value inside [t] *)
      val fst : ('a * 'b, 'k) t -> ('a, 'k) t

      (** [snd t] gives a pointer to the second value inside [t] *)
      val snd : ('a * 'b, 'k) t -> ('b, 'k) t

      (** [extract ~pasword ~f t] applies [f] to the value of [t] within the sub-capsule
          of ['k] and returns the result. The result has access to ['k] so must be
          [portable] and is marked [contended]. *)
      val extract : password:'k Password.Shared.t -> f:('a -> 'b) -> ('a, 'k) t -> 'b

      (** [inject v] is a pointer to an value [v] injected into the capsule ['k]. It's a
          specialization of [create] to values that are always [uncontended]. *)
      val inject : 'a 'k. 'a -> ('a, 'k) t

      (** [project t] returns the value of [t]. The result is within ['k], so must be
          [portable] and is marked [contended]. Since it's always [portable], unlike with
          [extract], we don't need exclusive access to ['k]: all accesses to the value
          happen only after it's marked [contended]. *)
      val project : 'a 'k. ('a, 'k) t -> 'a

      (** [bind ~password ~f t] is [project (map ~password ~f t)]. *)
      val bind
        :  password:'k Password.Shared.t
        -> f:('a -> ('b, 'j) t)
        -> ('a, 'k) t
        -> ('b, 'j) t

      (** [iter] is [extract] with result type specialized to [unit]. *)
      val iter : password:'k Password.Shared.t -> f:('a -> unit) -> ('a, 'k) t -> unit

      (** [map_into] is like [Capsule.map_shared] but returns a [Shared.t]. *)
      val map_into
        : 'a 'b 'k.
        password:'k Password.Shared.t -> f:('a -> 'b) -> ('a, 'k) data -> ('b, 'k) t
    end
  end

  (** Functions to work with [('a, 'k) t @ local]. *)
  module Local : sig
    (** [wrap ~access v] returns a pointer to the local value [v], which lives in the
        capsule ['k]. ['k] is always the current capsule. *)
    val wrap : access:'k Access.t -> 'a -> ('a, 'k) t

    (** [unwrap ~access t] returns the value of [t], which lives in the capsule ['k]. ['k]
        is always the current capsule. *)
    val unwrap : access:'k Access.t -> ('a, 'k) t -> 'a

    (** Like [wrap], but for unique values. *)
    val wrap_unique : access:'k Access.t -> 'a -> ('a, 'k) t

    (** Like [unwrap], but for unique values. *)
    val unwrap_unique : access:'k Access.t -> ('a, 'k) t -> 'a

    (** Like [wrap], but for once values. *)
    val wrap_once : access:'k Access.t -> 'a -> ('a, 'k) t

    (** Like [unwrap], but for once values. *)
    val unwrap_once : access:'k Access.t -> ('a, 'k) t -> 'a

    (** [unwrap_shared ~access t] returns the shared value of [t], which lives in the
        capsule ['k]. ['k] is always the current capsule. Since ['a] may have been shared
        with other domains, ['a] must cross portability. *)
    val unwrap_shared : 'a 'k. access:'k Access.t -> ('a, 'k) t -> 'a

    (** [create f] runs [f] within the capsule ['k] and returns a local pointer to the
        result of [f]. *)
    val create : (unit -> 'a) -> ('a, 'k) t

    (** [map ~password ~f t] applies [f] to the value of [p] within the capsule ['k] and
        returns a local pointer to the result. *)
    val map : password:'k Password.t -> f:('a -> 'b) -> ('a, 'k) t -> ('b, 'k) t

    (** [both t1 t2] is a pointer to a pair of the values of [t1] and [t2]. *)
    val both : ('a, 'k) t -> ('b, 'k) t -> ('a * 'b, 'k) t

    (** [fst t] gives a pointer to the first value inside [t] *)
    val fst : ('a * 'b, 'k) t -> ('a, 'k) t

    (** [snd t] gives a pointer to the second value inside [t] *)
    val snd : ('a * 'b, 'k) t -> ('b, 'k) t

    (** [extract ~password ~f t] applies [f] to the value of [t] within the capsule ['k]
        and returns the result. The result is within ['k] so must be [portable] and is
        marked [contended]. *)
    val extract : password:'k Password.t -> f:('a -> 'b) -> ('a, 'k) t -> 'b

    (** [inject v] is a pointer to an value [v] injected into the capsule ['k]. It's a
        specialization of [create] to values that are always [uncontended]. *)
    val inject : 'a 'k. 'a -> ('a, 'k) t

    (** [project t] returns the value of [t]. The result is within ['k], so must be
        [portable] and is marked [contended]. Since it's always [portable], unlike with
        [extract], we don't need exclusive access to ['k]: all accesses to the value
        happen only after it's marked [contended]. *)
    val project : 'a 'k. ('a, 'k) t -> 'a

    (** [project_shared ~key t] is like [project t], but since [t] is a capsule associated
        with a [key @ aliased global], the contents can be returned at [shared]. *)
    val project_shared : 'a 'k. key:'k Key.t -> ('a, 'k) t -> 'a

    (** [bind ~password ~f t] is [project (map ~password ~f t)]. *)
    val bind : password:'k Password.t -> f:('a -> ('b, 'j) t) -> ('a, 'k) t -> ('b, 'j) t

    (** [iter] is [extract] with result type specialized to [unit]. *)
    val iter : password:'k Password.t -> f:('a -> unit) -> ('a, 'k) t -> unit

    (** [map_shared ~password ~f t] applies [f] to the shared parts of [t] within the
        capsule ['k] and returns a pointer to the result. Since ['a] may have been shared
        with other domains, ['a] must cross portability. *)
    val map_shared
      : 'a 'b 'k.
      password:'k Password.Shared.t -> f:('a -> 'b) -> ('a, 'k) t -> ('b, 'k) t

    (** [extract_shared ~password ~f t] applies [f] to the shared parts of [t] within the
        capsule ['k] and returns the result. The result is within ['k] so must be portable
        and is marked contended. Since ['a] may have been shared with other domains, ['a]
        must cross portability. *)
    val extract_shared
      : 'a 'b 'k.
      password:'k Password.Shared.t -> f:('a -> 'b) -> ('a, 'k) t -> 'b
  end
end
