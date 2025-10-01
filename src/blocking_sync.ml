external yield : unit -> unit @@ portable = "caml_thread_yield"
external unsafe_key : unit -> 'k Capsule.Key.t @ unique @@ portable = "%unbox_unit"

external unsafe_password
  :  unit
  -> 'k Capsule.Password.t @ local
  @@ portable
  = "%unbox_unit"

type never = |

(* Like [Stdlib.raise], but [portable], and the value it never returns is also [portable unique] *)
external reraise
  : ('a : value_or_null).
  exn -> 'a @ portable unique
  @@ portable
  = "%reraise"

let reraise_void (type r : void) exn : r =
  match reraise exn with
  | (_ : never) -> .
;;

(* Like [Stdlib.Mutex], but [portable]. *)
module M = struct
  type t : value mod contended portable

  external create : unit -> t @@ portable = "caml_blocking_mutex_new"
  external lock : t @ local -> unit @@ portable = "caml_blocking_mutex_lock"
  external unlock : t @ local -> unit @@ portable = "caml_blocking_mutex_unlock"
end

module Mutex = struct
  type mutex : value mod contended portable =
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

  let[@inline never] poison_and_reraise
    : type (a : value_or_null) k. (k t -> exn:exn -> a @ unique) @ portable
    =
    fun t ~exn ->
    t.poisoned <- true;
    M.unlock t.mutex;
    reraise exn
  ;;

  let[@inline] with_lock
    : type (a : value_or_null) k.
      (k t
       -> f:(k Capsule.Password.t @ local -> a @ once unique) @ local once
       -> a @ once unique) @ portable
    =
    fun t ~f ->
    M.lock t.mutex;
    match t.poisoned with
    | true ->
      M.unlock t.mutex;
      reraise Poisoned
    | false ->
      let pw : k Capsule.Password.t = unsafe_password () in
      (match f pw with
       | x ->
         M.unlock t.mutex;
         x
       | exception exn -> poison_and_reraise t ~exn [@nontail])
  ;;

  let[@inline] with_key
    : type (a : value_or_null) k.
      (k t
       -> f:(k Capsule.Key.t @ unique -> #(a * k Capsule.Key.t) @ once unique)
          @ local once
       -> a @ once unique) @ portable
    =
    fun t ~f ->
    M.lock t.mutex;
    match t.poisoned with
    | true ->
      M.unlock t.mutex;
      reraise Poisoned
    | false ->
      let key : k Capsule.Key.t = unsafe_key () in
      (match f key with
       | #(x, _key) ->
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
      reraise_void Poisoned
    | false ->
      t.poisoned <- true;
      M.unlock t.mutex;
      unsafe_key ()
  ;;
end

module Condition = struct
  type 'k t : value mod contended portable

  external create : unit -> 'k t @@ portable = "caml_blocking_condition_new"
  external wait : 'k t -> M.t -> unit @@ portable = "caml_blocking_condition_wait"
  external signal : 'k t -> unit @@ portable = "caml_blocking_condition_signal"
  external broadcast : 'k t -> unit @@ portable = "caml_blocking_condition_broadcast"

  let[@inline] wait t ~(mutex : 'k Mutex.t) key =
    (* Check that the mutex is not poisoned. It's safe to do so without locking:
       either we hold the [key] because it's locked, or because it's poisoned. *)
    match mutex.poisoned with
    | true -> reraise_void Mutex.Poisoned
    | false ->
      wait t mutex.mutex;
      (* Check that the mutex wasn't poisoned again while we were waiting.
         If it was, we can't return the key. *)
      (match mutex.poisoned with
       | true -> reraise_void Mutex.Poisoned
       | false -> key)
  ;;
end
