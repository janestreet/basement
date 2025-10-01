type 'a global = { global : 'a } [@@unboxed]

module Access : sig
  type 'k t [@@immediate]
  type packed = P : 'k t -> packed [@@unboxed]
  type 'k boxed

  val box : 'k t -> 'k boxed
  val unbox : 'k boxed -> 'k t

  (* Can break soundness. *)
  val unsafe_mk : unit -> 'k t
  val equality_witness : 'k t -> 'j t -> ('k, 'j) Type.eq
end = struct
  type inner [@@immediate]

  external mk_inner : unit -> inner = "%identity"

  type dummy
  type 'k t = T : inner -> dummy t [@@unboxed]
  type packed = P : 'k t -> packed [@@unboxed]
  type 'k boxed = Box : dummy boxed

  external unsafe_rebrand : 'k t -> 'j t = "%identity"
  external unsafe_rebrand_boxed : 'k boxed -> 'j boxed = "%identity"

  let[@inline] unsafe_mk (type k) () : k t = unsafe_rebrand (T (mk_inner ()))
  let box _ = unsafe_rebrand_boxed Box
  let unbox _ = unsafe_mk ()

  let[@inline] equality_witness (type k j) (T _ : k t) (T _ : j t) : (k, j) Type.eq =
    Type.Equal
  ;;
end

let[@inline] current () = Access.P (Access.unsafe_mk ())

type initial

let initial = Access.(box (unsafe_mk ()))

let access_initial =
  if Stdlib_shim.runtime5 ()
  then
    fun [@inline] f ->
    if Stdlib.Domain.is_main_domain ()
    then f (Some Access.(box (unsafe_mk ())))
    else f None
  else fun [@inline] f -> f (Some Access.(box (unsafe_mk ())))
;;

module Password : sig
  type 'k t [@@immediate]
  type 'k boxed

  (* Can break the soundness of the API. *)
  val unsafe_mk : unit -> 'k t
  val box : 'k t -> 'k boxed
  val unbox : 'k boxed -> 'k t

  module Shared : sig
    type 'k t [@@immediate]
    type 'k boxed

    val box : 'k t -> 'k boxed
    val unbox : 'k boxed -> 'k t
    val borrow : 'a 'k. 'k t -> ('k t -> 'a) -> 'a

    (* Can break the soundness of the API. *)
    val unsafe_mk : unit -> 'k t
  end

  val shared : 'k t -> 'k Shared.t
  val with_current : 'a 'k. 'k Access.t -> ('k t -> 'a) -> 'a
end = struct
  type 'k t [@@immediate]
  type 'k boxed = unit

  external unsafe_mk : unit -> 'k t = "%identity"

  let box _ = ()
  let unbox () = unsafe_mk ()

  module Shared = struct
    type 'k t [@@immediate]
    type 'k boxed = unit

    external unsafe_mk : unit -> 'k t = "%identity"

    let box _ = ()
    let unbox () = unsafe_mk ()
    let[@inline] borrow _ f = f (unsafe_mk ())
  end

  external shared : 'k t -> 'k Shared.t = "%identity"

  let[@inline] with_current _ f = f (unsafe_mk ()) [@nontail]
end

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
    let[@inline] wrap_once ~access:_ t = unsafe_mk_once t
    let[@inline] unwrap_once ~access:_ t = unsafe_get_once t
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
  type 'k boxed

  val unsafe_mk : unit -> 'k t
  val box : 'k t -> 'k boxed
  val unbox : 'k boxed -> 'k t
  val box_aliased : 'k t -> 'k boxed
  val unbox_aliased : 'k boxed -> 'k t
  val with_password : 'a 'k. 'k t -> f:('k Password.t -> 'a) -> 'a * 'k t
  val with_password_local : 'a 'k. 'k t -> f:('k Password.t -> 'a) -> 'a
  val with_password_shared : 'a 'k. 'k t -> f:('k Password.Shared.t -> 'a) -> 'a
  val with_password_shared_local : 'a 'k. 'k t -> f:('k Password.Shared.t -> 'a) -> 'a
  val access : 'a 'k. 'k t -> f:('k Access.t -> 'a) -> 'a * 'k t
  val access_local : 'a 'k. 'k t -> f:('k Access.t -> 'a) -> 'a * 'k t
  val access_shared : 'a 'k. 'k t -> f:('k Access.t -> 'a) -> 'a
  val access_shared_local : 'a 'k. 'k t -> f:('k Access.t -> 'a) -> 'a
  val globalize_unique : 'k t -> 'k t
  val destroy : 'k t -> 'k Access.t
end = struct
  type 'k t [@@immediate]
  type packed = P : 'k t -> packed [@@unboxed]
  type 'k boxed = unit

  external unsafe_mk : unit -> 'k t = "%identity"

  let box _ = ()
  let unbox () = unsafe_mk ()
  let box_aliased _ = ()
  let unbox_aliased () = unsafe_mk ()

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

  let[@inline] globalize_unique _ = unsafe_mk ()
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
