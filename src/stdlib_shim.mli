(** Jane Street extensions add some new functions to the OCaml standard library. To remain
    compatible with upstream OCaml, we swap out their implementation when releasing the
    open-source version of our libraries.

    This file provides the common interface between the two different implementations. *)

(** Detect whether we are using the OCaml 5 runtime. *)
external runtime5 : unit -> bool = "caml_is_runtime5_stub"

(** Like {!ignore}, but takes a [contended] value. This is technically strictly stronger
    than [ignore], but changing [ignore] in place causes backwards compatibility issues
    due to type inference. *)
external ignore_contended : 'a -> unit = "%ignore"

external raise : exn -> 'a = "%reraise"
external raise_notrace : exn -> 'a = "%raise_notrace"
val failwith : string -> 'a

module Atomic : sig
  type 'a t := 'a Stdlib.Atomic.t

  module Local : sig
    external make : 'a -> ('a t[@local_opt]) = "%makemutable"
    external make_contended : 'a -> ('a t[@local_opt]) = "caml_atomic_make_contended"
    external get : 'a t -> 'a = "%atomic_load"
    external set : 'a t -> 'a -> unit = "caml_atomic_set_stub"
    external exchange : 'a t -> 'a -> 'a = "%atomic_exchange"
    external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas"
    external compare_exchange : 'a t -> 'a -> 'a -> 'a = "caml_atomic_compare_exchange_stub"
    external fetch_and_add : int t -> int -> int = "%atomic_fetch_add"
    external add : int t -> int -> unit = "caml_atomic_add_stub"
    external sub : int t -> int -> unit = "caml_atomic_sub_stub"
    external logand : int t -> int -> unit = "caml_atomic_land_stub"
    external logor : int t -> int -> unit = "caml_atomic_lor_stub"
    external logxor : int t -> int -> unit = "caml_atomic_lxor_stub"
    val incr : int t -> unit
    val decr : int t -> unit
  end

  module Contended : sig
    external get : 'a. 'a t -> 'a = "%atomic_load"
    external set : 'a. 'a t -> 'a -> unit = "caml_atomic_set_stub"
    external exchange : 'a. 'a t -> 'a -> 'a = "%atomic_exchange"
    external compare_and_set : 'a. 'a t -> 'a -> 'a -> bool = "%atomic_cas"
    external compare_exchange : 'a. 'a t -> 'a -> 'a -> 'a = "caml_atomic_compare_exchange_stub"
  end

  module Expert : sig
    external fenceless_get : 'a t -> 'a = "%field0"
    external fenceless_set : 'a t -> 'a -> unit = "%setfield0"

    module Contended : sig
      external fenceless_get : 'a. 'a t -> 'a = "%field0"
      external fenceless_set : 'a. 'a t -> 'a -> unit = "%setfield0"
    end
  end
end

module Callback : sig
  module Safe : sig
    (** Like {!register}, but is safe to use in the presence of multiple domains. The
        provided value must be [portable] as registered values may be looked up from any
        domain. *)
    val register : string -> 'a -> unit

    (** Like {!register_exception}, but is safe to use in the presence of multiple
        domains. The provided exception must be [portable] as registered exceptions may be
        looked up from any domain. *)
    val register_exception : string -> exn -> unit
  end
end

module Domain : sig
  type 'a t := 'a Domain.t

  module Safe : sig
    module DLS : sig
      module Access : sig
        type t

        val for_initial_domain : t
      end

      type 'a key = 'a Domain.DLS.key

      exception Encapsulated of string

      val access : (Access.t -> 'a) -> 'a

      val new_key'
        :  ?split_from_parent:('a -> Access.t -> 'a)
        -> (Access.t -> 'a)
        -> 'a key

      val new_key : ?split_from_parent:('a -> unit -> 'a) -> (unit -> 'a) -> 'a key
      val get : Access.t -> 'a key -> 'a
      val set : Access.t -> 'a key -> 'a -> unit
    end

    val spawn : (unit -> 'a) -> 'a t
    [@@alert
      unsafe_parallelism
        "This function is unsafe and should not be used in production code.\n\
         A safe interface for parallelism is forthcoming."]

    val spawn' : (DLS.Access.t -> 'a) -> 'a t
    [@@alert
      unsafe_parallelism
        "This function is unsafe and should not be used in production code.\n\
         A safe interface for parallelism is forthcoming."]

    val at_exit : (unit -> unit) -> unit
    val at_exit' : DLS.Access.t -> (unit -> unit) -> unit
  end
end

module Ephemeron : sig
  module K1 : sig
    module MakePortable (H : sig
        include Hashtbl.HashedType
      end) : sig
      include Ephemeron.S with type key = H.t
    end

    module MakeSeededPortable (H : sig
        include Hashtbl.SeededHashedType
      end) : sig
      include Ephemeron.SeededS with type key = H.t
    end
  end

  module K2 : sig
    module MakePortable
        (H1 : sig
           include Hashtbl.HashedType
         end)
        (H2 : sig
           include Hashtbl.HashedType
         end) : sig
      include Ephemeron.S with type key = H1.t * H2.t
    end

    module MakeSeededPortable
        (H1 : sig
           include Hashtbl.SeededHashedType
         end)
        (H2 : sig
           include Hashtbl.SeededHashedType
         end) : sig
      include Ephemeron.SeededS with type key = H1.t * H2.t
    end
  end

  module Kn : sig
    module MakePortable (H : sig
        include Hashtbl.HashedType
      end) : sig
      include Ephemeron.S with type key = H.t array
    end

    module MakeSeededPortable (H : sig
        include Hashtbl.SeededHashedType
      end) : sig
      include Ephemeron.SeededS with type key = H.t array
    end
  end
end

module Format : sig
  type formatter := Format.formatter

  (** Submodule containing non-backwards-compatible functions which enforce thread safety
      via modes. *)
  module Safe : sig
    (** Like {!get_std_formatter}, but can be called from any domain.

        An additional [Domain.Safe.DLS.Access.t] argument is taken, which acts as a
        witness that the returned [formatter] does not escape the current domain. This is
        necessary as the [formatter] may contain functions which close over other data in
        the current domain. *)
    val get_std_formatter : Domain.Safe.DLS.Access.t -> formatter

    (** Like {!get_err_formatter}, but can be called from any domain.

        An additional [Domain.Safe.DLS.Access.t] argument is taken, which acts as a
        witness that the returned [formatter] does not escape the current domain. This is
        necessary as the [formatter] may contain functions which close over other data in
        the current domain. *)
    val get_err_formatter : Domain.Safe.DLS.Access.t -> formatter

    (** Like {!get_str_formatter}, but can be called from any domain.

        An additional [Domain.Safe.DLS.Access.t] argument is taken, which acts as a
        witness that the returned [formatter] does not escape the current domain. This is
        necessary as the [formatter] may contain functions which close over other data in
        the current domain. *)
    val get_str_formatter : Domain.Safe.DLS.Access.t -> formatter

    (** Like {!get_stdbuf}, but can be called from any domain.

        An additional [Domain.Safe.DLS.Access.t] argument is taken, which acts as a
        witness that the returned [Buffer.t] does not escape the current domain. This is
        necessary as the [Buffer.t] is mutable data which is not safe to share between
        domains. *)
    val get_stdbuf : Domain.Safe.DLS.Access.t -> Buffer.t

    (** Like {!make_synchronized_formatter}, but can be called from any domain.

        The provided closures must be [portable] as they will be called from other domains
        that access the returned [Domain.Safe.DLS.key]. *)
    val make_synchronized_formatter
      :  (string -> int -> int -> unit)
      -> (unit -> unit)
      -> formatter Domain.Safe.DLS.key
  end
end

module Hashtbl : sig
  (** Like [Make], but takes a portable [hash] function to portable [Hashtbl] operations. *)
  module MakePortable (H : sig
      include Hashtbl.HashedType
    end) : sig
    include Hashtbl.S with type key = H.t
  end

  (** Like [MakeSeeded], but takes a portable [seeded_hash] function to portable [Hashtbl]
      operations. *)
  module MakeSeededPortable (H : sig
      include Hashtbl.SeededHashedType
    end) : sig
    include Hashtbl.SeededS with type key = H.t
  end
end

module Map : sig
  (** Like [Make], but takes a portable [compare] function to portable [Map] operations. *)
  module MakePortable (Ord : sig
      include Map.OrderedType
    end) : sig
    include Map.S with type key = Ord.t
  end
end

module Modes : sig
  module Global : sig
    type 'a t = { global : 'a [@globalized] } [@@unboxed]
  end

  module Portable : sig
    type 'a t = { portable : 'a } [@@unboxed]
  end

  module Contended : sig
    type 'a t = { contended : 'a } [@@unboxed]
  end

  module Portended : sig
    type 'a t = { portended : 'a } [@@unboxed]
  end

  module Aliased : sig
    type 'a t = { aliased : 'a } [@@unboxed]
  end
end

module Obj : sig
  external magic_portable : ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
  external magic_uncontended : ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
  external magic_unique : ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
  external magic_many : ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
  external magic_at_unique : ('a[@local_opt]) -> ('b[@local_opt]) = "%identity"

  module Extension_constructor : sig
    val of_val : 'a -> extension_constructor
  end
end

module Printexc : sig
  (** Submodule containing non-backwards-compatible functions which enforce thread safety
      via modes. *)
  module Safe : sig
    (** Like {!register_printer}, but is safe to use in the presence of multiple domains.
        The provided closure must be [portable] as exception printers may be called from
        any domain, not just the one that it's registered on. *)
    val register_printer : (exn -> string option) -> unit

    (** Like {!set_uncaught_exception_handler}, but is safe to use in the presence of
        multiple domains. The provided closure must be [portable] as exception handlers
        may be called from any domain, not just the one that it's registered on. *)
    val set_uncaught_exception_handler : (exn -> Printexc.raw_backtrace -> unit) -> unit
  end
end

(** Submodule containing non-backwards-compatible functions which enforce thread safety
    via modes. *)
module Safe : sig
  (** Like {!at_exit}, but can be called from any domain. The provided closure must be
      [portable] as it might be called from another domain. In particular, the primary
      domain may call {!exit}, thus calling the provided closure even if it came from a
      secondary domain. *)
  val at_exit : (unit -> unit) -> unit
end

module Set : sig
  (** Like [Make], but takes a portable [compare] function to portable [Set] operations. *)
  module MakePortable (Ord : sig
      include Set.OrderedType
    end) : sig
    include Set.S with type elt = Ord.t
  end
end

module Sys : sig
  (** Submodule containing non-backwards-compatible functions which enforce thread safety
      via modes. *)
  module Safe : sig
    (** Like {!signal}, but is safe to call in the presence of multiple domains. The
        provided [signal_behavior] must be [portable] as it is shared between all domains. *)
    external signal
      :  int
      -> Sys.signal_behavior
      -> Sys.signal_behavior
      = "caml_install_signal_handler"

    (** Like {!set_signal}, but is safe to call in the presence of multiple domains. The
        provided [signal_behavior] must be [portable] as it is shared between all domains. *)
    val set_signal : int -> Sys.signal_behavior -> unit
  end
end

module MoreLabels : sig
  module Hashtbl : sig
    (** Like {!Make}, but takes a portable [hash] function to portable [Hashtbl]
        operations. *)
    module MakePortable (H : sig
        include MoreLabels.Hashtbl.HashedType
      end) : sig
      include
        MoreLabels.Hashtbl.S
        with type key = H.t
         and type 'a t = 'a Hashtbl.MakePortable(H).t
    end

    (** Like {!MakeSeeded}, but takes a portable [seeded_hash] function to portable
        [Hashtbl] operations. *)
    module MakeSeededPortable (H : sig
        include MoreLabels.Hashtbl.SeededHashedType
      end) : sig
      include
        MoreLabels.Hashtbl.SeededS
        with type key = H.t
         and type 'a t = 'a Hashtbl.MakeSeededPortable(H).t
    end
  end

  module Map : sig
    (** Like {!Make}, but takes a portable [compare] function to portable [Map]
        operations. *)
    module MakePortable (Ord : sig
        include MoreLabels.Map.OrderedType
      end) : sig
      include
        MoreLabels.Map.S with type key = Ord.t and type 'a t = 'a Map.MakePortable(Ord).t
    end
  end

  module Set : sig
    (** Like {!Make}, but takes a portable [compare] function to portable [Set]
        operations. *)
    module MakePortable (Ord : sig
        include MoreLabels.Set.OrderedType
      end) : sig
      include MoreLabels.Set.S with type elt = Ord.t and type t = Set.MakePortable(Ord).t
    end
  end
end
