(** Jane Street extensions add some new functions to the OCaml standard library. To remain
    compatible with upstream OCaml, we swap out their implementation when releasing the
    open-source version of our libraries.

    This file provides the common interface between the two different implementations. *)

(** Detect whether we are using the OCaml 5 runtime. *)
external runtime5 : unit -> bool @@ portable = "%runtime5"

(** Like {!ignore}, but takes a [contended] value. This is technically strictly stronger
    than [ignore], but changing [ignore] in place causes backwards compatibility issues
    due to type inference. *)
external ignore_contended : 'a @ contended -> unit = "%ignore"

external raise : exn -> 'a @ portable unique @@ portable = "%reraise"
external raise_notrace : exn -> 'a @ portable unique @@ portable = "%raise_notrace"

module Atomic : sig @@ portable
  type 'a t := 'a Atomic.t

  module Safe : sig @@ portable
    external make : 'a @ contended portable -> ('a t[@local_opt]) = "%makemutable"

    external make_contended
      :  'a @ contended portable
      -> 'a t
      = "caml_atomic_make_contended"

    external get : ('a t[@local_opt]) -> 'a @ contended portable = "%atomic_load"
    external set : ('a t[@local_opt]) -> 'a @ contended portable -> unit = "%atomic_set"

    external exchange
      :  ('a t[@local_opt])
      -> 'a @ contended portable
      -> 'a @ contended portable
      = "%atomic_exchange"

    external compare_and_set
      :  ('a t[@local_opt])
      -> 'a @ contended portable
      -> 'a @ contended portable
      -> bool
      = "%atomic_cas"

    external compare_exchange
      :  ('a t[@local_opt])
      -> 'a @ contended portable
      -> 'a @ contended portable
      -> 'a @ contended portable
      = "%atomic_compare_exchange"

    external add : (int t[@local_opt]) -> int -> unit = "%atomic_add"
    external sub : (int t[@local_opt]) -> int -> unit = "%atomic_sub"
    external logand : (int t[@local_opt]) -> int -> unit = "%atomic_land"
    external logor : (int t[@local_opt]) -> int -> unit = "%atomic_lor"
    external logxor : (int t[@local_opt]) -> int -> unit = "%atomic_lxor"

    external fetch_and_add
      :  (int t[@local_opt])
      -> int
      -> int
      @@ portable
      = "%atomic_fetch_add"
  end

  module Expert : sig
    external fenceless_get : ('a t[@local_opt]) -> 'a @ contended portable = "%field0"

    external fenceless_set
      :  ('a Atomic.t[@local_opt])
      -> 'a @ contended portable
      -> unit
      = "%setfield0"
  end
end

module Callback : sig
  module Safe : sig
    (** Like {!register}, but is safe to use in the presence of multiple domains. The
        provided value must be [portable] as registered values may be looked up from any
        domain. *)
    val register : string -> 'a @ portable -> unit @@ portable

    (** Like {!register_exception}, but is safe to use in the presence of multiple
        domains. The provided exception must be [portable] as registered exceptions may be
        looked up from any domain. *)
    val register_exception : string -> exn @ portable -> unit @@ portable
  end
end

module Domain : sig @@ portable
  type 'a t := 'a Domain.t

  module Safe : sig
    module DLS : sig
      module Access : sig
        type t : value mod external_ global many portable unique

        val for_initial_domain : t @@ nonportable
      end

      type 'a key : value mod contended portable = 'a Domain.DLS.key

      exception Encapsulated of string

      val access
        :  (Access.t -> 'a @ contended portable) @ local portable
        -> 'a @ contended portable
        @@ portable

      val new_key'
        :  ?split_from_parent:('a -> (Access.t -> 'a) @ portable) @ portable
        -> (Access.t -> 'a) @ portable
        -> 'a key
        @@ portable

      val new_key
        :  ?split_from_parent:('a -> (unit -> 'a) @ once portable) @ portable
        -> (unit -> 'a) @ portable
        -> 'a key
        @@ portable

      val get : Access.t -> 'a key -> 'a @@ portable
      val set : Access.t -> 'a key -> 'a -> unit @@ portable
    end

    val spawn : (unit -> 'a) @ portable -> 'a t @@ portable
    [@@alert
      unsafe_parallelism
        "This function is unsafe and should not be used in production code.\n\
         A safe interface for parallelism is forthcoming."]

    val spawn' : (DLS.Access.t -> 'a) @ portable -> 'a t @@ portable
    [@@alert
      unsafe_parallelism
        "This function is unsafe and should not be used in production code.\n\
         A safe interface for parallelism is forthcoming."]

    val at_exit : (unit -> unit) @ portable -> unit @@ portable
    val at_exit' : DLS.Access.t -> (unit -> unit) -> unit @@ portable
  end
end

module Ephemeron : sig @@ portable
  module K1 : sig
    module MakePortable (H : sig
      @@ portable
        include Hashtbl.HashedType
      end) : sig
      @@ portable
      include Ephemeron.S with type key = H.t
    end

    module MakeSeededPortable (H : sig
      @@ portable
        include Hashtbl.SeededHashedType
      end) : sig
      @@ portable
      include Ephemeron.SeededS with type key = H.t
    end
  end

  module K2 : sig
    module MakePortable
        (H1 : sig
         @@ portable
           include Hashtbl.HashedType
         end)
        (H2 : sig
         @@ portable
           include Hashtbl.HashedType
         end) : sig
      @@ portable
      include Ephemeron.S with type key = H1.t * H2.t
    end

    module MakeSeededPortable
        (H1 : sig
         @@ portable
           include Hashtbl.SeededHashedType
         end)
        (H2 : sig
         @@ portable
           include Hashtbl.SeededHashedType
         end) : sig
      @@ portable
      include Ephemeron.SeededS with type key = H1.t * H2.t
    end
  end

  module Kn : sig
    module MakePortable (H : sig
      @@ portable
        include Hashtbl.HashedType
      end) : sig
      @@ portable
      include Ephemeron.S with type key = H.t array
    end

    module MakeSeededPortable (H : sig
      @@ portable
        include Hashtbl.SeededHashedType
      end) : sig
      @@ portable
      include Ephemeron.SeededS with type key = H.t array
    end
  end
end

module Format : sig @@ portable
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
      :  (string -> int -> int -> unit) @ portable
      -> (unit -> unit) @ portable
      -> formatter Domain.Safe.DLS.key
  end
end

module Hashtbl : sig @@ portable
  (** Like [Make], but takes a portable [hash] function to portable [Hashtbl] operations. *)
  module MakePortable (H : sig
    @@ portable
      include Hashtbl.HashedType
    end) : sig
    @@ portable
    include Hashtbl.S with type key = H.t
  end

  (** Like [MakeSeeded], but takes a portable [seeded_hash] function to portable [Hashtbl]
      operations. *)
  module MakeSeededPortable (H : sig
    @@ portable
      include Hashtbl.SeededHashedType
    end) : sig
    @@ portable
    include Hashtbl.SeededS with type key = H.t
  end
end

module Map : sig @@ portable
  (** Like [Make], but takes a portable [compare] function to portable [Map] operations. *)
  module MakePortable (Ord : sig
    @@ portable
      include Map.OrderedType
    end) : sig
    @@ portable
    include Map.S with type key = Ord.t
  end
end

module Modes : sig
  module Global : sig
    type 'a t = { global_ global : 'a } [@@unboxed]
  end

  module Portable : sig
    type 'a t : value mod portable = { portable : 'a @@ portable }
    [@@unboxed] [@@unsafe_allow_any_mode_crossing]
  end

  module Contended : sig
    type 'a t : value mod contended = { contended : 'a @@ contended }
    [@@unboxed] [@@unsafe_allow_any_mode_crossing]
  end

  module Portended : sig
    type 'a t : value mod contended portable = { portended : 'a @@ contended portable }
    [@@unboxed] [@@unsafe_allow_any_mode_crossing]
  end

  module Aliased : sig
    type 'a t = { aliased : 'a @@ aliased } [@@unboxed]
  end
end

module Obj : sig @@ portable
  external magic_portable : ('a[@local_opt]) -> ('a[@local_opt]) @ portable = "%identity"

  external magic_uncontended
    :  ('a[@local_opt]) @ contended
    -> ('a[@local_opt])
    = "%identity"

  external magic_unique : ('a[@local_opt]) -> ('a[@local_opt]) @ unique = "%identity"
  external magic_many : ('a[@local_opt]) @ once -> ('a[@local_opt]) = "%identity"

  external magic_at_unique
    :  ('a[@local_opt]) @ unique
    -> ('b[@local_opt]) @ unique
    = "%identity"
end

module Printexc : sig @@ portable
  (** Submodule containing non-backwards-compatible functions which enforce thread safety
      via modes. *)
  module Safe : sig
    (** Like {!register_printer}, but is safe to use in the presence of multiple domains.
        The provided closure must be [portable] as exception printers may be called from
        any domain, not just the one that it's registered on. *)
    val register_printer : (exn -> string option) @ portable -> unit

    (** Like {!set_uncaught_exception_handler}, but is safe to use in the presence of
        multiple domains. The provided closure must be [portable] as exception handlers
        may be called from any domain, not just the one that it's registered on. *)
    val set_uncaught_exception_handler
      :  (exn -> Printexc.raw_backtrace -> unit) @ portable
      -> unit
  end
end

(** Submodule containing non-backwards-compatible functions which enforce thread safety
    via modes. *)
module Safe : sig @@ portable
  (** Like {!at_exit}, but can be called from any domain. The provided closure must be
      [portable] as it might be called from another domain. In particular, the primary
      domain may call {!exit}, thus calling the provided closure even if it came from a
      secondary domain. *)
  val at_exit : (unit -> unit) @ portable -> unit
end

module Set : sig @@ portable
  (** Like [Make], but takes a portable [compare] function to portable [Set] operations. *)
  module MakePortable (Ord : sig
    @@ portable
      include Set.OrderedType
    end) : sig
    @@ portable
    include Set.S with type elt = Ord.t
  end
end

module Sys : sig @@ portable
  (** Submodule containing non-backwards-compatible functions which enforce thread safety
      via modes. *)
  module Safe : sig
    (** Like {!signal}, but is safe to call in the presence of multiple domains. The
        provided [signal_behavior] must be [portable] as it is shared between all domains. *)
    external signal
      :  int
      -> Sys.signal_behavior @ portable
      -> Sys.signal_behavior @ portable
      = "caml_install_signal_handler"

    (** Like {!set_signal}, but is safe to call in the presence of multiple domains. The
        provided [signal_behavior] must be [portable] as it is shared between all domains. *)
    val set_signal : int -> Sys.signal_behavior @ portable -> unit
  end
end

module MoreLabels : sig @@ portable
  module Hashtbl : sig
    (** Like {!Make}, but takes a portable [hash] function to portable [Hashtbl]
        operations. *)
    module MakePortable (H : sig
      @@ portable
        include MoreLabels.Hashtbl.HashedType
      end) : sig
      @@ portable
      include
        MoreLabels.Hashtbl.S
        with type key = H.t
         and type 'a t = 'a Hashtbl.MakePortable(H).t
    end

    (** Like {!MakeSeeded}, but takes a portable [seeded_hash] function to portable
        [Hashtbl] operations. *)
    module MakeSeededPortable (H : sig
      @@ portable
        include MoreLabels.Hashtbl.SeededHashedType
      end) : sig
      @@ portable
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
      @@ portable
        include MoreLabels.Map.OrderedType
      end) : sig
      @@ portable
      include
        MoreLabels.Map.S with type key = Ord.t and type 'a t = 'a Map.MakePortable(Ord).t
    end
  end

  module Set : sig
    (** Like {!Make}, but takes a portable [compare] function to portable [Set]
        operations. *)
    module MakePortable (Ord : sig
      @@ portable
        include MoreLabels.Set.OrderedType
      end) : sig
      @@ portable
      include MoreLabels.Set.S with type elt = Ord.t and type t = Set.MakePortable(Ord).t
    end
  end
end
