external runtime5 : unit -> bool @@ portable = "%runtime5"
external ignore_contended : 'a @ contended -> unit = "%ignore"
external raise : exn -> 'a @ portable unique @@ portable = "%reraise"
external raise_notrace : exn -> 'a @ portable unique @@ portable = "%raise_notrace"

module Atomic = struct
  module Safe = struct
    (* Expose functions as externals for locality support and extra performance. *)
    external make
      :  'a @ contended portable
      -> ('a Atomic.t[@local_opt])
      @@ portable
      = "%makemutable"

    external make_contended
      :  'a @ contended portable
      -> 'a Atomic.t
      @@ portable
      = "caml_atomic_make_contended"

    external get
      :  ('a Atomic.t[@local_opt])
      -> 'a @ contended portable
      @@ portable
      = "%atomic_load"

    external set
      :  ('a Atomic.t[@local_opt])
      -> 'a @ contended portable
      -> unit
      @@ portable
      = "%atomic_set"

    external exchange
      :  ('a Atomic.t[@local_opt])
      -> 'a @ contended portable
      -> 'a @ contended portable
      @@ portable
      = "%atomic_exchange"

    external compare_and_set
      :  ('a Atomic.t[@local_opt])
      -> 'a @ contended portable
      -> 'a @ contended portable
      -> bool
      @@ portable
      = "%atomic_cas"

    external compare_exchange
      :  ('a Atomic.t[@local_opt])
      -> 'a @ contended portable
      -> 'a @ contended portable
      -> 'a @ contended portable
      @@ portable
      = "%atomic_compare_exchange"

    external add : (int Atomic.t[@local_opt]) -> int -> unit @@ portable = "%atomic_add"
    external sub : (int Atomic.t[@local_opt]) -> int -> unit @@ portable = "%atomic_sub"

    external logand
      :  (int Atomic.t[@local_opt])
      -> int
      -> unit
      @@ portable
      = "%atomic_land"

    external logor : (int Atomic.t[@local_opt]) -> int -> unit @@ portable = "%atomic_lor"

    external logxor
      :  (int Atomic.t[@local_opt])
      -> int
      -> unit
      @@ portable
      = "%atomic_lxor"

    external fetch_and_add
      :  (int Atomic.t[@local_opt])
      -> int
      -> int
      @@ portable
      = "%atomic_fetch_add"
  end

  module Expert = struct
    external fenceless_get
      :  ('a Atomic.t[@local_opt])
      -> 'a @ contended portable
      @@ portable
      = "%field0"

    external fenceless_set
      :  ('a Atomic.t[@local_opt])
      -> 'a @ contended portable
      -> unit
      @@ portable
      = "%setfield0"
  end
end

module Callback = struct
  module Safe = Callback.Safe
end

module Domain = struct
  module Safe = struct
    include Domain.Safe

    module DLS = struct
      include Domain.Safe.DLS

      external magic_many__portable
        :  'a @ once portable
        -> 'a @ portable
        @@ portable
        = "%identity"

      let[@inline] new_key ?split_from_parent f =
        let split_from_parent =
          match split_from_parent with
          | None -> None
          | Some split_from_parent ->
            let split_from_parent x =
              let mk = split_from_parent x in
              magic_many__portable mk
            in
            Some split_from_parent
        in
        new_key ?split_from_parent f
      ;;
    end
  end
end

module Ephemeron = struct
  module K1 = Ephemeron.K1
  module K2 = Ephemeron.K2
  module Kn = Ephemeron.Kn
end

module Format = struct
  module Safe = Format.Safe
end

module Hashtbl = struct
  module MakePortable = Hashtbl.MakePortable
  module MakeSeededPortable = Hashtbl.MakeSeededPortable
end

module Map = struct
  module MakePortable = Map.MakePortable
end

module Modes = struct
  include Modes

  module Aliased = struct
    type 'a t = { aliased : 'a @@ aliased } [@@unboxed]
  end
end

module MoreLabels = struct
  module Hashtbl = struct
    module MakePortable = MoreLabels.Hashtbl.MakePortable
    module MakeSeededPortable = MoreLabels.Hashtbl.MakeSeededPortable
  end

  module Map = struct
    module MakePortable = MoreLabels.Map.MakePortable
  end

  module Set = struct
    module MakePortable = MoreLabels.Set.MakePortable
  end
end

module Obj = struct
  external magic_portable
    :  ('a[@local_opt])
    -> ('a[@local_opt]) @ portable
    @@ portable
    = "%identity"

  external magic_uncontended
    :  ('a[@local_opt]) @ contended
    -> ('a[@local_opt])
    @@ portable
    = "%identity"

  external magic_unique
    :  ('a[@local_opt])
    -> ('a[@local_opt]) @ unique
    @@ portable
    = "%identity"

  external magic_many
    :  ('a[@local_opt]) @ once
    -> ('a[@local_opt])
    @@ portable
    = "%identity"

  external magic_at_unique
    :  ('a[@local_opt]) @ unique
    -> ('b[@local_opt]) @ unique
    @@ portable
    = "%identity"
end

module Printexc = struct
  module Safe = Printexc.Safe
end

module Safe = Safe

module Set = struct
  module MakePortable = Set.MakePortable
end

module Sys = struct
  module Safe = Sys.Safe
end
