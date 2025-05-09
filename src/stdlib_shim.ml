[@@@alert "-unsafe_parallelism"]
[@@@alert "-unsafe_multidomain"]

external runtime5 : unit -> bool = "caml_is_runtime5_stub"
external ignore_contended : 'a -> unit = "%ignore"
external raise : exn -> 'a = "%reraise"
external raise_notrace : exn -> 'a = "%raise_notrace"

let failwith s = raise (Failure s)

module Atomic = struct
  type 'a t = 'a Stdlib.Atomic.t

  module Local = struct
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

    let incr r = add r 1
    let decr r = sub r 1
  end

  module Contended = struct
    external get : 'a t -> 'a = "%atomic_load"
    external set : 'a t -> 'a -> unit = "caml_atomic_set_stub"
    external exchange : 'a t -> 'a -> 'a = "%atomic_exchange"
    external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas"
    external compare_exchange : 'a t -> 'a -> 'a -> 'a = "caml_atomic_compare_exchange_stub"
  end

  module Expert = struct
    external fenceless_get : 'a Atomic.t -> 'a = "%field0"
    external fenceless_set : 'a Atomic.t -> 'a -> unit = "%setfield0"

    module Contended = struct
      external fenceless_get : 'a Atomic.t -> 'a = "%field0"
      external fenceless_set : 'a Atomic.t -> 'a -> unit = "%setfield0"
    end
  end
end

module Callback = struct
  module Safe = struct
    let register = Callback.register
    let register_exception = Callback.register_exception
  end
end

module Domain = struct
  module Safe = struct
    module DLS = struct
      module Access = struct
        type t = Access

        let for_initial_domain = Access
      end

      type 'a key = 'a Domain.DLS.key

      exception Encapsulated of string

      let[@inline] access f =
        try f Access.Access with
        | exn ->
          let bt = Printexc.get_raw_backtrace () in
          let exn_string = Printexc.to_string exn in
          Printexc.raise_with_backtrace (Encapsulated exn_string) bt
      ;;

      let[@inline] new_key ?split_from_parent f =
        let split_from_parent =
          match split_from_parent with
          | None -> None
          | Some g -> Some (fun x -> g x ())
        in
        Domain.DLS.new_key ?split_from_parent f
      ;;

      let[@inline] new_key' ?split_from_parent f =
        let split_from_parent =
          match split_from_parent with
          | None -> None
          | Some g -> Some (fun x -> g x Access.Access)
        in
        let f () = f Access.Access in
        Domain.DLS.new_key ?split_from_parent f
      ;;

      let get Access.Access k = Domain.DLS.get k
      let set Access.Access k v = Domain.DLS.set k v
    end

    let spawn = Domain.spawn
    let[@inline] spawn' f = Domain.spawn (fun () -> f DLS.Access.Access)
    let at_exit = Domain.at_exit
    let[@inline] at_exit' DLS.Access.Access f = Domain.at_exit f
  end
end

module Ephemeron = struct
  module K1 = struct
    module MakePortable = Ephemeron.K1.Make
    module MakeSeededPortable = Ephemeron.K1.MakeSeeded
  end

  module K2 = struct
    module MakePortable = Ephemeron.K2.Make
    module MakeSeededPortable = Ephemeron.K2.MakeSeeded
  end

  module Kn = struct
    module MakePortable = Ephemeron.Kn.Make
    module MakeSeededPortable = Ephemeron.Kn.MakeSeeded
  end
end

module Format = struct
  module Safe = struct
    let get_std_formatter _ = Format.get_std_formatter ()
    let get_err_formatter _ = Format.get_err_formatter ()
    let get_str_formatter _ = Format.get_str_formatter ()
    let get_stdbuf _ = Format.get_stdbuf ()
    let make_synchronized_formatter f g = Format.make_synchronized_formatter f g
  end
end

module Hashtbl = struct
  module MakePortable = Hashtbl.Make
  module MakeSeededPortable = Hashtbl.MakeSeeded
end

module Map = struct
  module MakePortable = Map.Make
end

module Modes = struct
  module Global = struct
    type 'a t = { global : 'a } [@@unboxed]
  end

  module Portable = struct
    type 'a t = { portable : 'a } [@@unboxed]
  end

  module Contended = struct
    type 'a t = { contended : 'a } [@@unboxed]
  end

  module Portended = struct
    type 'a t = { portended : 'a } [@@unboxed]
  end

  module Aliased = struct
    type 'a t = { aliased : 'a } [@@unboxed]
  end
end

module MoreLabels = struct
  module Hashtbl = struct
    module MakePortable = MoreLabels.Hashtbl.Make
    module MakeSeededPortable = MoreLabels.Hashtbl.MakeSeeded
  end

  module Map = struct
    module MakePortable = MoreLabels.Map.Make
  end

  module Set = struct
    module MakePortable = MoreLabels.Set.Make
  end
end

module Obj = struct
  external magic_portable : 'a -> 'a = "%identity"
  external magic_uncontended : 'a -> 'a = "%identity"
  external magic_unique : 'a -> 'a = "%identity"
  external magic_many : 'a -> 'a = "%identity"
  external magic_at_unique : 'a -> 'b = "%identity"

  module Extension_constructor = struct
    let of_val = Stdlib.Obj.Extension_constructor.of_val
  end
end

module Printexc = struct
  module Safe = struct
    let register_printer = Printexc.register_printer
    let set_uncaught_exception_handler = Printexc.set_uncaught_exception_handler
  end
end

module Safe = struct
  let at_exit = at_exit
end

module Set = struct
  module MakePortable = Set.Make
end

module Sys = struct
  module Safe = struct
    external signal
      :  int
      -> Sys.signal_behavior
      -> Sys.signal_behavior
      = "caml_install_signal_handler"

    let set_signal = Sys.set_signal
  end
end
