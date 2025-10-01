[@@@alert "-unsafe_parallelism"]
[@@@alert "-unsafe_multidomain"]

external runtime5 : unit -> bool = "caml_is_runtime5_stub"
external poll : unit -> unit = "%identity"
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

    external compare_exchange
      :  'a t
      -> 'a
      -> 'a
      -> 'a
      = "caml_atomic_compare_exchange_stub"

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

    external compare_exchange
      :  'a t
      -> 'a
      -> 'a
      -> 'a
      = "caml_atomic_compare_exchange_stub"
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
  type id = int

  let cpu_relax = if runtime5 () then Domain.cpu_relax else fun () -> ()
  let self = if runtime5 () then (Domain.self :> unit -> id) else fun () -> 0

  let recommended_domain_count =
    if runtime5 () then Domain.recommended_domain_count else fun () -> 1
  ;;

  module Safe = struct
    module DLS = struct
      type 'a key = 'a Domain.DLS.key

      let[@inline] new_key ?split_from_parent f =
        let split_from_parent =
          match split_from_parent with
          | None -> None
          | Some g -> Some (fun x -> g x ())
        in
        Domain.DLS.new_key ?split_from_parent f
      ;;

      let get = Domain.DLS.get
      let set = Domain.DLS.set
    end

    let at_exit = Domain.at_exit
  end
end

module Backoff = struct
  type t = int

  let cpu_relax = Domain.cpu_relax
  let single_mask = Bool.to_int (Domain.recommended_domain_count () = 1) - 1
  let bits = 5
  let max_wait_log = 30
  let mask = (1 lsl bits) - 1

  let create ?(lower_wait_log = 4) ?(upper_wait_log = 17) () =
    assert (
      0 <= lower_wait_log
      && lower_wait_log <= upper_wait_log
      && upper_wait_log <= max_wait_log);
    (upper_wait_log lsl (bits * 2)) lor (lower_wait_log lsl bits) lor lower_wait_log
  ;;

  let get_upper_wait_log backoff = backoff lsr (bits * 2)
  let get_lower_wait_log backoff = (backoff lsr bits) land mask
  let get_wait_log backoff = backoff land mask

  let reset backoff =
    let lower_wait_log = get_lower_wait_log backoff in
    backoff land lnot mask lor lower_wait_log
  ;;

  let[@inline never] once backoff =
    let t = Random.bits () in
    let wait_log = get_wait_log backoff in
    let wait_mask = (1 lsl wait_log) - 1 in
    let t = ref (t land wait_mask land single_mask) in
    while 0 <= !t do
      Domain.cpu_relax ();
      t := !t - 1
    done;
    let upper_wait_log = get_upper_wait_log backoff in
    let wait_log = get_wait_log backoff in
    let next_wait_log = wait_log + Bool.to_int (wait_log < upper_wait_log) in
    backoff - wait_log + next_wait_log
  ;;

  let default = create ()
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
    let make_synchronized_formatter = Format.make_synchronized_formatter
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

  module Many = struct
    type 'a t = { many : 'a } [@@unboxed]
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
  external magic_unyielding : 'a -> 'a = "%identity"

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
