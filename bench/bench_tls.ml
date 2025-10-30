open! Basement

module Bench (Storage : sig
    type 'a t

    val new_key : (unit -> 'a) -> 'a t
    val get : 'a. 'a t -> 'a
    val set : 'a. 'a t -> 'a -> unit
  end) =
struct
  let key = Storage.new_key (fun () -> ())
  let%bench "get" = ignore (Sys.opaque_identity (Storage.get key))
  let%bench "set" = ignore (Sys.opaque_identity (Storage.set key ()))
end

module%bench DLS = Bench (struct
    type 'a t = 'a Domain.Safe.DLS.key

    let new_key a = Domain.Safe.DLS.new_key a
    let get = Domain.Safe.DLS.get
    let set = Domain.Safe.DLS.set
  end)

module%bench TLS = Bench (struct
    type 'a t = 'a Domain.Safe.TLS.key

    let new_key a = Domain.Safe.TLS.new_key a
    let get = Domain.Safe.TLS.get
    let set = Domain.Safe.TLS.set
  end)
