module type Dynamic = sig
  (** This module is reexported, with documentation, as [Base.Dynamic]; see that module
      for documentation on this interface. *)

  type 'a t

  val make : 'a. 'a -> 'a t
  val get : 'a. 'a t -> 'a
  val set_root : 'a. 'a t -> 'a -> unit
  val with_temporarily : 'a 'b. 'a t -> 'a -> f:(unit -> 'b) -> 'b
end
