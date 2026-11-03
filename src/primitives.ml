module Int32_u = struct
  type t = int32

  external box : t -> int32 = "%identity"
  external unbox : int32 -> t = "%identity"
end

module Int64_u = struct
  type t = int64

  external box : t -> int64 = "%identity"
  external unbox : int64 -> t = "%identity"
end

module Nativeint_u = struct
  type t = nativeint

  external box : t -> nativeint = "%identity"
  external unbox : nativeint -> t = "%identity"
end

external box_bool : bool -> bool = "%identity"
external box_char : char -> char = "%identity"
external box_float : float -> float = "%identity"
external box_int : int -> int = "%identity"
external unbox_bool : bool -> bool = "%identity"
external unbox_char : char -> char = "%identity"
external unbox_float : float -> float = "%identity"
external unbox_int : int -> int = "%identity"
