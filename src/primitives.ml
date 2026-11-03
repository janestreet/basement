[@@@warning "-incompatible-with-upstream"]

module Int32_u = struct
  type t = int32_u

  external box : (t[@unboxed]) -> (int32[@local_opt]) @@ stateless = "%box_int32"
  external unbox : int32 @ local -> (t[@unboxed]) @@ stateless = "%unbox_int32"
end

module Int64_u = struct
  type t = int64_u

  external box : (t[@unboxed]) -> (int64[@local_opt]) @@ stateless = "%box_int64"
  external unbox : int64 @ local -> (t[@unboxed]) @@ stateless = "%unbox_int64"
end

module Nativeint_u = struct
  type t = nativeint_u

  external box : (t[@unboxed]) -> (nativeint[@local_opt]) @@ stateless = "%box_nativeint"
  external unbox : nativeint @ local -> (t[@unboxed]) @@ stateless = "%unbox_nativeint"
end

external box_bool
  :  (bool#[@unboxed])
  -> (bool[@local_opt])
  @@ stateless
  = "%int8_of_int8#"

let box_char c =
  let open struct
    external char_u_to_int : (char#[@unboxed]) -> int @@ stateless = "%int_of_int8#"
  end in
  let signed_int = char_u_to_int c in
  let unsigned_int = signed_int land ((1 lsl 8) - 1) in
  Char.chr unsigned_int
;;

external box_float : (float#[@unboxed]) -> (float[@local_opt]) @@ stateless = "%box_float"
external box_int : (int#[@unboxed]) -> (int[@local_opt]) @@ stateless = "%int_of_int#"

external unbox_bool
  :  (bool[@local_opt])
  -> (bool#[@unboxed])
  @@ stateless
  = "%int8#_of_int8"

external unbox_char
  :  (char[@local_opt])
  -> (char#[@unboxed])
  @@ stateless
  = "%int8#_of_int8"

external unbox_float : float @ local -> (float#[@unboxed]) @@ stateless = "%unbox_float"
external unbox_int : (int[@local_opt]) -> (int#[@unboxed]) @@ stateless = "%int#_of_int"
