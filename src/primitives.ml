[@@@warning "-incompatible-with-upstream"]

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
external box_int32 : (int32#[@unboxed]) -> (int32[@local_opt]) @@ stateless = "%box_int32"
external box_int64 : (int64#[@unboxed]) -> (int64[@local_opt]) @@ stateless = "%box_int64"

external box_nativeint
  :  (nativeint#[@unboxed])
  -> (nativeint[@local_opt])
  @@ stateless
  = "%box_nativeint"

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
external unbox_int32 : int32 @ local -> (int32#[@unboxed]) @@ stateless = "%unbox_int32"
external unbox_int64 : int64 @ local -> (int64#[@unboxed]) @@ stateless = "%unbox_int64"

external unbox_nativeint
  :  nativeint @ local
  -> (nativeint#[@unboxed])
  @@ stateless
  = "%unbox_nativeint"
