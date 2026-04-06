open! Base
open Basement
open Or_null_shim.Export

let%test_unit "pattern-matching [Null]" =
  let x = Null in
  match x with
  | Null -> ()
  | This _ -> assert false
;;

let%test_unit "pattern-matching [This]" =
  let y = This 3 in
  match y with
  | This 3 -> ()
  | _ -> assert false
;;

external int_as_pointer : int -> int or_null = "%int_as_pointer"

let%test_unit "[Null] is represented as [0] pointer" =
  let n = int_as_pointer 0 in
  match n with
  | Null -> ()
  | _ -> assert false
;;

external int_as_int : int -> int or_null = "%opaque"

let%test_unit "[This x] and [x] share representation" =
  let m = int_as_int 5 in
  match m with
  | This 5 -> ()
  | This _ -> assert false
  | Null -> assert false
;;

let%test_unit "pattern-matching tuples containing [or_null]" =
  let x = Null, This "bar" in
  match x with
  | Null, This "foo" -> assert false
  | Null, This "bar" -> ()
  | _, This "bar" -> assert false
  | Null, _ -> assert false
  | _, _ -> assert false
;;

let%test_unit "functions" =
  let y a () = This a in
  let d = y 5 in
  match d () with
  | This 5 -> ()
  | _ -> assert false
;;

external to_bytes
  : ('a : value_or_null).
  'a -> int list -> bytes
  = "caml_output_value_to_bytes"

external from_bytes_unsafe
  : ('a : value_or_null).
  bytes -> int -> 'a
  = "caml_input_value_from_bytes"

let%test_unit ("marshaling [This]" [@tags "no-wasm"]) =
  let z = to_bytes (This "foo") [] in
  match from_bytes_unsafe z 0 with
  | This "foo" -> ()
  | This _ -> assert false
  | Null -> assert false
;;

let%test_unit ("marshaling [Null]" [@tags "no-wasm"]) =
  let w = to_bytes Null [] in
  match from_bytes_unsafe w 0 with
  | Null -> ()
  | This _ -> assert false
;;

external evil : 'a or_null -> 'a = "%opaque"

let%test_unit "[This x] and [x] share representation" =
  let e' = evil (This 4) in
  match e' with
  | 4 -> ()
  | _ -> assert false
;;

let%test_unit "Trying to create [This Null] results in [Null]" =
  let e = This (evil Null) in
  match e with
  | Null -> ()
  | This _ -> assert false
;;

let%test_unit "functions and pattern-matching" =
  let f a () =
    match a with
    | This x -> x ^ "bar"
    | Null -> "foo"
  in
  let g = f (This "xxx") in
  (match g () with
   | "xxxbar" -> ()
   | _ -> assert false);
  let h = f Null in
  match h () with
  | "foo" -> ()
  | _ -> assert false
;;

type 'a nref = { mutable v : 'a or_null }

let%test_unit "references containing [or_null]" =
  let x : string nref = { v = Null } in
  (match x.v with
   | Null -> ()
   | _ -> assert false);
  x.v <- This "foo";
  (match x.v with
   | This "foo" -> ()
   | _ -> assert false);
  x.v <- Null;
  match x.v with
  | Null -> ()
  | _ -> assert false
;;

external equal : ('a : value_or_null). 'a -> 'a -> bool = "%equal"
external compare : ('a : value_or_null). 'a -> 'a -> int = "%compare"

let%test_unit "equal" =
  assert (equal Null Null);
  assert (equal (This 4) (This 4));
  assert (not (equal Null (This 4)));
  assert (not (equal (This 8) Null));
  assert (not (equal (This 4) (This 5)))
;;

let%test_unit "compare" =
  assert (compare Null Null = 0);
  assert (compare (This 4) (This 4) = 0);
  assert (compare Null (This 4) < 0);
  assert (compare (This 8) Null > 0);
  assert (compare (This 4) (This 5) < 0);
  assert (compare (This "abc") (This "xyz") <> 0);
  assert (compare (This "xyz") (This "xyz") = 0)
;;

let%test_unit "obj_tag" = assert (Stdlib.Obj.tag (evil Null) = 1010)

external is_null : ('a : value_or_null). 'a -> bool = "%is_null"

let%test_unit "is_null" =
  assert (is_null Null);
  assert (not (is_null 4));
  assert (not (is_null "String"));
  assert (not (is_null (This 0)))
;;
