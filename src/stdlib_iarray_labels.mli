(** Operations on immutable arrays. This module mirrors the API of [Array], but omits
    functions that assume mutability; in addition to obviously mutating functions, it
    omits [copy] along with the functions [make], [create_float], and [make_matrix] that
    produce all-constant arrays. The exception is the sorting functions, which are given a
    copying API to replace the in-place one. *)

type +'a t

(** An alias for the type of immutable arrays. *)
type 'a iarray = 'a t

external unsafe_of_array : 'a array -> 'a iarray = "%identity"

(** Return the length (number of elements) of the given immutable array. *)
external length : 'a iarray -> int = "%array_length"

(** [get a n] returns the element number [n] of immutable array [a]. The first element has
    number 0. The last element has number [length a - 1]. You can also write [a.:(n)]
    instead of [get a n].

    @raise Invalid_argument if [n] is outside the range 0 to [(length a - 1)]. *)
external get : ('a iarray[@local_opt]) -> int -> ('a[@local_opt]) = "%array_safe_get"

(** A synonym for [get]. *)
external ( .:() ) : ('a iarray[@local_opt]) -> int -> ('a[@local_opt]) = "%array_safe_get"

(** [init n ~f] returns a fresh immutable array of length [n], with element number [i]
    initialized to the result of [f i]. In other terms, [init n ~f] tabulates the results
    of [f] applied to the integers [0] to [n-1].

    @raise Invalid_argument
      if [n < 0] or [n > Sys.max_array_length]. If the return type of [f] is [float], then
      the maximum size is only [Sys.max_array_length / 2]. *)
val init : int -> f:(int -> 'a) -> 'a iarray

(** The locally-allocating version of [init]. *)
val init_local : 'a. int -> f:(int -> 'a) -> 'a iarray

(** [append v1 v2] returns a fresh immutable array containing the concatenation of the
    immutable arrays [v1] and [v2].
    @raise Invalid_argument if [length v1 + length v2 > Sys.max_array_length]. *)
val append : 'a iarray -> 'a iarray -> 'a iarray

(** The locally-allocating version of [append]. *)
val append_local : 'a iarray -> 'a iarray -> 'a iarray

(** Same as {!append}, but concatenates a list of immutable arrays. *)
val concat : 'a iarray list -> 'a iarray

(** The locally-allocating version of [concat]. *)
val concat_local : 'a iarray list -> 'a iarray

(** [sub a ~pos ~len] returns a fresh immutable array of length [len], containing the
    elements number [pos] to [pos + len - 1] of immutable array [a]. This creates a copy
    of the selected portion of the immutable array.

    @raise Invalid_argument
      if [pos] and [len] do not designate a valid subarray of [a]; that is, if [pos < 0],
      or [len < 0], or [pos + len > length a]. *)
val sub : 'a iarray -> pos:int -> len:int -> 'a iarray

(** The locally-allocating version of [sub]. *)
val sub_local : 'a iarray -> pos:int -> len:int -> 'a iarray

(** [to_list a] returns the list of all the elements of [a]. *)
val to_list : 'a iarray -> 'a list

(** The locally-allocating version of []. *)
val to_list_local : 'a iarray -> 'a list

(** [of_list l] returns a fresh immutable array containing the elements of [l].

    @raise Invalid_argument if the length of [l] is greater than [Sys.max_array_length]. *)
val of_list : 'a list -> 'a iarray

(** The locally-allocating version of [of_list]. *)
val of_list_local : 'a list -> 'a iarray

(** {1 Converting to and from mutable arrays} *)

(** [to_array a] returns a mutable copy of the immutable array [a]; that is, a fresh
    (mutable) array containing the same elements as [a] *)
val to_array : 'a iarray -> 'a array

(** [of_array ma] returns an immutable copy of the mutable array [ma]; that is, a fresh
    immutable array containing the same elements as [ma] *)
val of_array : 'a array -> 'a iarray

(** {1 Iterators} *)

(** [iter ~f a] applies function [f] in turn to all the elements of [a]. It is equivalent
    to [f a.:(0); f a.:(1); ...; f a.:(length a - 1); ()]. *)
val iter : f:('a -> unit) -> 'a iarray -> unit

(** The locally-scoped version of [iter]. *)
val iter_local : f:('a -> unit) -> 'a iarray -> unit

(** Same as {!iter}, but the function is applied to the index of the element as first
    argument, and the element itself as second argument. *)
val iteri : f:(int -> 'a -> unit) -> 'a iarray -> unit

(** The locally-scoped version of [iteri]. *)
val iteri_local : f:(int -> 'a -> unit) -> 'a iarray -> unit

(** [map ~f a] applies function [f] to all the elements of [a], and builds an immutable
    array with the results returned by [f]:
    [[| f a.:(0); f a.:(1); ...; f a.:(length a - 1) |]]. *)
val map : f:('a -> 'b) -> 'a iarray -> 'b iarray

(** The locally-scoped and locally-allocating version of [map]. *)
val map_local : f:('a -> 'b) -> 'a iarray -> 'b iarray

(** The locally-constrained but globally-allocating version of [map]. *)
val map_local_input : f:('a -> 'b) -> 'a iarray -> 'b iarray

(** The locally-allocating but global-input version of [map]. *)
val map_local_output : f:('a -> 'b) -> 'a iarray -> 'b iarray

(** Same as {!map}, but the function is applied to the index of the element as first
    argument, and the element itself as second argument. *)
val mapi : f:(int -> 'a -> 'b) -> 'a iarray -> 'b iarray

(** The locally-scoped and locally-allocating version of [mapi]. *)
val mapi_local : f:(int -> 'a -> 'b) -> 'a iarray -> 'b iarray

(** The locally-constrained but globally-allocating version of [mapi]. *)
val mapi_local_input : f:(int -> 'a -> 'b) -> 'a iarray -> 'b iarray

(** The locally-allocating but global-input version of [mapi]. *)
val mapi_local_output : f:(int -> 'a -> 'b) -> 'a iarray -> 'b iarray

(** [fold_left ~f ~init a] computes [f (... (f (f init a.:(0)) a.:(1)) ...) a.:(n-1)],
    where [n] is the length of the immutable array [a]. *)
val fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b iarray -> 'a

(** The locally-constrained and locally-allocating version of [fold_left].

    WARNING: This function consumes O(n) extra stack space, as every intermediate
    accumulator will be left on the local stack! *)
val fold_left_local : f:('a -> 'b -> 'a) -> init:'a -> 'b iarray -> 'a

(** The locally-constrained but globally-allocating version of [fold_left]. *)
val fold_left_local_input : f:('a -> 'b -> 'a) -> init:'a -> 'b iarray -> 'a

(** The locally-allocating but global-input version of [fold_left].

    WARNING: This function consumes O(n) extra stack space, as every intermediate
    accumulator will be left on the local stack! *)
val fold_left_local_output : f:('a -> 'b -> 'a) -> init:'a -> 'b iarray -> 'a

(** [fold_left_map] is a combination of {!fold_left} and {!map} that threads an
    accumulator through calls to [f]. *)
val fold_left_map : f:('a -> 'b -> 'a * 'c) -> init:'a -> 'b iarray -> 'a * 'c iarray

(** The locally-constrained and locally-allocating version of [fold_left].

    WARNING: This function consumes O(n) extra stack space, as every intermediate
    accumulator will be left on the local stack! *)
val fold_left_map_local
  :  f:('a -> 'b -> 'a * 'c)
  -> init:'a
  -> 'b iarray
  -> 'a * 'c iarray

(** The locally-constrained but globally-allocating version of [fold_left]. *)
val fold_left_map_local_input
  :  f:('a -> 'b -> 'a * 'c)
  -> init:'a
  -> 'b iarray
  -> 'a * 'c iarray

(** The locally-allocating but global-input version of [fold_left].

    WARNING: This function consumes O(n) extra stack space, as every intermediate
    accumulator will be left on the local stack! *)
val fold_left_map_local_output
  :  f:('a -> 'b -> 'a * 'c)
  -> init:'a
  -> 'b iarray
  -> 'a * 'c iarray

(** [fold_right ~f a ~init] computes [f a.:(0) (f a.:(1) ( ... (f a.:(n-1) init) ...))],
    where [n] is the length of the immutable array [a]. *)
val fold_right : f:('b -> 'a -> 'a) -> 'b iarray -> init:'a -> 'a

(** The locally-constrained and locally-allocating version of [fold_right].

    WARNING: This function consumes O(n) extra stack space, as every intermediate
    accumulator will be left on the local stack! *)
val fold_right_local : f:('b -> 'a -> 'a) -> 'b iarray -> init:'a -> 'a

(** The locally-constrained but globally-allocating version of [fold_right]. *)
val fold_right_local_input : f:('b -> 'a -> 'a) -> 'b iarray -> init:'a -> 'a

(** The locally-allocating but global-input version of [fold_right].

    WARNING: This function consumes O(n) extra stack space, as every intermediate
    accumulator will be left on the local stack! *)
val fold_right_local_output : f:('b -> 'a -> 'a) -> 'b iarray -> init:'a -> 'a

(** {1 Iterators on two arrays} *)

(** [iter2 ~f a b] applies function [f] to all the elements of [a] and [b].
    @raise Invalid_argument if the immutable arrays are not the same size. *)
val iter2 : f:('a -> 'b -> unit) -> 'a iarray -> 'b iarray -> unit

(** The locally-scoped version of [iter2]. *)
val iter2_local : f:('a -> 'b -> unit) -> 'a iarray -> 'b iarray -> unit

(** The first-biased partly-locally-scoped version of [iter2]. *)
val iter2_local_first : f:('a -> 'b -> unit) -> 'a iarray -> 'b iarray -> unit

(** The second-biased partly-locally-scoped version of [iter2]. *)
val iter2_local_second : f:('a -> 'b -> unit) -> 'a iarray -> 'b iarray -> unit

(** [map2 ~f a b] applies function [f] to all the elements of [a] and [b], and builds an
    immutable array with the results returned by [f]:
    [[| f a.:(0) b.:(0); ...; f a.:(length a - 1) b.:(length b - 1)|]].
    @raise Invalid_argument if the immutable arrays are not the same size. *)
val map2 : f:('a -> 'b -> 'c) -> 'a iarray -> 'b iarray -> 'c iarray

(** The locally-scoped and locally-allocating version of [map2]. *)
val map2_local : f:('a -> 'b -> 'c) -> 'a iarray -> 'b iarray -> 'c iarray

(** The locally-scoped but globally-allocating version of [map2]. *)
val map2_local_inputs : f:('a -> 'b -> 'c) -> 'a iarray -> 'b iarray -> 'c iarray

(** The locally-allocating but global-inputs version of [map2]. *)
val map2_local_output : f:('a -> 'b -> 'c) -> 'a iarray -> 'b iarray -> 'c iarray

(** The first-biased partly-locally-scoped but globally-allocating version of [map2]. *)
val map2_local_first_input : f:('a -> 'b -> 'c) -> 'a iarray -> 'b iarray -> 'c iarray

(** The second-biased partly-locally-scoped but globally-allocating version of [map2]. *)
val map2_local_second_input : f:('a -> 'b -> 'c) -> 'a iarray -> 'b iarray -> 'c iarray

(** The locally-allocating and first-biased partly-locally-scoped version of [map2]. *)
val map2_local_first_input_and_output
  :  f:('a -> 'b -> 'c)
  -> 'a iarray
  -> 'b iarray
  -> 'c iarray

(** The locally-allocating and second-biased partly-locally-scoped version of [map2]. *)
val map2_local_second_input_and_output
  :  f:('a -> 'b -> 'c)
  -> 'a iarray
  -> 'b iarray
  -> 'c iarray

(** {1 Array scanning} *)

(** [for_all ~f [|a1; ...; an|]] checks if all elements of the immutable array satisfy the
    predicate [f]. That is, it returns [(f a1) && (f a2) && ... && (f an)]. *)
val for_all : f:('a -> bool) -> 'a iarray -> bool

(** The locally-scoped version of [for_all]. *)
val for_all_local : f:('a -> bool) -> 'a iarray -> bool

(** [exists ~f [|a1; ...; an|]] checks if at least one element of the immutable array
    satisfies the predicate [f]. That is, it returns [(f a1) || (f a2) || ... || (f an)]. *)
val exists : f:('a -> bool) -> 'a iarray -> bool

(** The locally-scoped version of [exists]. *)
val exists_local : f:('a -> bool) -> 'a iarray -> bool

(** Same as {!for_all}, but for a two-argument predicate.
    @raise Invalid_argument if the two immutable arrays have different lengths. *)
val for_all2 : f:('a -> 'b -> bool) -> 'a iarray -> 'b iarray -> bool

(** The locally-scoped version of [for_all2]. *)
val for_all2_local : f:('a -> 'b -> bool) -> 'a iarray -> 'b iarray -> bool

(** The first-biased partly-locally-scoped version of [for_all2]. *)
val for_all2_local_first : f:('a -> 'b -> bool) -> 'a iarray -> 'b iarray -> bool

(** The second-biased partly-locally-scoped version of [for_all2]. *)
val for_all2_local_second : f:('a -> 'b -> bool) -> 'a iarray -> 'b iarray -> bool

(** Same as {!exists}, but for a two-argument predicate.
    @raise Invalid_argument if the two immutable arrays have different lengths. *)
val exists2 : f:('a -> 'b -> bool) -> 'a iarray -> 'b iarray -> bool

(** The locally-scoped version of [exists2]. *)
val exists2_local : f:('a -> 'b -> bool) -> 'a iarray -> 'b iarray -> bool

(** The first-biased partly-locally-scoped version of [exists2]. *)
val exists2_local_first : f:('a -> 'b -> bool) -> 'a iarray -> 'b iarray -> bool

(** The second-biased partly-locally-scoped version of [exists2]. *)
val exists2_local_second : f:('a -> 'b -> bool) -> 'a iarray -> 'b iarray -> bool

(** [mem a ~set] is true if and only if [a] is structurally equal to an element of [l]
    (i.e. there is an [x] in [l] such that [compare a x = 0]). *)
val mem : 'a -> set:'a iarray -> bool

(** Same as {!mem}, but uses physical equality instead of structural equality to compare
    list elements. *)
val memq : 'a -> set:'a iarray -> bool

(** [find_opt ~f a] returns the first element of the immutable array [a] that satisfies
    the predicate [f], or [None] if there is no value that satisfies [f] in the array [a]. *)
val find_opt : f:('a -> bool) -> 'a iarray -> 'a option

(** The locally-constrained and locally-allocating version of []. *)
val find_opt_local : f:('a -> bool) -> 'a iarray -> 'a option

(** [find_map ~f a] applies [f] to the elements of [a] in order, and returns the first
    result of the form [Some v], or [None] if none exist. *)
val find_map : f:('a -> 'b option) -> 'a iarray -> 'b option

(** The locally-constrained and locally-allocating version of [find_map]. *)
val find_map_local : f:('a -> 'b option) -> 'a iarray -> 'b option

(** The locally-constrained but globally-allocating version of [find_map]. *)
val find_map_local_input : f:('a -> 'b option) -> 'a iarray -> 'b option

(** The locally-allocating but global-input version of [find_map]. *)
val find_map_local_output : f:('a -> 'b option) -> 'a iarray -> 'b option

(** {1 Arrays of pairs} *)

(** [split [:(a1,b1); ...; (an,bn):]] is [([:a1; ...; an:], [:b1; ...; bn:])]. *)
val split : ('a * 'b) iarray -> 'a iarray * 'b iarray

(** The locally-allocating version of [split]. *)
val split_local : ('a * 'b) iarray -> 'a iarray * 'b iarray

(** [combine [:a1; ...; an:] [:b1; ...; bn:]] is [[:(a1,b1); ...; (an,bn):]]. Raise
    [Invalid_argument] if the two immutable iarrays have different lengths. *)
val combine : 'a iarray -> 'b iarray -> ('a * 'b) iarray

(** The locally-allocating version of [combine]. *)
val combine_local : 'a iarray -> 'b iarray -> ('a * 'b) iarray

(** {1 Sorting} *)

(** Sort an immutable array in increasing order according to a comparison function. The
    comparison function must return 0 if its arguments compare as equal, a positive
    integer if the first is greater, and a negative integer if the first is smaller (see
    below for a complete specification). For example, {!Stdlib.compare} is a suitable
    comparison function. The result of calling [sort] is a fresh immutable array
    containing the same elements as the original sorted in increasing order. Other than
    this fresh array, [sort] is guaranteed to run in constant heap space and (at most)
    logarithmic stack space.

    The current implementation uses Heap Sort. It runs in constant stack space.

    Specification of the comparison function: Let [a] be the immutable array and [cmp] the
    comparison function. The following must be true for all [x], [y], [z] in [a] :
    - [cmp x y] > 0 if and only if [cmp y x] < 0
    - if [cmp x y] >= 0 and [cmp y z] >= 0 then [cmp x z] >= 0

    The result of [sort], which we'll call [a'], contains the same elements as [a],
    reordered in such a way that for all i and j valid indices of [a] (or equivalently, of
    [a']):
    - [cmp a'.:(i) a'.:(j)] >= 0 if and only if i >= j *)
val sort : cmp:('a -> 'a -> int) -> 'a iarray -> 'a iarray

(** Same as {!sort}, but the sorting algorithm is stable (i.e. elements that compare equal
    are kept in their original order) and not guaranteed to run in constant heap space.

    The current implementation uses Merge Sort. It uses a temporary array of length [n/2],
    where [n] is the length of the immutable array. It is usually faster than the current
    implementation of {!sort}. *)
val stable_sort : cmp:('a -> 'a -> int) -> 'a iarray -> 'a iarray

(** Same as {!sort} or {!stable_sort}, whichever is faster on typical input. *)
val fast_sort : cmp:('a -> 'a -> int) -> 'a iarray -> 'a iarray

(** {1 Iterators} *)

(** Iterate on the immutable array, in increasing order. *)
val to_seq : 'a iarray -> 'a Seq.t

(** Iterate on the immutable array, in increasing order, yielding indices along elements. *)
val to_seqi : 'a iarray -> (int * 'a) Seq.t

(** Create an immutable array from the generator *)
val of_seq : 'a Seq.t -> 'a iarray

(**/**)

(** {1 Undocumented functions} *)

(*_ The following is for system use only. Do not call directly. *)

external unsafe_get
  :  ('a iarray[@local_opt])
  -> int
  -> ('a[@local_opt])
  = "%array_unsafe_get"
