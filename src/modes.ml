include Stdlib_shim.Modes

type ('a : value_or_null) global : value_or_null mod global =
      'a Stdlib_shim.Modes.Global.t =
  { global : 'a @@ global }
[@@unboxed]

type ('a : value_or_null) portable : value_or_null mod portable =
      'a Stdlib_shim.Modes.Portable.t =
  { portable : 'a @@ portable }
[@@unboxed]

type ('a : value_or_null) contended : value_or_null mod contended =
      'a Stdlib_shim.Modes.Contended.t =
  { contended : 'a @@ contended }
[@@unboxed]

type ('a : value_or_null) portended : value_or_null mod contended portable =
      'a Stdlib_shim.Modes.Portended.t =
  { portended : 'a @@ contended portable }
[@@unboxed]

type ('a : value_or_null) aliased : value_or_null mod aliased =
      'a Stdlib_shim.Modes.Aliased.t =
  { aliased : 'a @@ aliased }
[@@unboxed]

type ('a : value_or_null) many : value_or_null mod many = 'a Stdlib_shim.Modes.Many.t =
  { many : 'a @@ many }
[@@unboxed]

type ('a : value_or_null) aliased_many : value_or_null mod aliased many =
  { aliased_many : 'a @@ aliased many }
[@@unboxed]

type ('a : value_or_null) shared = { shared : 'a @@ shared } [@@unboxed]

type ('a : value_or_null) forkable : value_or_null mod forkable =
  { forkable : 'a @@ forkable }
[@@unboxed]

type ('a : value_or_null) unyielding : value_or_null mod unyielding =
  { unyielding : 'a @@ unyielding }
[@@unboxed]

type ('a : value_or_null) stateless : value_or_null mod stateless =
  { stateless : 'a @@ stateless }
[@@unboxed]

type ('a : value_or_null) reading = { reading : 'a @@ reading } [@@unboxed]

type ('a : value_or_null) immutable : value_or_null mod immutable =
  { immutable : 'a @@ immutable }
[@@unboxed]

type ('a : value_or_null) read = { read : 'a @@ read } [@@unboxed]

type ('a : value mod non_float) immutable_data : immutable_data =
  { immutable_data : 'a @@ forkable immutable many stateless unyielding }
[@@unboxed]
