module Atomic_lazy = Atomic_lazy
module Backoff = Stdlib_shim.Backoff
module Compare_failed_or_set_here = Compare_failed_or_set_here
module Dynamic = Dynamic
module Int32_u = Primitives.Int32_u
module Int64_u = Primitives.Int64_u
module Modes = Modes
module Nativeint_u = Primitives.Nativeint_u
module Or_null_shim = Or_null_shim
module Portability_hacks = Portability_hacks
module Portable_atomic = Portable_atomic
module Portable_lazy = Portable_lazy
module Primitives = Primitives
module Stdlib_iarray_labels = Stdlib_iarray_labels
module Stdlib_shim = Stdlib_shim
module Subatomic = Subatomic

module Blocking_sync = Blocking_sync
[@@alert deprecated "Use synchronization primitives from [Await]."]

module Private = struct
  module Atomic_lazy_intf = Atomic_lazy_intf
  module Portable_lazy_intf = Portable_lazy_intf
end

type nothing = |
