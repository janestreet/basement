(module
   (import "env" "caml_failwith" (func $caml_failwith (param (ref eq))))
   (import "env" "caml_raise_sys_error" (func $caml_raise_sys_error (param (ref eq))))
   (import "env" "caml_ml_mutex_new" (func $caml_ml_mutex_new (param (ref eq)) (result (ref eq))))
   (import "env" "caml_ml_mutex_lock" (func $caml_ml_mutex_lock (param (ref eq)) (result (ref eq))))
   (import "env" "caml_ml_mutex_unlock" (func $caml_ml_mutex_unlock (param (ref eq)) (result (ref eq))))
   (import "custom" "custom_next_id" (func $custom_next_id (result i64)))
   (import "custom" "custom_compare_id" (func $custom_compare_id (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
   (import "custom" "custom_hash_id" (func $custom_hash_id (param (ref eq)) (result i32)))

   (type $bytes (array (mut i8)))
   (type $compare
      (func (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
   (type $hash
      (func (param (ref eq)) (result i32)))
   (type $fixed_length (struct (field $bsize_32 i32) (field $bsize_64 i32)))
   (type $serialize
      (func (param (ref eq)) (param (ref eq)) (result i32) (result i32)))
   (type $deserialize (func (param (ref eq)) (result (ref eq)) (result i32)))
   (type $dup (func (param (ref eq)) (result (ref eq))))
   (type $custom_operations
      (struct
         (field $id (ref $bytes))
         (field $compare (ref null $compare))
         (field $compare_ext (ref null $compare))
         (field $hash (ref null $hash))
         (field $fixed_length (ref null $fixed_length))
         (field $serialize (ref null $serialize))
         (field $deserialize (ref null $deserialize))
         (field $dup (ref null $dup))))
   (type $custom (sub (struct (field (ref $custom_operations)))))
   (type $custom_with_id
      (sub $custom
         (struct
            (field (ref $custom_operations))
            (field $id i64))))

   (global $mutex_ops (ref $custom_operations)
      (struct.new $custom_operations
         (@string "_mutex")
         (ref.func $custom_compare_id)
         (ref.null $compare)
         (ref.func $custom_hash_id)
         (ref.null $fixed_length)
         (ref.null $serialize)
         (ref.null $deserialize)
         (ref.null $dup)))

   (type $mutex
      (sub final $custom_with_id
         (struct
            (field (ref $custom_operations))
            (field i64)
            (field $state (mut i32)))))

   (@string $lock_failure "Mutex.lock: mutex already locked. Cannot wait.")

   (func (export "caml_capsule_mutex_new") (param (ref eq)) (result (ref eq))
      (call $caml_ml_mutex_new (local.get 0)))

   (func (export "caml_capsule_mutex_lock") (param (ref eq)) (result (ref eq))
      (local $t (ref $mutex))
      (local.set $t (ref.cast (ref $mutex) (local.get 0)))
      (if (struct.get $mutex $state (local.get $t))
         (then (call $caml_raise_sys_error (global.get $lock_failure))))
      (struct.set $mutex $state (local.get $t) (i32.const 1))
      (ref.i31 (i32.const 0)))

   (func (export "caml_capsule_mutex_unlock") (param (ref eq)) (result (ref eq))
      (call $caml_ml_mutex_unlock (local.get 0)))

   (@string $condition_unsupported "Capsule.Condition not supported in wasm.")
   (@string $rwlock_unsupported "Capsule.Rwlock not supported in wasm.")

   (func (export "caml_capsule_condition_new") (param (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $condition_unsupported))
        (local.get 0))

   (func (export "caml_capsule_condition_wait") (param (ref eq) (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $condition_unsupported))
            (local.get 0))

   (func (export "caml_capsule_condition_signal") (param (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $condition_unsupported))
            (local.get 0))

   (func (export "caml_capsule_condition_broadcast") (param (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $condition_unsupported))
            (local.get 0))

   (func (export "caml_capsule_rwlock_new") (param (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $rwlock_unsupported))
            (local.get 0))

   (func (export "caml_capsule_rwlock_rdlock") (param (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $rwlock_unsupported))
            (local.get 0))

   (func (export "caml_capsule_rwlock_wrlock") (param (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $rwlock_unsupported))
            (local.get 0))

   (func (export "caml_capsule_rwlock_unlock") (param (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $rwlock_unsupported))
            (local.get 0))

    (type $block (array (mut (ref eq))))

   (func (export "caml_is_runtime5_stub") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_compare_exchange_stub")
      (param $ref (ref eq)) (param $o (ref eq)) (param $n (ref eq))
      (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (if (result (ref eq))
         (ref.eq (local.get $old) (local.get $o))
         (then
            (array.set $block (local.get $b) (i32.const 1) (local.get $n))
            (local.get $old))
         (else
            (local.get $old))))

   (func (export "caml_atomic_add_stub")
      (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1)
         (ref.i31 (i32.add (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_sub_stub")
      (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1)
         (ref.i31 (i32.sub (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_land_stub")
      (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1)
         (ref.i31 (i32.and (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_lor_stub")
      (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1)
         (ref.i31 (i32.or (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))

   (func (export "caml_atomic_lxor_stub")
      (param $ref (ref eq)) (param $i (ref eq)) (result (ref eq))
      (local $b (ref $block))
      (local $old (ref eq))
      (local.set $b (ref.cast (ref $block) (local.get $ref)))
      (local.set $old (array.get $block (local.get $b) (i32.const 1)))
      (array.set $block (local.get $b) (i32.const 1)
         (ref.i31 (i32.xor (i31.get_s (ref.cast (ref i31) (local.get $old)))
                           (i31.get_s (ref.cast (ref i31) (local.get $i))))))
      (ref.i31 (i32.const 0)))
)
