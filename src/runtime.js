
//Provides: caml_is_runtime5_stub
function caml_is_runtime5_stub(unit) {
  return 0;
}

//Provides: caml_atomic_compare_exchange_stub
function caml_atomic_compare_exchange_stub(ref, o, n) {
  var old = ref[1];
  if (old === o) {
    ref[1] = n;
  }
  return old;
}

//Provides: caml_atomic_add_stub
function caml_atomic_add_stub(ref, i) {
  ref[1] += i;
  return 0;
}

//Provides: caml_atomic_sub_stub
function caml_atomic_sub_stub(ref, i) {
  ref[1] -= i;
  return 0;
}

//Provides: caml_atomic_land_stub
function caml_atomic_land_stub(ref, i) {
  ref[1] &= i;
  return 0;
}

//Provides: caml_atomic_lor_stub
function caml_atomic_lor_stub(ref, i) {
  ref[1] |= i;
  return 0;
}

//Provides: caml_atomic_lxor_stub
function caml_atomic_lxor_stub(ref, i) {
  ref[1] ^= i;
  return 0;
}

//Provides: caml_capsule_mutex_new
//Requires: caml_ml_mutex_new
function caml_capsule_mutex_new(unit) {
  return caml_ml_mutex_new(unit);
}

//Provides: caml_capsule_mutex_lock
//Requires: caml_raise_sys_error
function caml_capsule_mutex_lock(t) {
  // Same as caml_ml_mutex_lock, except raises [Sys_error] instead of [Failure].
  if (t.locked) caml_raise_sys_error("Mutex.lock: mutex already locked. Cannot wait.");
  else t.locked = true;
  return 0;
}

//Provides: caml_capsule_mutex_unlock
//Requires: caml_ml_mutex_unlock
function caml_capsule_mutex_unlock(t) {
  return caml_ml_mutex_unlock(t);
}

//Provides: caml_capsule_condition_new
//Requires: caml_failwith
function caml_capsule_condition_new(unit) {
  caml_failwith("Capsule.Condition not supported in javascript.");
}

//Provides: caml_capsule_condition_wait
//Requires: caml_failwith
function caml_capsule_condition_wait(t) {
  caml_failwith("Capsule.Condition not supported in javascript.");
}

//Provides: caml_capsule_condition_signal
//Requires: caml_failwith
function caml_capsule_condition_signal(t) {
  caml_failwith("Capsule.Condition not supported in javascript.");
}

//Provides: caml_capsule_condition_broadcast
//Requires: caml_failwith
function caml_capsule_condition_broadcast(t) {
  caml_failwith("Capsule.Condition not supported in javascript.");
}

//Provides: caml_capsule_rwlock_new
//Requires: caml_failwith
function caml_capsule_rwlock_new(unit) {
  caml_failwith("Capsule.Rwlock not supported in javascript.");
}

//Provides: caml_capsule_rwlock_rdlock
//Requires: caml_failwith
function caml_capsule_rwlock_rdlock(t) {
  caml_failwith("Capsule.Rwlock not supported in javascript.");
}

//Provides: caml_capsule_rwlock_wrlock
//Requires: caml_failwith
function caml_capsule_rwlock_wrlock(t) {
  caml_failwith("Capsule.Rwlock not supported in javascript.");
}

//Provides: caml_capsule_rwlock_unlock
//Requires: caml_failwith
function caml_capsule_rwlock_unlock(t) {
  caml_failwith("Capsule.Rwlock not supported in javascript.");
}
