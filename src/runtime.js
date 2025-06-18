
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

//Provides: caml_atomic_set_stub
function caml_atomic_set_stub(ref, i) {
  ref[1] = i;
  return 0;
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

//Provides: CapsuleMutex
class CapsuleMutex {
  constructor() {
    this.locked = false;
  }
}

//Provides: caml_capsule_mutex_new
//Requires: CapsuleMutex
function caml_capsule_mutex_new(unit) {
  return new CapsuleMutex();
}

//Provides: caml_capsule_mutex_lock
//Requires: caml_raise_sys_error
function caml_capsule_mutex_lock(t) {
  if (t.locked) caml_raise_sys_error("Mutex.lock: mutex already locked.");
  else t.locked = true;
  return 0;
}

//Provides: caml_capsule_mutex_unlock
function caml_capsule_mutex_unlock(t) {
  t.locked = false;
  return 0;
}

//Provides: caml_capsule_condition_new
function caml_capsule_condition_new(unit) {
  return { condition: 1 };
}

//Provides: caml_capsule_condition_wait
//Requires: caml_raise_sys_error
function caml_capsule_condition_wait(t, mutext) {
  caml_raise_sys_error("Condition.wait: cannot wait.");
  return 0;
}

//Provides: caml_capsule_condition_broadcast
function caml_capsule_condition_broadcast(t) {
  return 0;
}

//Provides: caml_capsule_condition_signal
function caml_capsule_condition_signal(t) {
  return 0;
}

//Provides: CapsuleRwlock
class CapsuleRwlock {
  constructor() {
    this.wrlocked = false;
    this.rdlocks = 0;
  }
}

//Provides: caml_capsule_rwlock_new
//Requires: CapsuleRwlock
function caml_capsule_rwlock_new(unit) {
  return new CapsuleRwlock();
}

//Provides: caml_capsule_rwlock_wrlock
//Requires: caml_raise_sys_error
function caml_capsule_rwlock_wrlock(t) {
  if (t.wrlocked || t.rdlocks > 0) caml_raise_sys_error("Rwlock.wrlock: rwlock already locked.");
  else t.wrlocked = true;
  return 0;
}

//Provides: caml_capsule_rwlock_rdlock
//Requires: caml_raise_sys_error
function caml_capsule_rwlock_rdlock(t) {
  if (t.wrlocked) caml_raise_sys_error("Rwlock.rdlock: rwlock already write locked.");
  else t.rdlocks += 1;
  return 0;
}

//Provides: caml_capsule_rwlock_unlock
function caml_capsule_rwlock_unlock(t) {
  if (t.wrlocked) t.wrlocked = false;
  else t.rdlocks -= 1;
  return 0;
}
