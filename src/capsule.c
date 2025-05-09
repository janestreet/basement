
#define CAML_INTERNALS

#include "caml/fail.h"
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/version.h"
#include "caml/sys.h"
#include "caml/signals.h"

#include "capsule_mutex.h"
#include "capsule_condition.h"
#include "capsule_rwlock.h"

// Copy-pasted from [runtime/sync_posix.h] which is not included in the distribution.
Caml_inline void capsule_check_error(int rc, char *msg) {
  char *err;
  char buf[1024];
  int errlen, msglen;
  value str;

  if (rc == 0)
    return;
  if (rc == ENOMEM)
    caml_raise_out_of_memory();
  err = caml_strerror(rc, buf, sizeof(buf));
  msglen = strlen(msg);
  errlen = strlen(err);
  str = caml_alloc_string(msglen + 2 + errlen);
  memcpy(&Byte(str, 0), msg, msglen);
  memcpy(&Byte(str, msglen), ": ", 2);
  memcpy(&Byte(str, msglen + 2), err, errlen);
  caml_raise_sys_error(str);
}

/* Mutex ops, ported from the runtime implementations */
static void caml_capsule_mutex_finalize(value wrapper) {
  capsule_mutex_destroy(Mutex_val(wrapper));
}

static int caml_capsule_mutex_compare(value wrapper1, value wrapper2) {
  capsule_mutex rwl1 = Mutex_val(wrapper1);
  capsule_mutex rwl2 = Mutex_val(wrapper2);
  return rwl1 == rwl2 ? 0 : rwl1 < rwl2 ? -1 : 1;
}

static intnat caml_capsule_mutex_hash(value wrapper) {
  return (intnat)Mutex_val(wrapper);
}

static struct custom_operations caml_capsule_mutex_ops = {
    "_capsule_mutex",           caml_capsule_mutex_finalize, caml_capsule_mutex_compare,
    caml_capsule_mutex_hash,    custom_serialize_default,    custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

CAMLprim value caml_capsule_mutex_new(__attribute__((unused)) value unit) {
  capsule_mutex mut = NULL;
  capsule_check_error(capsule_mutex_create(&mut), "Mutex.create");
  value wrapper =
      caml_alloc_custom(&caml_capsule_mutex_ops, sizeof(capsule_mutex *), 0, 1);
  Mutex_val(wrapper) = mut;
  return wrapper;
}

CAMLprim value caml_capsule_mutex_lock(value wrapper) {
  CAMLparam1(wrapper);
  capsule_mutex mut = Mutex_val(wrapper);
  capsule_check_error(capsule_mutex_lock(mut), "Mutex.lock");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_capsule_mutex_unlock(value wrapper) {
  CAMLparam1(wrapper);
  capsule_mutex mut = Mutex_val(wrapper);
  capsule_check_error(capsule_mutex_unlock(mut), "Mutex.unlock");
  CAMLreturn(Val_unit);
}

/* Condition ops, ported from the runtime implementations */
static void caml_capsule_condition_finalize(value wrapper) {
  capsule_condition_destroy(Condition_val(wrapper));
}

static int caml_capsule_condition_compare(value wrapper1, value wrapper2) {
  capsule_condition rwl1 = Condition_val(wrapper1);
  capsule_condition rwl2 = Condition_val(wrapper2);
  return rwl1 == rwl2 ? 0 : rwl1 < rwl2 ? -1 : 1;
}

static intnat caml_capsule_condition_hash(value wrapper) {
  return (intnat)(Condition_val(wrapper));
}

static struct custom_operations caml_capsule_condition_ops = {
    "_capsule_condition",           caml_capsule_condition_finalize,
    caml_capsule_condition_compare, caml_capsule_condition_hash,
    custom_serialize_default,       custom_deserialize_default,
    custom_compare_ext_default,     custom_fixed_length_default};

CAMLprim value caml_capsule_condition_new(__attribute__((unused)) value unit) {
  capsule_condition cond = NULL;
  capsule_check_error(capsule_condition_create(&cond), "Condition.create");
  value wrapper =
      caml_alloc_custom(&caml_capsule_condition_ops, sizeof(capsule_condition *), 0, 1);
  Condition_val(wrapper) = cond;
  return wrapper;
}

CAMLprim value caml_capsule_condition_signal(value wrapper) {
  CAMLparam1(wrapper);
  capsule_condition cond = Condition_val(wrapper);
  capsule_check_error(capsule_condition_signal(cond), "Condition.signal");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_capsule_condition_broadcast(value wrapper) {
  CAMLparam1(wrapper);
  capsule_condition mut = Condition_val(wrapper);
  capsule_check_error(capsule_condition_broadcast(mut), "Condition.broadcast");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_capsule_condition_wait(value condition_wrapper, value mutex_wrapper) {
  CAMLparam2(condition_wrapper, mutex_wrapper);
  capsule_condition cond = Condition_val(condition_wrapper);
  capsule_mutex mut = Mutex_val(mutex_wrapper);
  capsule_check_error(capsule_condition_wait(cond, mut), "Condition.wait");
  CAMLreturn(Val_unit);
}

/* Rwlock ops, ported from the runtime implementations */
static void caml_capsule_rwlock_finalize(value wrapper) {
  capsule_rwlock_destroy(Rwlock_val(wrapper));
}

static int caml_capsule_rwlock_compare(value wrapper1, value wrapper2) {
  capsule_rwlock rwl1 = Rwlock_val(wrapper1);
  capsule_rwlock rwl2 = Rwlock_val(wrapper2);
  return rwl1 == rwl2 ? 0 : rwl1 < rwl2 ? -1 : 1;
}

static intnat caml_capsule_rwlock_hash(value wrapper) {
  return (intnat)(Rwlock_val(wrapper));
}

static struct custom_operations caml_capsule_rwlock_ops = {
    "_capsule_rwlock",          caml_capsule_rwlock_finalize, caml_capsule_rwlock_compare,
    caml_capsule_rwlock_hash,   custom_serialize_default,     custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

CAMLprim value caml_capsule_rwlock_new(__attribute__((unused)) value unit) {
  capsule_rwlock rwl = NULL;
  capsule_check_error(capsule_rwlock_create(&rwl), "Rwlock.create");
  value wrapper =
      caml_alloc_custom(&caml_capsule_rwlock_ops, sizeof(capsule_rwlock *), 0, 1);
  Rwlock_val(wrapper) = rwl;
  return wrapper;
}

CAMLprim value caml_capsule_rwlock_rdlock(value wrapper) {
  CAMLparam1(wrapper);
  capsule_rwlock rwl = Rwlock_val(wrapper);
  capsule_check_error(capsule_rwlock_rdlock(rwl), "Rwlock.rdlock");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_capsule_rwlock_wrlock(value wrapper) {
  CAMLparam1(wrapper);
  capsule_rwlock rwl = Rwlock_val(wrapper);
  capsule_check_error(capsule_rwlock_wrlock(rwl), "Rwlock.wrlock");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_capsule_rwlock_unlock(value wrapper) {
  CAMLparam1(wrapper);
  capsule_rwlock rwl = Rwlock_val(wrapper);
  capsule_check_error(capsule_rwlock_unlock(rwl), "Rwlock.unlock");
  CAMLreturn(Val_unit);
}
