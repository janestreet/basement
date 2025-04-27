
#define CAML_INTERNALS

#include "caml/fail.h"
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/version.h"
#include "caml/sys.h"

// We can't use [#ifdef CAML_RUNTIME_5] here, because upstream doesn't define it. This
// seems to be the only way to accept js and upstream rt5 and not js or upstream rt4
#if (OCAML_VERSION_MAJOR >= 5) && !(defined JANE_STREET_HAS_NO_DOMAINS)

#include "sync_rwlock.h"

// Copy-pasted from [runtime/sync_posix.h] which is not included in the distribution.
Caml_inline void sync_check_error(int retcode, char *msg) {
  char *err;
  char buf[1024];
  int errlen, msglen;
  value str;

  if (retcode == 0)
    return;
  if (retcode == ENOMEM)
    caml_raise_out_of_memory();
  err = caml_strerror(retcode, buf, sizeof(buf));
  msglen = strlen(msg);
  errlen = strlen(err);
  str = caml_alloc_string(msglen + 2 + errlen);
  memcpy(&Byte(str, 0), msg, msglen);
  memcpy(&Byte(str, msglen), ": ", 2);
  memcpy(&Byte(str, msglen + 2), err, errlen);
  caml_raise_sys_error(str);
}

CAMLextern value caml_ml_mutex_new(value unit);
CAMLextern value caml_ml_mutex_lock(value wrapper);
CAMLextern value caml_ml_mutex_unlock(value wrapper);
CAMLextern value caml_ml_condition_new(value unit);
CAMLextern value caml_ml_condition_wait(value wcond, value wmut);
CAMLextern value caml_ml_condition_signal(value wrapper);
CAMLextern value caml_ml_condition_broadcast(value wrapper);

/* Rwlock ops */
static void caml_capsule_rwlock_finalize(value wrapper) {
  sync_rwlock_destroy(Rwlock_val(wrapper));
}

static int caml_capsule_rwlock_compare(value wrapper1, value wrapper2) {
  sync_rwlock rwl1 = Rwlock_val(wrapper1);
  sync_rwlock rwl2 = Rwlock_val(wrapper2);
  return rwl1 == rwl2 ? 0 : rwl1 < rwl2 ? -1 : 1;
}

static intnat caml_capsule_rwlock_hash(value wrapper) {
  return (intnat)(Rwlock_val(wrapper));
}

static struct custom_operations caml_capsule_rwlock_ops = {"_rwlock",
                                                           caml_capsule_rwlock_finalize,
                                                           caml_capsule_rwlock_compare,
                                                           caml_capsule_rwlock_hash,
                                                           custom_serialize_default,
                                                           custom_deserialize_default,
                                                           custom_compare_ext_default,
                                                           custom_fixed_length_default};

CAMLprim value caml_capsule_rwlock_new(value unit) {
  (void)unit;
  sync_rwlock rwl = NULL;
  value wrapper;
  sync_check_error(sync_rwlock_create(&rwl), "Rwlock.create");
  wrapper = caml_alloc_custom(&caml_capsule_rwlock_ops, sizeof(pthread_rwlock_t *), 0, 1);
  Rwlock_val(wrapper) = rwl;
  return wrapper;
}

CAMLprim value caml_capsule_rwlock_rdlock(value wrapper) {
  CAMLparam1(wrapper);
  sync_retcode retcode;
  sync_rwlock rwl = Rwlock_val(wrapper);
  if (sync_rwlock_tryrdlock(rwl) != RWLOCK_SUCCESS) {
    caml_enter_blocking_section();
    retcode = sync_rwlock_rdlock(rwl);
    caml_leave_blocking_section();
    sync_check_error(retcode, "Rwlock.rdlock");
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_capsule_rwlock_wrlock(value wrapper) {
  CAMLparam1(wrapper);
  sync_retcode retcode;
  sync_rwlock rwl = Rwlock_val(wrapper);
  if (sync_rwlock_trywrlock(rwl) != RWLOCK_SUCCESS) {
    caml_enter_blocking_section();
    retcode = sync_rwlock_wrlock(rwl);
    caml_leave_blocking_section();
    sync_check_error(retcode, "Rwlock.wrlock");
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_capsule_rwlock_unlock(value wrapper) {
  sync_rwlock rwl = Rwlock_val(wrapper);
  sync_retcode retcode;
  retcode = sync_rwlock_unlock(rwl);
  sync_check_error(retcode, "Rwlock.unlock");
  return Val_unit;
}

CAMLprim value caml_capsule_mutex_new(value unit) { return caml_ml_mutex_new(unit); }

CAMLprim value caml_capsule_mutex_lock(value wrapper) {
  return caml_ml_mutex_lock(wrapper);
}

CAMLprim value caml_capsule_mutex_unlock(value wrapper) {
  return caml_ml_mutex_unlock(wrapper);
}

CAMLprim value caml_capsule_condition_new(value unit) {
  return caml_ml_condition_new(unit);
}

CAMLprim value caml_capsule_condition_wait(value wcond, value wmut) {
  return caml_ml_condition_wait(wcond, wmut);
}

CAMLprim value caml_capsule_condition_signal(value wrapper) {
  return caml_ml_condition_signal(wrapper);
}

CAMLprim value caml_capsule_condition_broadcast(value wrapper) {
  return caml_ml_condition_broadcast(wrapper);
}

#else

CAMLprim value caml_capsule_rwlock_new(value unit) {
  (void)unit;
  caml_failwith("Must use runtime5 to use Rwlock");
}

CAMLprim value caml_capsule_rwlock_rdlock(value wrapper) {
  (void)wrapper;
  caml_failwith("Must use runtime5 to use Rwlock");
}

CAMLprim value caml_capsule_rwlock_wrlock(value wrapper) {
  (void)wrapper;
  caml_failwith("Must use runtime5 to use Rwlock");
}

CAMLprim value caml_capsule_rwlock_unlock(value wrapper) {
  (void)wrapper;
  caml_failwith("Must use runtime5 to use Rwlock");
}

CAMLprim value caml_capsule_mutex_new(value unit) {
  CAMLparam1(unit);
  value res = caml_alloc_small(1, 0);
  Field(res, 0) = Val_false;
  CAMLreturn(res);
}

CAMLprim value caml_capsule_mutex_lock(value wrapper) {
  CAMLparam1(wrapper);
  if (Field(wrapper, 0) == Val_true) {
    caml_raise_sys_error(caml_copy_string("Attempted to recursively lock mutex."));
  }
  Field(wrapper, 0) = Val_true;
  CAMLreturn(Val_unit);
}

CAMLprim value caml_capsule_mutex_unlock(value wrapper) {
  CAMLparam1(wrapper);
  if (Field(wrapper, 0) == Val_false) {
    caml_raise_sys_error(caml_copy_string("Attempted to recursively unlock mutex."));
  }
  Field(wrapper, 0) = Val_false;
  CAMLreturn(Val_unit);
}

CAMLprim value caml_capsule_condition_new(value unit) {
  (void)unit;
  caml_failwith("Must use runtime5 to use Condition");
}

CAMLprim value caml_capsule_condition_wait(value wcond, value wmut) {
  (void)wcond;
  (void)wmut;
  caml_failwith("Must use runtime5 to use Condition");
}

CAMLprim value caml_capsule_condition_signal(value wrapper) {
  (void)wrapper;
  caml_failwith("Must use runtime5 to use Condition");
}

CAMLprim value caml_capsule_condition_broadcast(value wrapper) {
  (void)wrapper;
  caml_failwith("Must use runtime5 to use Condition");
}

#endif
