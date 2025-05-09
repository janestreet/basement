#pragma once

/* Reader-writer lock implementation via pthreads, mirroring OCaml stdlib. */

#ifdef CAML_INTERNALS

#include <pthread.h>
#include <string.h>
#include <errno.h>

#include "caml/mlvalues.h"
#include "caml/memory.h"

typedef pthread_rwlock_t *capsule_rwlock;
#define Rwlock_val(v) (*((capsule_rwlock *)Data_custom_val(v)))

#define RWLOCK_SUCCESS 0

Caml_inline int capsule_rwlock_rdlock(capsule_rwlock rw) {
  if (pthread_rwlock_tryrdlock(rw) == RWLOCK_SUCCESS) {
    return RWLOCK_SUCCESS;
  }
  caml_enter_blocking_section();
  int rc = pthread_rwlock_rdlock(rw);
  caml_leave_blocking_section();
  return rc;
}

Caml_inline int capsule_rwlock_wrlock(capsule_rwlock rw) {
  if (pthread_rwlock_trywrlock(rw) == RWLOCK_SUCCESS) {
    return RWLOCK_SUCCESS;
  }
  caml_enter_blocking_section();
  int rc = pthread_rwlock_wrlock(rw);
  caml_leave_blocking_section();
  return rc;
}

Caml_inline int capsule_rwlock_unlock(capsule_rwlock rw) {
  return pthread_rwlock_unlock(rw);
}

Caml_inline int capsule_rwlock_create(capsule_rwlock *res) {
  capsule_rwlock rw = caml_stat_alloc_noexc(sizeof(*rw));
  if (rw == NULL) {
    return ENOMEM;
  }
  int rc = pthread_rwlock_init(rw, NULL); // default read-write lock attributes are used
  if (rc != RWLOCK_SUCCESS) {
    caml_stat_free(rw);
    return rc;
  }
  *res = rw;
  return RWLOCK_SUCCESS;
}

Caml_inline int capsule_rwlock_destroy(capsule_rwlock m) {
  int rc = pthread_rwlock_destroy(m);
  caml_stat_free(m);
  return rc;
}

#endif /* CAML_INTERNALS */
