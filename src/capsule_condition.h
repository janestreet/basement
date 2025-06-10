#pragma once

/* Condition variable implementation via futex, mirroring OCaml stdlib.

   The implementation is copied from the OCaml runtime to avoid this bug:
   https://sourceware.org/bugzilla/show_bug.cgi?id=25847
*/

#ifdef CAML_INTERNALS

#include <string.h>
#include <errno.h>
#include <limits.h>
#include <unistd.h>

#include "caml/mlvalues.h"
#include "caml/memory.h"

#include "capsule_mutex.h"

#ifdef __APPLE__
#include <pthread.h>
typedef struct {
  _Atomic uint64_t counter;
  pthread_mutex_t mut;
  pthread_cond_t cond;
} * capsule_condition;
#else
#include <linux/futex.h>
#include <sys/syscall.h>
typedef struct {
  _Atomic uint64_t counter;
} * capsule_condition;
#endif
#define Condition_val(v) (*((capsule_condition *)Data_custom_val(v)))

#define CONDITION_SUCCESS 0

Caml_inline int capsule_condition_signal(capsule_condition cond) {
  atomic_fetch_add(&cond->counter, 1);
#ifdef __APPLE__
  if (pthread_mutex_lock(&cond->mut)) {
    caml_fatal_error("Failed to acquire futex lock.");
  }
  int rc = pthread_cond_signal(&cond->cond);
  if (pthread_mutex_unlock(&cond->mut)) {
    caml_fatal_error("Failed to release futex lock.");
  }
  return rc;
#else
  if (syscall(SYS_futex, &cond->counter, FUTEX_WAKE_PRIVATE, 1, NULL, NULL, 0) == -1) {
    return errno;
  }
  return CONDITION_SUCCESS;
#endif
}

Caml_inline int capsule_condition_broadcast(capsule_condition cond) {
  atomic_fetch_add(&cond->counter, 1);
#ifdef __APPLE__
  if (pthread_mutex_lock(&cond->mut)) {
    caml_fatal_error("Failed to acquire futex lock.");
  }
  int rc = pthread_cond_broadcast(&cond->cond);
  if (pthread_mutex_unlock(&cond->mut)) {
    caml_fatal_error("Failed to release futex lock.");
  }
  return rc;
#else
  if (syscall(SYS_futex, &cond->counter, FUTEX_WAKE_PRIVATE, INT_MAX, NULL, NULL, 0) ==
      -1) {
    return errno;
  }
  return CONDITION_SUCCESS;
#endif
}

Caml_inline int capsule_condition_wait(capsule_condition cond, capsule_mutex mut) {
  // Save and restore owner, as the current fiber may be a descendant.
  uint64_t old_count = atomic_load(&cond->counter);
  fiber_t owner = atomic_load_explicit(&mut->owner, memory_order_relaxed);

  // If the mutex operations fail, the condition is in an inconsistent state and it's not
  // safe to return to OCaml.
  if (capsule_mutex_unlock(mut) != MUTEX_SUCCESS) {
    caml_fatal_error("Failed to release mutex.");
  }

  caml_enter_blocking_section();
#ifdef __APPLE__
  if (pthread_mutex_lock(&cond->mut)) {
    caml_fatal_error("Failed to acquire futex lock.");
  }
  int rc = CONDITION_SUCCESS;
  if (atomic_load(&cond->counter) == old_count) {
    rc = pthread_cond_wait(&cond->cond, &cond->mut);
  }
  if (pthread_mutex_unlock(&cond->mut)) {
    caml_fatal_error("Failed to release futex lock.");
  }
#else
  int rc = syscall(SYS_futex, &cond->counter, FUTEX_WAIT_PRIVATE, old_count,
                   NULL, NULL, 0);
  int futex_errno = errno;
#endif
  // Re-aquire the domain lock to avoid blocking other domains on our systhreads
  caml_leave_blocking_section();

  if (capsule_mutex_lock(mut) != MUTEX_SUCCESS) {
    caml_fatal_error("Failed to re-acquire mutex.");
  }
  atomic_store_explicit(&mut->owner, owner, memory_order_relaxed);

#ifdef __APPLE__
  return rc;
#else
  if (rc == -1 && futex_errno != EAGAIN) {
    return futex_errno;
  }
  return CONDITION_SUCCESS;
#endif
}

Caml_inline int capsule_condition_create(capsule_condition *res) {
  capsule_condition cond = caml_stat_alloc_noexc(sizeof(*cond));
  if (cond == NULL) {
    return ENOMEM;
  }
  atomic_store(&cond->counter, 0);
#ifdef __APPLE__
  if (pthread_mutex_init(&cond->mut, NULL)) {
    caml_fatal_error("Failed to create futex mut.");
  }
  if (pthread_cond_init(&cond->cond, NULL)) {
    caml_fatal_error("Failed to create futex cond.");
  }
#endif
  *res = cond;
  return CONDITION_SUCCESS;
}

Caml_inline int capsule_condition_destroy(capsule_condition cond) {
#ifdef __APPLE__
  pthread_cond_destroy(&cond->cond);
  pthread_mutex_destroy(&cond->mut);
#endif
  caml_stat_free(cond);
  return CONDITION_SUCCESS;
}

#endif /* CAML_INTERNALS */
