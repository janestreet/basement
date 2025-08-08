#pragma once

/* Mutex implementation via binary semaphore, mirroring OCaml stdlib. */

#ifdef CAML_INTERNALS

#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include <errno.h>
#include <pthread.h>

#ifdef __APPLE__
#include <dispatch/dispatch.h>
#define platform_sem_t dispatch_semaphore_t
#else
#include <semaphore.h>
#define platform_sem_t sem_t
#endif

#include "caml/mlvalues.h"
#include "caml/memory.h"

// We can't use [#ifdef CAML_RUNTIME_5] here, because upstream doesn't define it. This
// seems to be the only way to accept js and upstream rt5 and not js or upstream rt4
#if (OCAML_VERSION_MAJOR >= 5) && !(defined JANE_STREET_HAS_NO_DOMAINS)
#define CAPSULE_RUNTIME_5
#include "caml/fiber.h"
#endif

// Blocking on a mutex held by the current thread deadlocks. However, we cannot directly
// track which thread holds the lock, as OCaml fibers may migrate between threads.
// Instead, we can track the owning fiber's ID, which does not change. If the current
// fiber is descended from the owning fiber, we know the mutex is held by this thread.
//
// Note: we cannot directly expose lock/unlock (vs. with_lock) because it may narrow
// the owning ID to a child fiber, then refuse to unlock in its parent.
// [Condition.wait] includes special handling for this.

#define FIBER_NONE ((fiber_t){-1})
#define TOO_MANY_PARENTS 10000000

typedef union {
  int64_t id;
  pthread_t pthread;
} fiber_t;
static_assert(sizeof(fiber_t) == sizeof(int64_t), "fiber_t is not 64 bits");

static bool capsule_fiber_is_none(fiber_t fiber) { return fiber.id == FIBER_NONE.id; }

static fiber_t capsule_fiber_current() {
  fiber_t ret;
#ifdef CAPSULE_RUNTIME_5
  ret.id = Caml_state->current_stack->id;
#else
  ret.pthread = pthread_self();
#endif
  return ret;
}

static bool capsule_fiber_descends_from(fiber_t parent) {
  if (capsule_fiber_is_none(parent))
    return false;
#ifdef CAPSULE_RUNTIME_5
  int64_t iters = 0;
  struct stack_info *stack = Caml_state->current_stack;
  while (stack) {
    if (iters++ >= TOO_MANY_PARENTS)
      caml_fatal_error("Did not reach root fiber after many iterations.");
    if (stack->id == parent.id)
      return true;
    stack = Stack_parent(stack);
  }
  return false;
#else
  return pthread_equal(pthread_self(), parent.pthread);
#endif
}

typedef struct {
  platform_sem_t sem;
  /* ID of the fiber that has locked the mutex. FIBER_NONE if the mutex is unlocked. */
  _Atomic fiber_t owner;
} * capsule_mutex;
#define Mutex_val(v) (*(capsule_mutex *)Data_custom_val(v))

#define MUTEX_SUCCESS 0

Caml_inline int capsule_mutex_lock(capsule_mutex mut) {

#ifdef __APPLE__
  /* This only returns non-zero on timeout; don't need the other checks */
  if (dispatch_semaphore_wait(mut->sem, DISPATCH_TIME_NOW) == MUTEX_SUCCESS) {
    atomic_store_explicit(&mut->owner, capsule_fiber_current(), memory_order_relaxed);
    return MUTEX_SUCCESS;
  }
#else
  if (sem_trywait(&mut->sem) == MUTEX_SUCCESS) {
    atomic_store_explicit(&mut->owner, capsule_fiber_current(), memory_order_relaxed);
    return MUTEX_SUCCESS;
  } else if (errno == EINTR) {
    return capsule_mutex_lock(mut);
  } else if (errno != EAGAIN) {
    return errno;
  }
#endif

  if (capsule_fiber_descends_from(
          atomic_load_explicit(&mut->owner, memory_order_relaxed))) {
    // In this case, we did not race on owner
    return EDEADLK;
  }

  caml_enter_blocking_section();
#ifdef __APPLE__
  // No need to check return value, as we're not setting a timeout
  dispatch_semaphore_wait(mut->sem, DISPATCH_TIME_FOREVER);
#else
  int rc = sem_wait(&mut->sem);
#endif
  caml_leave_blocking_section();

#ifdef __APPLE__
  atomic_store_explicit(&mut->owner, capsule_fiber_current(), memory_order_relaxed);
  return MUTEX_SUCCESS;
#else
  if (rc == MUTEX_SUCCESS) {
    atomic_store_explicit(&mut->owner, capsule_fiber_current(), memory_order_relaxed);
    return MUTEX_SUCCESS;
  } else if (errno == EINTR) {
    return capsule_mutex_lock(mut);
  }
  return errno;
#endif
}

Caml_inline int capsule_mutex_unlock(capsule_mutex mut) {
  if (!capsule_fiber_descends_from(
          atomic_load_explicit(&mut->owner, memory_order_relaxed))) {
    // Cannot unlock another fiber's mutex
    return EPERM;
  }
  atomic_store_explicit(&mut->owner, FIBER_NONE, memory_order_relaxed);
#ifdef __APPLE__
  dispatch_semaphore_signal(mut->sem);
  return MUTEX_SUCCESS;
#else
  if (sem_post(&mut->sem) == MUTEX_SUCCESS) {
    return MUTEX_SUCCESS;
  }
  return errno;
#endif
}

Caml_inline int capsule_mutex_create(capsule_mutex *res) {
  capsule_mutex mut = caml_stat_alloc_noexc(sizeof(*mut));
  if (mut == NULL) {
    return ENOMEM;
  }
  atomic_store_explicit(&mut->owner, FIBER_NONE, memory_order_relaxed);
#ifdef __APPLE__
  mut->sem = dispatch_semaphore_create(1);
  if (mut->sem == NULL) {
    caml_stat_free(mut);
    return ENOMEM;
  }
#else
  // 0 = thread-shared, 1 = initial value
  if (sem_init(&mut->sem, 0, 1) != MUTEX_SUCCESS) {
    caml_stat_free(mut);
    return errno;
  }
#endif
  *res = mut;
  return MUTEX_SUCCESS;
}

Caml_inline int capsule_mutex_destroy(capsule_mutex mut) {
#ifdef __APPLE__
  dispatch_release(mut->sem);
  caml_stat_free(mut);
  return MUTEX_SUCCESS;
#else
  if (sem_close(&mut->sem) == MUTEX_SUCCESS) {
    caml_stat_free(mut);
    return MUTEX_SUCCESS;
  }
  caml_stat_free(mut);
  return errno;
#endif
}

#endif /* CAML_INTERNALS */
