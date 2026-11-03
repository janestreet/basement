#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/camlatomic.h>
#include <caml/domain.h>

// The upstream OCaml runtime doesn't export the write_barrier directly.
Caml_inline void write_barrier(value obj, intnat field, value old_val, value new_val) {
  /* HACK: can't assert when get old C-api style pointers
    CAMLassert (Is_block(obj)); */

  if (!Is_young(obj)) {

    if (Is_block(old_val)) {
      /* if old is in the minor heap,
         then this is in a remembered set already */
      if (Is_young(old_val))
        return;
      /* old is a block and in the major heap */
      caml_darken(Caml_state, old_val, 0);
    }
    /* this update is creating a new link from major to minor, remember it */
    if (Is_block_and_young(new_val)) {
      Ref_table_add(&Caml_state->minor_tables->major_ref, Op_val(obj) + field);
    }
  }
}

CAMLprim value caml_atomic_set_stub_loc(value loc, value newv) {
  value ref = Field(loc, 0);
  intnat field = Long_val(Field(loc, 1));
  value oldv;
  if (caml_domain_alone()) {
    oldv = Field(ref, field);
    Field(ref, field) = newv;
  } else {
    atomic_thread_fence(memory_order_acquire);
    oldv = atomic_exchange(&Op_atomic_val(ref)[field], newv);
    atomic_thread_fence(memory_order_release); /* generates `dmb ish` on Arm64*/
  }
  write_barrier(ref, field, oldv, newv);
  return Val_unit;
}

CAMLprim value caml_atomic_compare_exchange_stub_loc(value loc, value oldv, value newv) {
  value ref = Field(loc, 0);
  intnat field = Long_val(Field(loc, 1));
  if (caml_domain_alone()) {
    value *p = &Op_val(ref)[field];
    if (*p == oldv) {
      *p = newv;
      write_barrier(ref, field, oldv, newv);
      return oldv;
    } else {
      return *p;
    }
  } else {
    atomic_value *p = &Op_atomic_val(ref)[field];
    int cas_ret = atomic_compare_exchange_strong(p, &oldv, newv);
    atomic_thread_fence(memory_order_release); /* generates `dmb ish` on Arm64*/
    if (cas_ret) {
      write_barrier(ref, field, oldv, newv);
    }
    return oldv;
  }
}

CAMLprim value caml_atomic_add_stub_loc(value loc, value incr) {
  value ref = Field(loc, 0);
  intnat field = Long_val(Field(loc, 1));
  if (caml_domain_alone()) {
    value *p = &Op_val(ref)[field];
    CAMLassert(Is_long(*p));
    *p = Val_long(Long_val(*p) + Long_val(incr));
    /* no write barrier needed, integer write */
  } else {
    atomic_value *p = &Op_atomic_val(ref)[field];
    atomic_fetch_add(p, 2 * Long_val(incr));   /* ignore the result */
    atomic_thread_fence(memory_order_release); /* generates `dmb ish` on Arm64*/
  }
  return Val_unit;
}

CAMLprim value caml_atomic_sub_stub_loc(value loc, value incr) {
  value ref = Field(loc, 0);
  intnat field = Long_val(Field(loc, 1));
  if (caml_domain_alone()) {
    value *p = &Op_val(ref)[field];
    CAMLassert(Is_long(*p));
    *p = Val_long(Long_val(*p) - Long_val(incr));
    /* no write barrier needed, integer write */
  } else {
    atomic_value *p = &Op_atomic_val(ref)[field];
    atomic_fetch_sub(p, 2 * Long_val(incr));   /* ignore the result */
    atomic_thread_fence(memory_order_release); /* generates `dmb ish` on Arm64*/
  }
  return Val_unit;
}

CAMLprim value caml_atomic_land_stub_loc(value loc, value incr) {
  value ref = Field(loc, 0);
  intnat field = Long_val(Field(loc, 1));
  if (caml_domain_alone()) {
    value *p = &Op_val(ref)[field];
    CAMLassert(Is_long(*p));
    *p = Val_long(Long_val(*p) & Long_val(incr));
    /* no write barrier needed, integer write */
  } else {
    atomic_value *p = &Op_atomic_val(ref)[field];
    atomic_fetch_and(p, incr);                 /* ignore the result */
    atomic_thread_fence(memory_order_release); /* generates `dmb ish` on Arm64*/
  }
  return Val_unit;
}

CAMLprim value caml_atomic_lor_stub_loc(value loc, value incr) {
  value ref = Field(loc, 0);
  intnat field = Long_val(Field(loc, 1));
  if (caml_domain_alone()) {
    value *p = &Op_val(ref)[field];
    CAMLassert(Is_long(*p));
    *p = Val_long(Long_val(*p) | Long_val(incr));
    /* no write barrier needed, integer write */
  } else {
    atomic_value *p = &Op_atomic_val(ref)[field];
    atomic_fetch_or(p, incr);                  /* ignore the result */
    atomic_thread_fence(memory_order_release); /* generates `dmb ish` on Arm64*/
  }
  return Val_unit;
}

CAMLprim value caml_atomic_lxor_stub_loc(value loc, value incr) {
  value ref = Field(loc, 0);
  intnat field = Long_val(Field(loc, 1));
  if (caml_domain_alone()) {
    value *p = &Op_val(ref)[field];
    CAMLassert(Is_long(*p));
    *p = Val_long(Long_val(*p) ^ Long_val(incr));
    /* no write barrier needed, integer write */
  } else {
    atomic_value *p = &Op_atomic_val(ref)[field];
    atomic_fetch_xor(p, 2 * Long_val(incr));   /* ignore the result */
    atomic_thread_fence(memory_order_release); /* generates `dmb ish` on Arm64*/
  }
  return Val_unit;
}
