/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2023                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_STDATOMIC
#define __FC_STDATOMIC
#include "features.h"
__PUSH_FC_STDLIB
#include "wchar.h"
#include "stddef.h"

__BEGIN_DECLS

#define ATOMIC_BOOL_LOCK_FREE 2
#define ATOMIC_CHAR_LOCK_FREE 2
#define ATOMIC_CHAR16_T_LOCK_FREE 2
#define ATOMIC_CHAR32_T_LOCK_FREE 2
#define ATOMIC_WCHAR_T_LOCK_FREE 2
#define ATOMIC_SHORT_LOCK_FREE 2
#define ATOMIC_INT_LOCK_FREE 2
#define ATOMIC_LONG_LOCK_FREE 2
#define ATOMIC_LLONG_LOCK_FREE 2
#define ATOMIC_POINTER_LOCK_FREE 2

typedef enum __fc_memory_order {
                                memory_order_relaxed,
                                memory_order_consume,
                                memory_order_acquire,
                                memory_order_release,
                                memory_order_acq_rel,
                                memory_order_seq_cst
} memory_order;

// _Atomic is currently ignored by Frama-C
#define _Atomic

typedef _Atomic struct __fc_atomic_flag {
  unsigned char __fc_val;
} atomic_flag;

#define ATOMIC_VAR_INIT(V) (V)
#define ATOMIC_FLAG_INIT {0}

typedef _Atomic _Bool atomic_bool;
typedef _Atomic char atomic_char;
typedef _Atomic signed char atomic_schar;
typedef _Atomic unsigned char atomic_uchar;
typedef _Atomic short atomic_short;
typedef _Atomic unsigned short atomic_ushort;
typedef _Atomic int atomic_int;
typedef _Atomic unsigned int atomic_uint;
typedef _Atomic long atomic_long;
typedef _Atomic unsigned long atomic_ulong;
typedef _Atomic long long atomic_llong;
typedef _Atomic unsigned long long atomic_ullong;
//typedef _Atomic char16_t atomic_char16_t;
//typedef _Atomic char32_t atomic_char32_t;
typedef _Atomic wchar_t atomic_wchar_t;
typedef _Atomic int_least8_t atomic_int_least8_t;
typedef _Atomic uint_least8_t atomic_uint_least8_t;
typedef _Atomic int_least16_t atomic_int_least16_t;
typedef _Atomic uint_least16_t atomic_uint_least16_t;
typedef _Atomic int_least32_t atomic_int_least32_t;
typedef _Atomic uint_least32_t atomic_uint_least32_t;
typedef _Atomic int_least64_t atomic_int_least64_t;
typedef _Atomic uint_least64_t atomic_uint_least64_t;
typedef _Atomic int_fast8_t atomic_int_fast8_t;
typedef _Atomic uint_fast8_t atomic_uint_fast8_t;
typedef _Atomic int_fast16_t atomic_int_fast16_t;
typedef _Atomic uint_fast16_t atomic_uint_fast16_t;
typedef _Atomic int_fast32_t atomic_int_fast32_t;
typedef _Atomic uint_fast32_t atomic_uint_fast32_t;
typedef _Atomic int_fast64_t atomic_int_fast64_t;
typedef _Atomic uint_fast64_t atomic_uint_fast64_t;
typedef _Atomic intptr_t atomic_intptr_t;
typedef _Atomic uintptr_t atomic_uintptr_t;
typedef _Atomic size_t atomic_size_t;
typedef _Atomic ptrdiff_t atomic_ptrdiff_t;
typedef _Atomic intmax_t atomic_intmax_t;
typedef _Atomic uintmax_t atomic_uintmax_t;


// NOTE: The stubs below serve only to help parsing succeed and do not
//       match expected concurrence semantics.

#define atomic_init(obj, value)                                         \
  do { __fc_atomic_init_marker(obj, value); *obj = value; } while (0)

#define kill_dependency(y) (y)

extern void atomic_thread_fence(memory_order order);
extern void atomic_signal_fence(memory_order order);

_Bool __fc_atomic_is_lock_free(void *obj);
#define atomic_is_lock_free(obj) __fc_atomic_is_lock_free(obj)

void __fc_atomic_store_marker(void *object, unsigned long long desired);
#define atomic_store(obj, value)                                        \
  do { __fc_atomic_store_marker(obj, value); *obj = value; } while (0)

void __fc_atomic_store_explicit_marker(void *object, unsigned long long desired,
                                       memory_order order);
#define atomic_store_explicit(obj, value, order)                        \
  do { __fc_atomic_store_explicit_marker(obj, value, order); *obj = value; } \
  while (0)

unsigned long long __fc_atomic_load(void *object, size_t obj_size);
#define atomic_load(obj) __fc_atomic_load(obj, sizeof(*obj))

unsigned long long __fc_atomic_load_explicit(void *object, memory_order order,
                                             size_t obj_size);
#define atomic_load_explicit(obj, order)                \
  __fc_atomic_load_explicit(obj, order, sizeof(*obj))

unsigned long long __fc_atomic_exchange(void *object,
                                        unsigned long long desired,
                                        size_t obj_size);
#define atomic_exchange(obj, desired)                   \
  __fc_atomic_exchange(obj, desired, sizeof(*obj))

unsigned long long __fc_atomic_exchange_explicit(void *object,
                                                 unsigned long long desired,
                                                 memory_order order,
                                                 size_t obj_size);
#define atomic_exchange_explicit(obj, desired, order)                   \
  __fc_atomic_exchange_explicit(obj, desired, order, sizeof(*obj))

_Bool __fc_atomic_compare_exchange_strong(void *object, void *expected,
                                          unsigned long long desired,
                                          size_t obj_size);
#define atomic_compare_exchange_strong(obj, desired, expected)          \
  __fc_atomic_compare_exchange_strong(obj, desired, expected, sizeof(*obj))

_Bool __fc_atomic_compare_exchange_strong_explicit(void *object, void *expected,
                                                   unsigned long long desired,
                                                   memory_order success,
                                                   memory_order failure,
                                                   size_t obj_size);
#define atomic_compare_exchange_strong_explicit(obj, desired, expected, \
                                                success, failure)       \
  __fc_atomic_compare_exchange_strong_explicit(obj, desired, expected,  \
                                               success, failure, sizeof(*obj))

_Bool __fc_atomic_compare_exchange_weak(void *object, void *expected,
                                        unsigned long long desired,
                                        size_t obj_size);
#define atomic_compare_exchange_weak(obj, desired, expected) \
  __fc_atomic_compare_exchange_weak(obj, desired, expected, sizeof(*obj))

_Bool __fc_atomic_compare_exchange_weak_explicit(void *object, void *expected,
                                                 unsigned long long desired,
                                                 memory_order success,
                                                 memory_order failure,
                                                 size_t obj_size);
#define atomic_compare_exchange_weak_explicit(obj, desired, expected,   \
                                              success, failure)         \
  __fc_atomic_compare_exchange_weak_explicit(obj, desired, expected, success, \
                                             failure, sizeof(*obj))

unsigned long long __fc_atomic_fetch_add(void *object,
                                         unsigned long long operand,
                                         size_t obj_size);
#define atomic_fetch_add(obj, operand)                  \
  __fc_atomic_fetch_add(obj, operand, sizeof(*obj))

unsigned long long __fc_atomic_fetch_add_explicit(void *object,
                                                  unsigned long long operand,
                                                  memory_order order,
                                                  size_t obj_size);
#define atomic_fetch_add_explicit(obj, operand, order)                  \
  __fc_atomic_fetch_add_explicit(obj, operand, order, sizeof(*obj))

unsigned long long __fc_atomic_fetch_sub(void *object,
                                         unsigned long long operand,
                                         size_t obj_size);
#define atomic_fetch_sub(obj, operand)                  \
  __fc_atomic_fetch_sub(obj, operand, sizeof(*obj))

unsigned long long __fc_atomic_fetch_sub_explicit(void *object,
                                                  unsigned long long operand,
                                                  memory_order order,
                                                  size_t obj_size);
#define atomic_fetch_sub_explicit(obj, operand, order)                  \
  __fc_atomic_fetch_sub_explicit(obj, operand, order, sizeof(*obj))

unsigned long long __fc_atomic_fetch_or(void *object,
                                        unsigned long long operand,
                                        size_t obj_size);
#define atomic_fetch_or(obj, operand)                   \
  __fc_atomic_fetch_or(obj, operand, sizeof(*obj))

unsigned long long __fc_atomic_fetch_or_explicit(void *object,
                                                 unsigned long long operand,
                                                 memory_order order,
                                                 size_t obj_size);
#define atomic_fetch_or_explicit(obj, operand, order)                   \
  __fc_atomic_fetch_or_explicit(obj, operand, order, sizeof(*obj))

unsigned long long __fc_atomic_fetch_xor(void *object,
                                         unsigned long long operand,
                                         size_t obj_size);
#define atomic_fetch_xor(obj, operand)                  \
  __fc_atomic_fetch_xor(obj, operand, sizeof(*obj))

unsigned long long __fc_atomic_fetch_xor_explicit(void *object,
                                                  unsigned long long operand,
                                                  memory_order order,
                                                  size_t obj_size);
#define atomic_fetch_xor_explicit(obj, operand, order)                  \
  __fc_atomic_fetch_xor_explicit(obj, operand, order, sizeof(*obj))

unsigned long long __fc_atomic_fetch_and(void *object,
                                         unsigned long long operand,
                                         size_t obj_size);
#define atomic_fetch_and(obj, operand)                  \
  __fc_atomic_fetch_and(obj, operand, sizeof(*obj))

unsigned long long __fc_atomic_fetch_and_explicit(void *object,
                                                  unsigned long long operand,
                                                  memory_order order,
                                                  size_t obj_size);
#define atomic_fetch_and_explicit(obj, operand, order)                  \
  __fc_atomic_fetch_and_explicit(obj, operand, order, sizeof(*obj))

extern _Bool atomic_flag_test_and_set(volatile atomic_flag *object);
extern _Bool atomic_flag_test_and_set_explicit(volatile atomic_flag *object,
                                               memory_order order);
extern void atomic_flag_clear(volatile atomic_flag *object);
extern void atomic_flag_clear_explicit(volatile atomic_flag *object,
                                       memory_order order);

__END_DECLS

__POP_FC_STDLIB
#endif
