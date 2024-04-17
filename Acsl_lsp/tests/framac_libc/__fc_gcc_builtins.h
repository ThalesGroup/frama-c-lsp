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

// This file contains some GCC builtins which are not already hardcoded in
// Frama-C, and which can be expressed using ACSL.

#ifndef __FC_GCC_BUILTINS
#define __FC_GCC_BUILTINS
#include "__fc_machdep.h"
#include "features.h"

__PUSH_FC_STDLIB

__BEGIN_DECLS

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (int)(a + b);
  ensures result_overflow: a + b == (int)(a + b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_sadd_overflow(int a, int b, int* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (long)(a + b);
  ensures result_overflow: a + b == (long)(a + b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_saddl_overflow(long a, long b, long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (long long)(a + b);
  ensures result_overflow: a + b == (long long)(a + b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_saddll_overflow(long long a, long long b, long long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned)(a + b);
  ensures result_overflow: a + b == (unsigned)(a + b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_uadd_overflow(unsigned a, unsigned b, unsigned* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned long)(a + b);
  ensures result_overflow: a + b == (unsigned long)(a + b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_uaddl_overflow(unsigned long a, unsigned long b, unsigned long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned long long)(a + b);
  ensures result_overflow: a + b == (unsigned long long)(a + b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_uaddll_overflow(unsigned long long a, unsigned long long b, unsigned long long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (int)(a - b);
  ensures result_overflow: a - b == (int)(a - b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_ssub_overflow(int a, int b, int* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (long)(a - b);
  ensures result_overflow: a - b == (long)(a - b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_ssubl_overflow(long a, long b, long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (long long)(a - b);
  ensures result_overflow: a - b == (long long)(a - b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_ssubll_overflow(long long a, long long b, long long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned)(a - b);
  ensures result_overflow: a - b == (unsigned)(a - b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_usub_overflow(unsigned a, unsigned b, unsigned* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned long)(a - b);
  ensures result_overflow: a - b == (unsigned long)(a - b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_usubl_overflow(unsigned long a, unsigned long b, unsigned long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned long long)(a - b);
  ensures result_overflow: a - b == (unsigned long long)(a - b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_usubll_overflow(unsigned long long a, unsigned long long b, unsigned long long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (int)(a * b);
  ensures result_overflow: a * b == (int)(a * b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_smul_overflow(int a, int b, int* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (long)(a * b);
  ensures result_overflow: a * b == (long)(a * b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_smull_overflow(long a, long b, long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (long long)(a * b);
  ensures result_overflow: a * b == (long long)(a * b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_smulll_overflow(long long a, long long b, long long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned)(a * b);
  ensures result_overflow: a * b == (unsigned)(a * b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_umul_overflow(unsigned a, unsigned b, unsigned* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned long)(a * b);
  ensures result_overflow: a * b == (unsigned long)(a * b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_umull_overflow(unsigned long a, unsigned long b, unsigned long* res);

/*@
  requires valid_res: \valid(res);
  assigns \result, *res \from a, b;
  ensures initialization:res: \initialized(res);
  ensures res_wrapped: *res == (unsigned long long)(a * b);
  ensures result_overflow: a * b == (unsigned long long)(a * b) ? \result == 0 : \result == 1;
 */
_Bool __builtin_umulll_overflow(unsigned long long a, unsigned long long b, unsigned long long* res);

/*@
  requires x_nonzero: x != 0;
  assigns \result \from indirect:x;
  ensures result_is_bit_count: 0 <= \result < __CHAR_BIT * sizeof(x);
 */
int __builtin_clz(unsigned int x);

/*@
  requires x_nonzero: x != 0;
  assigns \result \from indirect:x;
  ensures result_is_bit_count: 0 <= \result < __CHAR_BIT * sizeof(x);
 */
int __builtin_clzl(unsigned long x);

/*@
  requires x_nonzero: x != 0;
  assigns \result \from indirect:x;
  ensures result_is_bit_count: 0 <= \result < __CHAR_BIT * sizeof(x);
 */
int __builtin_clzll(unsigned long long x);

/*@
  requires x_nonzero: x != 0;
  assigns \result \from indirect:x;
  ensures result_is_bit_count: 0 <= \result < __CHAR_BIT * sizeof(x);
 */
int __builtin_ctz(unsigned int x);

/*@
  requires x_nonzero: x != 0;
  assigns \result \from indirect:x;
  ensures result_is_bit_count: 0 <= \result < __CHAR_BIT * sizeof(x);
 */
int __builtin_ctzl(unsigned long x);

/*@
  requires x_nonzero: x != 0;
  assigns \result \from indirect:x;
  ensures result_is_bit_count: 0 <= \result < __CHAR_BIT * sizeof(x);
 */
int __builtin_ctzll(unsigned long long x);

/*@
  assigns \result \from indirect:x;
  ensures result_is_bit_count: 0 <= \result <= __CHAR_BIT * sizeof(x);
 */
int __builtin_popcount(unsigned int x);

/*@
  assigns \result \from indirect:x;
  ensures result_is_bit_count: 0 <= \result <= __CHAR_BIT * sizeof(x);
 */
int __builtin_popcountl(unsigned long x);

/*@
  assigns \result \from indirect:x;
  ensures result_is_bit_count: 0 <= \result <= __CHAR_BIT * sizeof(x);
 */
int __builtin_popcountll(unsigned long long x);

// GCC specialized __atomic_* functions.
// TODO: add generic counterpart with some help from cabs2cil for typing
// NB: __atomic_* operations do not seem to make a distinction between
// signed and unsigned: this may lead to some warnings by Frama-C

/*@
  requires validity: \valid_read(mem);
  requires initialization: \initialized(mem);
  assigns \result \from *mem, indirect:model;
  ensures load_value: \result == *mem;
*/
__UINT8_T __atomic_load_1(const __UINT8_T* mem, int model);

/*@
  requires validity: \valid_read(mem);
  requires initialization: \initialized(mem);
  assigns \result \from *mem, indirect: model;
  ensures load_value: \result == *mem;
*/
__UINT16_T __atomic_load_2(const __UINT16_T* mem, int model);

/*@
  requires validity: \valid_read(mem);
  requires initialization: \initialized(mem);
  assigns \result \from *mem, indirect:model;
  ensures load_value: \result == *mem;
*/
__UINT32_T __atomic_load_4(const __UINT32_T* mem, int model);

/*@
  requires validity: \valid_read(mem);
  requires initialization: \initialized(mem);
  assigns \result \from *mem, indirect:model;
  ensures load_value: \result == *mem;
*/
__UINT64_T __atomic_load_8(const __UINT64_T* mem, int model);

/*@
  requires validity: \valid(mem);
  assigns *mem \from val, indirect: model;
  ensures initialization: \initialized(mem);
  ensures store_value: *mem == val;
*/
void __atomic_store_1(__UINT8_T* mem, __UINT8_T val, int model);

/*@
  requires validity: \valid(mem);
  assigns *mem \from val, indirect: model;
  ensures initialization: \initialized(mem);
  ensures store_value: *mem == val;
*/
void __atomic_store_2(__UINT16_T* mem, __UINT16_T val, int model);

/*@
  requires validity: \valid(mem);
  assigns *mem \from val, indirect: model;
  ensures initialization: \initialized(mem);
  ensures store_value: *mem == val;
*/
void __atomic_store_4(__UINT32_T* mem, __UINT32_T val, int model);

/*@
  requires validity: \valid(mem);
  assigns *mem \from val, indirect: model;
  ensures initialization: \initialized(mem);
  ensures store_value: *mem == val;
*/
void __atomic_store_8(__UINT64_T* mem, __UINT64_T val, int model);

/*@ requires validity: \valid(mem);
    requires initialization: \initialized(mem);
    assigns *mem \from val, indirect:model;
    assigns \result \from *mem, indirect:model;
 */
__UINT8_T __atomic_exchange_1(__UINT8_T* mem, __UINT8_T val, int model);

/*@ requires validity: \valid(mem);
    requires initialization: \initialized(mem);
    assigns *mem \from val, indirect:model;
    assigns \result \from *mem, indirect:model;
 */
__UINT16_T __atomic_exchange_2(__UINT16_T* mem, __UINT16_T val, int model);

/*@ requires validity: \valid(mem);
    requires initialization: \initialized(mem);
    assigns *mem \from val, indirect:model;
    assigns \result \from *mem, indirect:model;
 */
__UINT32_T __atomic_exchange_4(__UINT32_T* mem, __UINT32_T val, int model);

/*@ requires validity: \valid(mem);
    requires initialization: \initialized(mem);
    assigns *mem \from val, indirect:model;
    assigns \result \from *mem, indirect:model;
 */
__UINT64_T __atomic_exchange_8(__UINT64_T* mem, __UINT64_T val, int model);

/*@
  requires validity: \valid(mem) && \valid(expected);
  requires initialization:mem: \initialized(mem);
  requires initialization:expected: \initialized(expected);
  assigns *mem \from *mem, desired, indirect: *expected,
          indirect: success_model, indirect: weak;
  assigns *expected \from *expected, *mem, indirect: desired,
          indirect: failure_model, indirect: weak;
  assigns \result \from indirect: *mem, indirect: *expected;
*/
_Bool __atomic_compare_exchange_1(__UINT8_T* mem,
                                  __UINT8_T* expected,
                                  __UINT8_T desired,
                                  _Bool weak,
                                  int success_model,
                                  int failure_model);

/*@
  requires validity: \valid(mem) && \valid(expected);
  requires initialization:mem: \initialized(mem);
  requires initialization:expected: \initialized(expected);
  assigns *mem \from *mem, desired, indirect: *expected,
          indirect: success_model, indirect: weak;
  assigns *expected \from *expected, *mem, indirect: desired,
          indirect: failure_model, indirect: weak;
  assigns \result \from indirect: *mem, indirect: *expected;
*/
_Bool __atomic_compare_exchange_2(__UINT16_T* mem,
                                  __UINT16_T* expected,
                                  __UINT16_T desired,
                                  _Bool weak,
                                  int success_model,
                                  int failure_model);

/*@
  requires validity: \valid(mem) && \valid(expected);
  requires initialization:mem: \initialized(mem);
  requires initialization:expected: \initialized(expected);
  assigns *mem \from *mem, desired, indirect: *expected,
          indirect: success_model, indirect: weak;
  assigns *expected \from *expected, *mem, indirect: desired,
          indirect: failure_model, indirect: weak;
  assigns \result \from indirect: *mem, indirect: *expected;
*/
_Bool __atomic_compare_exchange_4(__UINT32_T* mem,
                                  __UINT32_T* expected,
                                  __UINT32_T desired,
                                  _Bool weak,
                                  int success_model,
                                  int failure_model);

/*@
  requires validity: \valid(mem) && \valid(expected);
  requires initialization:mem: \initialized(mem);
  requires initialization:expected: \initialized(expected);
  assigns *mem \from *mem, desired, indirect: *expected,
          indirect: success_model, indirect: weak;
  assigns *expected \from *expected, *mem, indirect: desired,
          indirect: failure_model, indirect: weak;
  assigns \result \from indirect: *mem, indirect: *expected;
*/
_Bool __atomic_compare_exchange_8(__UINT64_T* mem,
                                  __UINT64_T* expected,
                                  __UINT64_T desired,
                                  _Bool weak,
                                  int success_model,
                                  int failure_model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_add_fetch_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_sub_fetch_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_and_fetch_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_xor_fetch_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_or_fetch_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_nand_fetch_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_add_fetch_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_sub_fetch_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_and_fetch_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_xor_fetch_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_or_fetch_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_nand_fetch_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_add_fetch_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_sub_fetch_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_and_fetch_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_xor_fetch_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_or_fetch_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_nand_fetch_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_add_fetch_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_sub_fetch_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_and_fetch_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_xor_fetch_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_or_fetch_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_nand_fetch_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_fetch_add_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_fetch_sub_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_fetch_and_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_fetch_xor_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_fetch_or_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT8_T __atomic_fetch_nand_1(__UINT8_T* ptr, __UINT8_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_fetch_add_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_fetch_sub_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_fetch_and_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_fetch_xor_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_fetch_or_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT16_T __atomic_fetch_nand_2(__UINT16_T* ptr, __UINT16_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_fetch_add_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_fetch_sub_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_fetch_and_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_fetch_xor_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_fetch_or_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT32_T __atomic_fetch_nand_4(__UINT32_T* ptr, __UINT32_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_fetch_add_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_fetch_sub__8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_fetch_and_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_fetch_xor_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_fetch_or_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid(ptr);
    requires initialization: \initialized(ptr);
    assigns \result, *ptr \from *ptr, val, indirect: model;
*/
__UINT64_T __atomic_fetch_nand_8(__UINT64_T* ptr, __UINT64_T val, int model);

/*@ requires validity: \valid((char*)ptr);
    assigns \result \from *((char*)ptr);
    assigns *((char*)ptr) \from \nothing;
*/
_Bool __atomic_test_and_set(void* ptr, int model);

/*@ requires validity: \valid(ptr);
    assigns *ptr \from \nothing;
*/
void __atomic_clear(_Bool* ptr, int model);

/*@ assigns \nothing; */
void __atomic_thread_fence(int model);

/*@ assigns \nothing; */
void __atomic_signal_fence(int model);

/*@ assigns \result \from indirect: size, indirect: ptr; */
_Bool __atomic_always_lock_free(__SIZE_T size, void* ptr);

/*@ assigns \result \from indirect: size, indirect: ptr; */
_Bool __atomic_is_lock_free(__SIZE_T size, void* ptr);

// According to the GCC docs
// (https://gcc.gnu.org/onlinedocs/gcc/_005f_005fsync-Builtins.html),
// this creates a 'full memory barrier'; we do not model the concurrency
// aspects yet.
/*@ assigns \nothing; */
void __sync_synchronize(void);

__END_DECLS

__POP_FC_STDLIB
#endif
