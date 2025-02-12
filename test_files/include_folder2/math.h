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

/* ISO C: 7.12 */
#ifndef __FC_MATH
#define __FC_MATH
#include "features.h"
__PUSH_FC_STDLIB

#include "__fc_string_axiomatic.h"
#include "errno.h"

__BEGIN_DECLS

typedef float float_t;
typedef double double_t;

#define MATH_ERRNO	1
#define MATH_ERREXCEPT	2

/* The constants below are not part of C99/C11 but they are defined in POSIX */
#define M_E 0x1.5bf0a8b145769p1         /* e          */
#define M_LOG2E 0x1.71547652b82fep0     /* log_2 e    */
#define M_LOG10E 0x1.bcb7b1526e50ep-2   /* log_10 e   */
#define M_LN2 0x1.62e42fefa39efp-1      /* log_e 2    */
#define M_LN10 0x1.26bb1bbb55516p1      /* log_e 10   */
#define M_PI 0x1.921fb54442d18p1        /* pi         */
#define M_PI_2 0x1.921fb54442d18p0      /* pi/2       */
#define M_PI_4 0x1.921fb54442d18p-1     /* pi/4       */
#define M_1_PI 0x1.45f306dc9c883p-2     /* 1/pi       */
#define M_2_PI 0x1.45f306dc9c883p-1     /* 2/pi       */
#define M_2_SQRTPI 0x1.20dd750429b6dp0  /* 2/sqrt(pi) */
#define M_SQRT2 0x1.6a09e667f3bcdp0     /* sqrt(2)    */
#define M_SQRT1_2 0x1.6a09e667f3bcdp-1  /* 1/sqrt(2)  */

/* The following specifications will set errno. */
#define math_errhandling	MATH_ERRNO

#define FP_NAN 0
#define FP_INFINITE 1
#define FP_ZERO 2
#define FP_SUBNORMAL 3
#define FP_NORMAL 4

#define FP_ILOGB0 __FC_INT_MIN
#define FP_ILOGBNAN __FC_INT_MIN

#include "float.h" // for DBL_MIN and FLT_MIN

/*@
  assigns \result \from x;
  behavior nan:
    assumes is_nan: \is_NaN(x);
    ensures fp_nan: \result == FP_NAN;
  behavior inf:
    assumes is_infinite: !\is_NaN(x) && !\is_finite(x);
    ensures fp_infinite: \result == FP_INFINITE;
  behavior zero:
    assumes is_a_zero: x == 0.0; // also includes -0.0
    ensures fp_zero: \result == FP_ZERO;
  behavior subnormal:
    assumes is_finite: \is_finite(x);
    assumes is_subnormal: (x > 0.0 && x < FLT_MIN) || (x < 0.0 && x > -FLT_MIN);
    ensures fp_subnormal: \result == FP_SUBNORMAL;
  behavior normal:
    assumes is_finite: \is_finite(x);
    assumes not_subnormal: (x <= -FLT_MIN || x >= FLT_MIN);
    ensures fp_normal: \result == FP_NORMAL;
  complete behaviors;
  disjoint behaviors;
 */
int __fc_fpclassifyf(float x);

/*@
  assigns \result \from x;
  behavior nan:
    assumes is_nan: \is_NaN(x);
    ensures fp_nan: \result == FP_NAN;
  behavior inf:
    assumes is_infinite: !\is_NaN(x) && !\is_finite(x);
    ensures fp_infinite: \result == FP_INFINITE;
  behavior zero:
    assumes is_a_zero: x == 0.0; // also includes -0.0
    ensures fp_zero: \result == FP_ZERO;
  behavior subnormal:
    assumes is_finite: \is_finite(x);
    assumes is_subnormal: (x > 0.0 && x < DBL_MIN) || (x < 0.0 && x > -DBL_MIN);
    ensures fp_subnormal: \result == FP_SUBNORMAL;
  behavior normal:
    assumes is_finite: \is_finite(x);
    assumes not_subnormal: (x <= -DBL_MIN || x >= DBL_MIN);
    ensures fp_normal: \result == FP_NORMAL;
  complete behaviors;
  disjoint behaviors;
 */
int __fc_fpclassify(double x);

// Incorrect in presence of long double with >64 bits
#define fpclassify(x) \
  (sizeof(x) == sizeof(float) ? __fc_fpclassifyf(x) : __fc_fpclassify(x))

#define isinf(x) \
  (sizeof(x) == sizeof(float) ? __fc_fpclassifyf(x) == FP_INFINITE : __fc_fpclassify(x) == FP_INFINITE)

#define isnan(x) \
  (sizeof(x) == sizeof(float) ? __fc_fpclassifyf(x) == FP_NAN : __fc_fpclassify(x) == FP_NAN)

#define isnormal(x) \
  (sizeof(x) == sizeof(float) ? __fc_fpclassifyf(x) == FP_NORMAL : __fc_fpclassify(x) == FP_NORMAL)


/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && \abs(x) <= 1;
    assigns \result \from x;
    ensures positive_result: \is_finite(\result) && \result >= 0;
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double acos(double x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && \abs(x) <= 1;
    assigns \result \from x;
    ensures positive_result: \is_finite(\result) && \result >= 0;
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float acosf(float x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && \abs(x) <= 1;
    assigns \result \from x;
    ensures positive_result: \is_finite(\result) && \result >= 0;
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern long double acosl(long double x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && \abs(x) <= 1;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double asin(double x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && \abs(x) <= 1;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float asinf(float x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && \abs(x) <= 1;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || (\is_finite(x) && \abs(x) > 1);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern long double asinl(long double x);

/*@
  assigns \result \from x;
  behavior normal:
    assumes number_arg: !\is_NaN(x);
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1.571 <= \result <= 1.571;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern float atanf(float x);

/*@
  assigns \result \from x;
  behavior normal:
    assumes number_arg: !\is_NaN(x);
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1.571 <= \result <= 1.571;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern double atan(double x);

/*@
  assigns \result \from x;
  behavior normal:
    assumes number_arg: !\is_NaN(x);
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1.571 <= \result <= 1.571;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern long double atanl(long double x);

/*@
  assigns \result \from x, y;
  behavior normal:
    assumes number_args: !\is_NaN(x) && !\is_NaN(y);
    ensures finite_result: \is_finite(\result);
  behavior nan:
    assumes nan_arg: \is_NaN(x) || \is_NaN(y);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern double atan2(double y, double x);

/*@
  assigns \result \from x, y;
  behavior normal:
    assumes number_args: !\is_NaN(x) && !\is_NaN(y);
    ensures finite_result: \is_finite(\result);
  behavior nan:
    assumes nan_arg: \is_NaN(x) || \is_NaN(y);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern float atan2f(float y, float x);

/*@
  assigns \result \from x, y;
  behavior normal:
    assumes number_args: !\is_NaN(x) && !\is_NaN(y);
    ensures finite_result: \is_finite(\result);
  behavior nan:
    assumes nan_arg: \is_NaN(x) || \is_NaN(y);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern long double atan2l(long double y, long double x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1. <= \result <= 1.;
    ensures no_error: errno == \old(errno);
  behavior infinity:
    assumes infinite_arg: \is_infinite(x);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double cos(double x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1. <= \result <= 1.;
    ensures no_error: errno == \old(errno);
  behavior infinity:
    assumes infinite_arg: \is_infinite(x);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float cosf(float x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1. <= \result <= 1.;
    ensures no_error: errno == \old(errno);
  behavior infinity:
    assumes infinite_arg: \is_infinite(x);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern long double cosl(long double x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1. <= \result <= 1.;
    ensures no_error: errno == \old(errno);
  behavior infinity:
    assumes infinite_arg: \is_infinite(x);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double sin(double x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1. <= \result <= 1.;
    ensures no_error: errno == \old(errno);
  behavior infinity:
    assumes infinite_arg: \is_infinite(x);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float sinf(float x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures result_domain: -1. <= \result <= 1.;
    ensures no_error: errno == \old(errno);
  behavior infinity:
    assumes infinite_arg: \is_infinite(x);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern long double sinl(long double x);

/* Note: the specs of tan/tanf below assume that, for a finite x,
 *       the result is always finite. This is _not_ guaranteed by the standard,
 *       but testing with the GNU libc, plus some mathematical arguments
 *       (see https://stackoverflow.com/questions/67482420) indicate that,
 *       in practice, the result is _never_ infinite.
 *       If you know of any implementations in which a finite argument
 *       produces an infinite result, please inform us.
 */
/*@
  assigns errno, \result \from x;
  behavior zero:
    assumes zero_arg: \is_finite(x) && x == 0.;
    assigns \result \from x;
    ensures zero_res: \is_finite(\result) && \result == x;
    ensures no_error: errno == \old(errno);
  behavior finite_non_zero:
    assumes finite_arg: \is_finite(x) && x != 0.;
    ensures finite_result: \is_finite(\result);
    ensures maybe_error: errno == \old(errno) || errno == ERANGE;
  behavior infinity:
    assumes infinite_arg: \is_infinite(x);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double tan(double x);

/*@
  assigns errno, \result \from x;
  behavior zero:
    assumes zero_arg: \is_finite(x) && x == 0.;
    assigns \result \from x;
    ensures zero_res: \is_finite(\result) && \result == x;
    ensures no_error: errno == \old(errno);
  behavior finite_non_zero:
    assumes finite_arg: \is_finite(x) && x != 0.;
    ensures finite_result: \is_finite(\result);
    ensures maybe_error: errno == \old(errno) || errno == ERANGE;
  behavior infinity:
    assumes infinite_arg: \is_infinite(x);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float tanf(float x);

/*@
  assigns errno, \result \from x;
  ensures maybe_error: errno == \old(errno) || errno == EDOM || errno == ERANGE;
*/
extern long double tanl(long double x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && x >= 1;
    assigns \result \from x;
    ensures positive_result: \is_finite(\result) && \result >= 0;
    ensures no_error: errno == \old(errno);
  behavior infinite:
    assumes is_plus_infinity: \is_plus_infinity(x);
    assigns \result \from x;
    ensures result_plus_infinity: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_minus_infinity(x) || (\is_finite(x) && x < 1);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
 */
extern double acosh(double x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && x >= 1;
    assigns \result \from x;
    ensures positive_result: \is_finite(\result) && \result >= 0;
    ensures no_error: errno == \old(errno);
  behavior infinite:
    assumes is_plus_infinity: \is_plus_infinity(x);
    assigns \result \from x;
    ensures result_plus_infinity: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_minus_infinity(x) || (\is_finite(x) && x < 1);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
 */
extern float acoshf(float x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes in_domain: \is_finite(x) && x >= 1;
    assigns \result \from x;
    ensures positive_result: \is_finite(\result) && \result >= 0;
    ensures no_error: errno == \old(errno);
  behavior infinite:
    assumes is_plus_infinity: \is_plus_infinity(x);
    assigns \result \from x;
    ensures result_plus_infinity: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_minus_infinity(x) || (\is_finite(x) && x < 1);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
 */
extern long double acoshl(long double x);

extern double asinh(double x);
extern float asinhf(float x);
extern long double asinhl(long double x);

extern double atanh(double x);
extern float atanhf(float x);
extern long double atanhl(long double x);

extern double cosh(double x);
extern float coshf(float x);
extern long double coshl(long double x);

extern double sinh(double x);
extern float sinhf(float x);
extern long double sinhl(long double x);

extern double tanh(double x);
extern float tanhf(float x);
extern long double tanhl(long double x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assumes domain_arg: x >= -0x1.74910d52d3051p9 && x <= 0x1.62e42fefa39efp+9;
    assigns \result \from x;
    ensures res_finite: \is_finite(\result);
    ensures positive_result: \result > 0.;
    ensures no_error: errno == \old(errno);
  behavior overflow:
    assumes overflow_arg: \is_finite(x) && x > 0x1.62e42fefa39efp+9;
    ensures infinite_res: \is_plus_infinity(\result);
    ensures errno_set: errno == ERANGE;
  behavior plus_infinity:
    assumes plus_infinity_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinity_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior underflow:
    assumes underflow_arg: \is_finite(x) && x < -0x1.74910d52d3051p9;
    ensures zero_res: \result == 0.;
    ensures errno_set: errno == ERANGE;
  behavior minus_infinity:
    assumes plus_infinity_arg: \is_minus_infinity(x);
    assigns \result \from x;
    ensures zero_result: \is_finite(\result) && \result == 0.;
    ensures no_error: errno == \old(errno);
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double exp(double x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assumes domain_arg: x >= -0x1.9fe368p6 && x <= 0x1.62e42ep+6;
    assigns \result \from x;
    ensures res_finite: \is_finite(\result);
    ensures positive_result: \result > 0.;
    ensures no_error: errno == \old(errno);
  behavior overflow:
    assumes overflow_arg: \is_finite(x) && x > 0x1.62e42ep+6;
    ensures infinite_res: \is_plus_infinity(\result);
    ensures errno_set: errno == ERANGE;
  behavior plus_infinity:
    assumes plus_infinity_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinity_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior underflow:
    assumes underflow_arg: \is_finite(x) && x < -0x1.9fe368p6;
    ensures zero_res: \result == 0.;
    ensures errno_set: errno == ERANGE;
  behavior minus_infinity:
    assumes plus_infinity_arg: \is_minus_infinity(x);
    assigns \result \from x;
    ensures zero_result: \is_finite(\result) && \result == 0.;
    ensures no_error: errno == \old(errno);
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float expf(float x);

extern long double expl(long double x);

extern double exp2(double x);
extern float exp2f(float x);
extern long double exp2l(long double x);

extern double expm1(double x);
extern float expm1f(float x);
extern long double expm1l(long double x);

/*@
  requires valid_exp: \valid(exp);
  assigns \result, *exp \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assumes arg_nonzero: x != 0.0;
    ensures finite_result: \is_finite(\result);
    ensures bounded_result: 0.5 <= \result < 1.0;
    ensures initialization:exp: \initialized(exp);
  behavior infinite:
    assumes infinite_arg: \is_plus_infinity(x) || \is_minus_infinity(x);
    ensures infinite_result: \is_infinite(\result);
    ensures result_same_infinite: \result == x;
  behavior zero:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures finite_result: \is_finite(\result);
    ensures zero_result: \result == 0.0;
    ensures initialization:exp: \initialized(exp);
    ensures zero_exp: *exp == 0;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern double frexp(double x, int *exp);

/*@
  requires valid_exp: \valid(exp);
  assigns \result, *exp \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assumes arg_nonzero: x != 0.0;
    ensures finite_result: \is_finite(\result);
    ensures bounded_result: 0.5 <= \result < 1.0;
    ensures initialization:exp: \initialized(exp);
  behavior infinite:
    assumes infinite_arg: \is_plus_infinity(x) || \is_minus_infinity(x);
    ensures infinite_result: \is_infinite(\result);
    ensures result_same_infinite: \result == x;
  behavior zero:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures finite_result: \is_finite(\result);
    ensures zero_result: \result == x;
    ensures initialization:exp: \initialized(exp);
    ensures zero_exp: *exp == 0;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern float frexpf(float x, int *exp);

/*@
  requires valid_exp: \valid(exp);
  assigns \result, *exp \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assumes arg_nonzero: x != 0.0;
    ensures finite_result: \is_finite(\result);
    ensures bounded_result: 0.5 <= \result < 1.0;
    ensures initialization:exp: \initialized(exp);
  behavior infinite:
    assumes infinite_arg: \is_plus_infinity(x) || \is_minus_infinity(x);
    ensures infinite_result: \is_infinite(\result);
    ensures result_same_infinite: \result == x;
  behavior zero:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures finite_result: \is_finite(\result);
    ensures zero_result: \result == x;
    ensures initialization:exp: \initialized(exp);
    ensures zero_exp: *exp == 0;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern long double frexpl(long double x, int *exp);

extern int ilogb(double x);
extern int ilogbf(float x);
extern int ilogbl(long double x);

/*@
  assigns errno, \result \from x, exp;
  behavior normal:
    assumes finite_logic_res: \is_finite((double)(x * pow(2.0d, (double)exp)));
    ensures finite_result: \is_finite(\result);
    ensures errno: errno == ERANGE || errno == \old(errno); //ERANGE if underflow
  behavior overflow_not_nan:
    assumes not_nan_arg: !\is_NaN(x);
    assumes infinite_logic_res: !\is_finite((double)(x * pow(2.0d, (double)exp)));
    ensures infinite_result: \is_infinite(\result);
    ensures errno: errno == ERANGE || errno == \old(errno);
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern double ldexp(double x, int exp);

/*@
  assigns errno, \result \from x, exp;
  behavior normal:
    assumes finite_logic_res: \is_finite((float)(x * pow(2.0f, (float)exp)));
    ensures finite_result: \is_finite(\result);
    ensures errno: errno == ERANGE || errno == \old(errno); //ERANGE if underflow
  behavior overflow_not_nan:
    assumes not_nan_arg: !\is_NaN(x);
    assumes infinite_logic_res: !\is_finite((float)(x * pow(2.0f, (float)exp)));
    ensures infinite_result: \is_infinite(\result);
    ensures errno: errno == ERANGE || errno == \old(errno);
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern float ldexpf(float x, int exp);

extern long double ldexpl(long double x, int exp);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior infinite:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior zero:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures infinite_result: \is_minus_infinity(\result); // -HUGE_VAL
    ensures errno_set: errno == ERANGE;
  behavior negative:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double log(double x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior infinite:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior zero:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures infinite_result: \is_minus_infinity(\result); // -HUGE_VALF
    ensures errno_set: errno == ERANGE;
  behavior negative:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float logf(float x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior infinite:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior zero:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures infinite_result: \is_minus_infinity(\result); // -HUGE_VALL
    ensures errno_set: errno == ERANGE;
  behavior negative:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern long double logl(long double x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior infinite:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior zero:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures infinite_result: \is_minus_infinity(\result); // -HUGE_VAL
    ensures errno_set: errno == ERANGE;
  behavior negative:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double log10(double x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior infinite:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior zero:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures infinite_result: \is_minus_infinity(\result); // -HUGE_VALF
    ensures errno_set: errno == ERANGE;
  behavior negative:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float log10f(float x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior infinite:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior zero:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures infinite_result: \is_minus_infinity(\result); // -HUGE_VALL
    ensures errno_set: errno == ERANGE;
  behavior negative:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern long double log10l(long double x);

extern double log1p(double x);
extern float log1pf(float x);
extern long double log1pl(long double x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior infinite:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior zero:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures infinite_result: \is_minus_infinity(\result); // -HUGE_VAL
    ensures errno_set: errno == ERANGE;
  behavior negative:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double log2(double x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior infinite:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior zero:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures infinite_result: \is_minus_infinity(\result); // -HUGE_VALF
    ensures errno_set: errno == ERANGE;
  behavior negative:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float log2f(float x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x > 0;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior infinite:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior zero:
    assumes zero_arg: \is_finite(x) && x == 0.;
    ensures infinite_result: \is_minus_infinity(\result); // -HUGE_VALL
    ensures errno_set: errno == ERANGE;
  behavior negative:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern long double log2l(long double x);

extern double logb(double x);
extern float logbf(float x);
extern long double logbl(long double x);

extern double modf(double value, double *iptr);
extern float modff(float value, float *iptr);
extern long double modfl(long double value, long double *iptr);

extern double scalbn(double x, int n);
extern float scalbnf(float x, int n);
extern long double scalbnl(long double x, int n);

extern double scalbln(double x, long int n);
extern float scalblnf(float x, long int n);
extern long double scalblnl(long double x, long int n);

extern double cbrt(double x);
extern float cbrtf(float x);
extern long double cbrtl(long double x);

/*@
  assigns \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    ensures res_finite: \is_finite(\result);
    ensures positive_result: \result >= 0.;
    ensures equal_magnitude_result: \result == x || \result == -x;
  behavior infinity:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_plus_infinity(\result);
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern double fabs(double x);

/*@
  assigns \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    ensures res_finite: \is_finite(\result);
    ensures positive_result: \result >= 0.;
    ensures equal_magnitude_result: \result == x || \result == -x;
  behavior infinity:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_plus_infinity(\result);
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern float fabsf(float x);

/*@
  assigns \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    ensures res_finite: \is_finite(\result);
    ensures positive_result: \result >= 0.;
    ensures equal_magnitude_result: \result == x || \result == -x;
  behavior infinity:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_plus_infinity(\result);
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern long double fabsl(long double x);

extern double hypot(double x, double y);
extern float hypotf(float x, float y);
extern long double hypotl(long double x, long double y);

/*@
  assigns errno, \result \from x, y;
  behavior normal:
    assumes finite_logic_res: \is_finite(pow(x, y));
    ensures finite_result: \is_finite(\result);
    ensures errno: errno == ERANGE || errno == \old(errno);
  behavior overflow:
    assumes infinite_logic_res: !\is_finite(pow(x, y));
    ensures infinite_result: !\is_finite(\result);
    ensures errno: errno == ERANGE || errno == EDOM || errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double pow(double x, double y);

/*@
  assigns errno, \result \from x, y;
  behavior normal:
    assumes finite_logic_res: \is_finite(pow(x, y));
    ensures finite_result: \is_finite(\result);
    ensures errno: errno == ERANGE || errno == \old(errno);
  behavior overflow:
    assumes infinite_logic_res: !\is_finite(pow(x, y));
    ensures infinite_result: !\is_finite(\result);
    ensures errno: errno == ERANGE || errno == EDOM || errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float powf(float x, float y);

extern long double powl(long double x, long double y);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x >= -0.;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures positive_result: \result >= -0.;
    ensures result_value: \result == sqrt(x);
    ensures no_error: errno == \old(errno);
  behavior negative:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior infinity:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double sqrt(double x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x >= -0.;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures positive_result: \result >= -0.;
    ensures result_value: \result == sqrt(x);
    ensures no_error: errno == \old(errno);
  behavior negative:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior infinity:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float sqrtf(float x);

/*@
  assigns errno, \result \from x;
  behavior normal:
    assumes finite_arg: \is_finite(x);
    assumes arg_positive: x >= -0.;
    assigns \result \from x;
    ensures finite_result: \is_finite(\result);
    ensures positive_result: \result >= -0.;
    ensures no_error: errno == \old(errno);
  behavior negative:
    assumes negative_arg: \is_minus_infinity(x) || (\is_finite(x) && x < -0.);
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior infinity:
    assumes infinite_arg: \is_plus_infinity(x);
    assigns \result \from x;
    ensures infinite_result: \is_plus_infinity(\result);
    ensures no_error: errno == \old(errno);
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    assigns \result \from x;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern long double sqrtl(long double x);

extern double erf(double x);
extern float erff(float x);
extern long double erfl(long double x);

extern double erfc(double x);
extern float erfcf(float x);
extern long double erfcl(long double x);

extern double lgamma(double x);
extern float lgammaf(float x);
extern long double lgammal(long double x);

extern double tgamma(double x);
extern float tgammaf(float x);
extern long double tgammal(long double x);

/*@
  assigns \result \from x;
  behavior finite:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern double ceil(double x);

/*@
  assigns \result \from x;
  behavior finite:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern float ceilf(float x);

/*@
  assigns \result \from x;
  behavior finite:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern long double ceill(long double x);

/*@
  assigns \result \from x;
  behavior finite:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern double floor(double x);

/*@
  assigns \result \from x;
  behavior finite:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern float floorf(float x);

/*@
  assigns \result \from x;
  behavior finite:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern long double floorl(long double x);

extern double nearbyint(double x);
extern float nearbyintf(float x);
extern long double nearbyintl(long double x);

extern double rint(double x);
extern float rintf(float x);
extern long double rintl(long double x);

extern long int lrint(double x);
extern long int lrintf(float x);
extern long int lrintl(long double x);

extern long long int llrint(double x);
extern long long int llrintf(float x);
extern long long int llrintl(long double x);

/*@
  assigns \result \from x;
  behavior finite:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern double round(double x);

/*@
  assigns \result \from x;
  behavior finite:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern float roundf(float x);

/*@
  assigns \result \from x;
  behavior finite:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern long double roundl(long double x);

extern long int lround(double x);
extern long int lroundf(float x);
extern long int lroundl(long double x);

extern long long int llround(double x);
extern long long int llroundf(float x);
extern long long int llroundl(long double x);

/*@
  assigns \result \from x;
  behavior finite:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern double trunc(double x);

/*@
  assigns \result \from x;
  behavior finite:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern float truncf(float x);

/*@
  assigns \result \from x;
  behavior finite:
    assumes finite_arg: \is_finite(x);
    ensures finite_result: \is_finite(\result);
  behavior infinite:
    assumes infinite_arg: \is_infinite(x);
    ensures infinite_result: \is_infinite(\result);
    ensures equal_result: \result == x;
  behavior nan:
    assumes nan_arg: \is_NaN(x);
    ensures nan_result: \is_NaN(\result);
  complete behaviors;
  disjoint behaviors;
*/
extern long double truncl(long double x);

/*@
  assigns errno, \result \from x, y;
  behavior normal:
    assumes in_domain: !\is_NaN(x) && !\is_NaN(y) && y != 0.;
    assigns \result \from x, y;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || y == 0.;
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_args: \is_NaN(x) || \is_NaN(y);
    assigns \result \from x, y;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern double fmod(double x, double y);

/*@
  assigns errno, \result \from x, y;
  behavior normal:
    assumes in_domain: !\is_NaN(x) && !\is_NaN(y) && y != 0.;
    assigns \result \from x, y;
    ensures finite_result: \is_finite(\result);
    ensures no_error: errno == \old(errno);
  behavior domain_error:
    assumes out_of_domain: \is_infinite(x) || y == 0.;
    ensures nan_result: \is_NaN(\result);
    ensures errno_set: errno == EDOM;
  behavior nan:
    assumes nan_args: \is_NaN(x) || \is_NaN(y);
    assigns \result \from x, y;
    ensures nan_result: \is_NaN(\result);
    ensures no_error: errno == \old(errno);
  complete behaviors;
  disjoint behaviors;
*/
extern float fmodf(float x, float y);

extern long double fmodl(long double x, long double y);

extern double remainder(double x, double y);
extern float remainderf(float x, float y);
extern long double remainderl(long double x, long double y);

extern double remquo(double x, double y, int *quo);
extern float remquof(float x, float y, int *quo);
extern long double remquol(long double x, long double y, int *quo);

extern double copysign(double x, double y);
extern float copysignf(float x, float y);
extern long double copysignl(long double x, long double y);

/*@
  requires tagp_valid_string: valid_read_string(tagp);
  assigns \result \from indirect:tagp[0..];
  ensures result_is_nan: \is_NaN(\result);
 */
extern double nan(const char *tagp);

/*@
  requires tagp_valid_string: valid_read_string(tagp);
  assigns \result \from indirect:tagp[0..];
  ensures result_is_nan: \is_NaN(\result);
 */
extern float nanf(const char *tagp);

/*@
  requires tagp_valid_string: valid_read_string(tagp);
  assigns \result \from indirect:tagp[0..];
  ensures result_is_nan: \is_NaN(\result);
 */
extern long double nanl(const char *tagp);

extern double nextafter(double x, double y);
extern float nextafterf(float x, float y);
extern long double nextafterl(long double x, long double y);

extern double nexttoward(double x, long double y);
extern float nexttowardf(float x, long double y);
extern long double nexttowardl(long double x, long double y);

extern double fdim(double x, double y);
extern float fdimf(float x, float y);
extern long double fdiml(long double x, long double y);

extern double fmax(double x, double y);
extern float fmaxf(float x, float y);
extern long double fmaxl(long double x, long double y);

extern double fmin(double x, double y);
extern float fminf(float x, float y);
extern long double fminl(long double x, long double y);

extern double fma(double x, double y, double z);
extern float fmaf(float x, float y, float z);
extern long double fmal(long double x, long double y, long double z);

/*@
  assigns \result \from f;
  behavior finite:
    assumes isfinite_f: \is_finite(f);
    ensures nonzero_result: \result > 0 || \result < 0;
  behavior nonfinite:
    assumes nonfinite_f: !\is_finite(f);
    ensures zero_result: \result == 0;
  complete behaviors;
  disjoint behaviors;
*/
extern int __finitef(float f);

/*@
  assigns \result \from d;
  behavior finite:
    assumes isfinite_d: \is_finite(d);
    ensures nonzero_result: \result > 0 || \result < 0;
  behavior nonfinite:
    assumes nonfinite_d: !\is_finite(d);
    ensures zero_result: \result == 0;
  complete behaviors;
  disjoint behaviors;
*/
extern int __finite(double d);

#  define isfinite(x) \
     (sizeof (x) == sizeof (float) ? __finitef (x) : __finite (x))

//The (integer x) argument is just here because a function without argument is
//applied differently in ACSL and C

/*@

  logic float __fc_infinity(integer x) = \plus_infinity;
  logic float __fc_nan(integer x) = \NaN;

  @*/

/*@
  ensures result_is_infinity: \is_plus_infinity(\result);
  assigns \result \from \nothing;
  @*/
extern float __fc_infinity(int x);

/*@
  ensures result_is_nan: \is_NaN(\result);
  assigns \result \from \nothing;
  @*/
extern float __fc_nan(int x);


#define INFINITY __fc_infinity(0)
#define NAN __fc_nan(0)

#define HUGE_VALF INFINITY
#define HUGE_VAL  ((double)INFINITY)
#define HUGE_VALL ((long double)INFINITY)


__END_DECLS

__POP_FC_STDLIB
#endif
