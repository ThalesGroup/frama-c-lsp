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

// Non-POSIX and not in the C ISO standard; this file is defined in the
// GNU libc.

#ifndef __FC_ERR_H
#define __FC_ERR_H

#include "features.h"
__PUSH_FC_STDLIB

#include "__fc_string_axiomatic.h"
#include "stdarg.h"

__BEGIN_DECLS

// TODO: extend Variadic to handle these functions, or provide C stubs.
// Currently, given their limited usage and non-standard status, this
// lightweight approach seems better suited.

/*@
  requires fmt_valid_read_or_null: valid_read_string(fmt) || fmt == \null;
  assigns \exit_status \from eval;
  ensures never_terminates: \false;
 */
void err(int eval, const char *fmt, ...);

/*@
  requires fmt_valid_read_or_null: valid_read_string(fmt) || fmt == \null;
  assigns \exit_status \from eval;
  ensures never_terminates: \false;
 */
void errx(int eval, const char *fmt, ...);

/*@
  requires fmt_valid_read_or_null: valid_read_string(fmt) || fmt == \null;
  assigns \nothing;
 */
void warn(const char *fmt, ...);

/*@
  requires fmt_valid_read_or_null: valid_read_string(fmt) || fmt == \null;
  assigns \nothing;
 */
void warnx(const char *fmt, ...);

/*@
  requires fmt_valid_read_or_null: valid_read_string(fmt) || fmt == \null;
  assigns \exit_status \from eval;
  ensures never_terminates: \false;
 */
void verr(int eval, const char *fmt, va_list args);

/*@
  requires fmt_valid_read_or_null: valid_read_string(fmt) || fmt == \null;
  assigns \exit_status \from eval;
  ensures never_terminates: \false;
 */
void verrx(int eval, const char *fmt, va_list args);

/*@
  requires fmt_valid_read_or_null: valid_read_string(fmt) || fmt == \null;
  assigns \nothing;
 */
void vwarn(const char *fmt, va_list args);

/*@
  requires fmt_valid_read_or_null: valid_read_string(fmt) || fmt == \null;
  assigns \nothing;
 */
void vwarnx(const char *fmt, va_list args);

__END_DECLS

__POP_FC_STDLIB
#endif
