/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 1995-2023                                               */
/*    Free Software Foundation, Inc.                                      */
/*  Copyright (C) 2021-2023                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  This file is derived from the GNU C Library. You can redistribute it  */
/*  and/or modify it under the terms of the GNU Lesser General Public     */
/*  License as published by the Free Software Foundation, version 2.1.    */
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

// Non-POSIX; this file is defined in the GNU libc.

#ifndef __FC_ARGZ_H
#define __FC_ARGZ_H

#include "features.h"
__PUSH_FC_STDLIB

#include "errno.h"
#include "stdlib.h"
#include "string.h"

__BEGIN_DECLS

typedef int error_t;

extern error_t argz_create (char *const argv[], char **restrict argz,
                            size_t *restrict len);

/*@ assigns \result, *argz[..] \from \nothing; */
extern error_t argz_create_sep (const char *restrict string,
                                int sep, char **restrict argz,
                                size_t *restrict len);

extern size_t argz_count (const char *argz, size_t len);

extern void argz_extract (const char *restrict argz, size_t len,
                          char **restrict argv);

extern void argz_stringify (char *argz, size_t len, int sep);

extern error_t argz_append (char **restrict argz,
                            size_t *restrict argz_len,
                            const char *restrict buf, size_t buf_len);

extern error_t argz_add (char **restrict argz,
                         size_t *restrict argz_len,
                         const char *restrict str);

extern error_t argz_add_sep (char **restrict argz,
                             size_t *restrict argz_len,
                             const char *restrict string, int delim);

extern void argz_delete (char **restrict argz,
                         size_t *restrict argz_len,
                         char *restrict entry);

extern error_t argz_insert (char **restrict argz,
                            size_t *restrict argz_len,
                            char *restrict before,
                            const char *restrict entry);

extern error_t argz_replace (char **restrict argz,
                             size_t *restrict argz_len,
                             const char *restrict str,
                             const char *restrict with,
                             unsigned int *restrict replace_count);

extern char *argz_next (const char *restrict argz, size_t argz_len,
                        const char *restrict entry);

__END_DECLS

__POP_FC_STDLIB
#endif
