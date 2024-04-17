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

#ifndef __FC_WORDEXP
#define __FC_WORDEXP
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_size_t.h"

__BEGIN_DECLS

typedef struct __fc_wordexp_t {
  size_t we_wordc;
  char **we_wordv;
  size_t we_offs;
} wordexp_t;

#define WRDE_DOOFFS (1 << 0)
#define WRDE_APPEND (1 << 1)
#define WRDE_NOCMD (1 << 2)
#define WRDE_REUSE (1 << 3)
#define WRDE_SHOWERR (1 << 4)
#define WRDE_UNDEF (1 << 5)

#define WRDE_NOSPACE 1
#define WRDE_BADCHAR 2
#define WRDE_BADVAL 3
#define WRDE_CMDSUB 4
#define WRDE_SYNTAX 5

extern int wordexp(const char *restrict words, wordexp_t *restrict pwordexp,
                   int flags);

extern void wordfree(wordexp_t *pwordexp);

__END_DECLS

__POP_FC_STDLIB
#endif
