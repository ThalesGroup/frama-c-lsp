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

#ifndef __FC_MONETARY
#define __FC_MONETARY
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_locale_t.h"
#include "__fc_define_size_t.h"
#include "__fc_define_ssize_t.h"

__BEGIN_DECLS

extern ssize_t strfmon(char *restrict s, size_t maxsize,
                       const char *restrict format, ...);

extern ssize_t strfmon_l(char *restrict s, size_t maxsize, locale_t locale,
                         const char *restrict format, ...);

__END_DECLS

__POP_FC_STDLIB
#endif
