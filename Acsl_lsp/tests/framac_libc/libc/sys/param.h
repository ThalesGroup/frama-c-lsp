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

#ifndef __FC_SYS_PARAM_H__
#define __FC_SYS_PARAM_H__
#include "../features.h"
__PUSH_FC_STDLIB

// Note: sys/param.h is not a POSIX file. This is provided as a best-effort
// basis to support projects using constants such as PATH_MAX, which should
// be defined in "limits.h" according to POSIX. For instance, in Linux,
// PATH_MAX is defined in the non-POSIX file linux/limits.h.

#include "../limits.h"

__POP_FC_STDLIB
#endif
