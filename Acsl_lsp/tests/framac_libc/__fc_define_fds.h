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

#ifndef __FC_DEFINE_FDS
#define __FC_DEFINE_FDS
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

// arbitrary number
#ifndef __FC_MAX_OPEN_FILES
#define __FC_MAX_OPEN_FILES 1024
#endif

// __fc_fds represents the state of open file descriptors.
extern volatile int __fc_fds[__FC_MAX_OPEN_FILES];

__END_DECLS

__POP_FC_STDLIB
#endif
