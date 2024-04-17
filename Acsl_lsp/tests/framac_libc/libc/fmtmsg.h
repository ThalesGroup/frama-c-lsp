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

#ifndef __FC_FMTMSG
#define __FC_FMTMSG
#include "features.h"
__PUSH_FC_STDLIB

__BEGIN_DECLS

#define MM_HARD 0x001
#define MM_SOFT 0x002
#define MM_FIRM 0x004
#define MM_APPL MM_APPL
#define MM_UTIL 0x010
#define MM_OPSYS 0x020
#define MM_RECOVER 0x040
#define MM_NRECOV 0x080
#define MM_HALT 1
#define MM_ERROR 2
#define MM_WARNING 3
#define MM_INFO 4
#define MM_NOSEV 0
#define MM_PRINT 0x100
#define MM_CONSOLE 0x200

#define MM_NULLLBL ((char*)0)
#define MM_NULLSEV 0
#define MM_NULLMC 0L
#define MM_NULLTXT ((char*)0)
#define MM_NULLACT ((char*)0)
#define MM_NULLTAG ((char*)0)

#define MM_OK 0
#define MM_NOTOK (-1)
#define MM_NOMSG 1
#define MM_NOCON 4

extern int fmtmsg(long classification, const char *label, int severity,
                  const char *text, const char *action, const char *tag);

__END_DECLS

__POP_FC_STDLIB
#endif
