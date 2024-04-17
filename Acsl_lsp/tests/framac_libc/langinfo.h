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

#ifndef __FC_LANGINFO_H
#define __FC_LANGINFO_H
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_locale_t.h"
#include "nl_types.h"
#include "locale.h" // for LC_CTYPE and other constants
__BEGIN_DECLS


#define _NL_ITEM(category, index)	(((category) << 16) | (index))

/* Extract the category and item index from a constructed `nl_item' value.  */
#define _NL_ITEM_CATEGORY(item)		((int) (item) >> 16)
#define _NL_ITEM_INDEX(item)		((int) (item) & 0xffff)

enum
{
  CODESET = (LC_CTYPE) << 16,
#define CODESET CODESET
  ABDAY_1 = (LC_TIME) << 16,
#define ABDAY_1 ABDAY_1
  ABDAY_2,
#define ABDAY_2 ABDAY_2
  ABDAY_3,
#define ABDAY_3 ABDAY_3
  ABDAY_4,
#define ABDAY_4 ABDAY_4
  ABDAY_5,
#define ABDAY_5 ABDAY_5
  ABDAY_6,
#define ABDAY_6 ABDAY_6
  ABDAY_7,
#define ABDAY_7 ABDAY_7
  DAY_1,
#define DAY_1 DAY_1
  DAY_2,
#define DAY_2 DAY_2
  DAY_3,
#define DAY_3 DAY_3
  DAY_4,
#define DAY_4 DAY_4
  DAY_5,
#define DAY_5 DAY_5
  DAY_6,
#define DAY_6 DAY_6
  DAY_7,
#define DAY_7 DAY_7
  ABMON_1,
#define ABMON_1 ABMON_1
  ABMON_2,
#define ABMON_2 ABMON_2
  ABMON_3,
#define ABMON_3 ABMON_3
  ABMON_4,
#define ABMON_4 ABMON_4
  ABMON_5,
#define ABMON_5 ABMON_5
  ABMON_6,
#define ABMON_6 ABMON_6
  ABMON_7,
#define ABMON_7 ABMON_7
  ABMON_8,
#define ABMON_8 ABMON_8
  ABMON_9,
#define ABMON_9 ABMON_9
  ABMON_10,
#define ABMON_10 ABMON_10
  ABMON_11,
#define ABMON_11 ABMON_11
  ABMON_12,
#define ABMON_12 ABMON_12
  MON_1,
#define MON_1 MON_1
  MON_2,
#define MON_2 MON_2
  MON_3,
#define MON_3 MON_3
  MON_4,
#define MON_4 MON_4
  MON_5,
#define MON_5 MON_5
  MON_6,
#define MON_6 MON_6
  MON_7,
#define MON_7 MON_7
  MON_8,
#define MON_8 MON_8
  MON_9,
#define MON_9 MON_9
  MON_10,
#define MON_10 MON_10
  MON_11,
#define MON_11 MON_11
  MON_12,
#define MON_12 MON_12
  AM_STR,
#define AM_STR AM_STR
  PM_STR,
#define PM_STR PM_STR
  D_T_FMT,
#define D_T_FMT D_T_FMT
  D_FMT,
#define D_FMT D_FMT
  T_FMT,
#define T_FMT T_FMT
  T_FMT_AMPM,
#define T_FMT_AMPM T_FMT_AMPM
  ERA,
#define ERA ERA
  ERA_YEAR, // Non-POSIX; GNU
#define ERA_YEAR ERA_YEAR
  ERA_D_FMT,
#define ERA_D_FMT ERA_D_FMT
  ALT_DIGITS,
#define ALT_DIGITS ALT_DIGITS
  ERA_D_T_FMT,
#define ERA_D_T_FMT ERA_D_T_FMT
  ERA_T_FMT,
#define ERA_T_FMT ERA_T_FMT
  DECIMAL_POINT = (LC_NUMERIC) << 16, // Non-POSIX; GNU
#define DECIMAL_POINT DECIMAL_POINT
  RADIXCHAR,
#define RADIXCHAR RADIXCHAR
  THOUSEP,
#define THOUSEP THOUSEP
  YESEXPR = (LC_MESSAGES) << 16,
#define YESEXPR YESEXPR
  NOEXPR,
#define NOEXPR NOEXPR
  CRNCYSTR = (LC_MONETARY) << 16,
#define CRNCYSTR CRNCYSTR
};

extern char *nl_langinfo(nl_item item);
extern char *nl_langinfo_l(nl_item item, locale_t locale);

__END_DECLS

__POP_FC_STDLIB
#endif
