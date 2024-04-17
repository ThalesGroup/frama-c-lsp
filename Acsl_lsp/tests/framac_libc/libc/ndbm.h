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

#ifndef __FC_NDBM
#define __FC_NDBM
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_mode_t.h"
#include "__fc_define_size_t.h"

__BEGIN_DECLS

typedef struct __fc_datum {
  void *dptr;
  size_t dsize;
} datum;

typedef int DBM;

#define DBM_INSERT 0
#define DBM_REPLACE 1

extern int dbm_clearerr(DBM *db);

extern void dbm_close(DBM *db);

extern int dbm_delete(DBM *db, datum key);

extern int dbm_error(DBM *db);

extern datum dbm_fetch(DBM *db, datum key);

extern datum dbm_firstkey(DBM *db);

extern datum dbm_nextkey(DBM *db);

extern DBM *dbm_open(const char *file, int open_flags, mode_t file_mode);

extern int dbm_store(DBM *db, datum key, datum content, int store_mode);

__END_DECLS

__POP_FC_STDLIB
#endif
