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

#ifndef __FC_SEARCH
#define __FC_SEARCH
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_size_t.h"

__BEGIN_DECLS

typedef struct entry {
  char    *key;
  void    *data;
} ENTRY;

typedef enum { FIND, ENTER } ACTION;
typedef enum { preorder, postorder, endorder, leaf } VISIT;

extern int hcreate(size_t nel);

extern void hdestroy(void);

extern ENTRY *hsearch(ENTRY item, ACTION action);

extern void insque(void *element, void *pred);

extern void *lfind(const void *key, const void *base, size_t *nelp,
                   size_t width, int (*compar)(const void *, const void *));

extern void *lsearch(const void *key, void *based, size_t *nelp,
                     size_t width, int (*compar)(const void *, const void *));

extern void remque(void *element);

extern void *tdelete(const void *restrict key, void **restrict,
                     int(*compar)(const void *, const void *));

extern void *tfind(const void *key, void *const *rootp,
                   int(*compar)(const void *, const void *));

extern void *tsearch(const void *key, void **rootp,
                     int(*compar)(const void *, const void *));

extern void twalk(const void *root,
                  void (*action)(const void *, VISIT, int ));

__END_DECLS

__POP_FC_STDLIB
#endif
