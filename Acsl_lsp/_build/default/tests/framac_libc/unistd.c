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

#include "unistd.h"
#include "__fc_builtin.h"
#include "string.h"
__PUSH_FC_STDLIB

volatile char __fc_ttyname[TTY_NAME_MAX];

int optind = 1;
char *optarg;
int opterr = 1; // initial value is not zero (zero silences error messages)

int getopt(int argc, char * const argv[], const char *optstring) {
  if (argc == 0) {
    return -1;
  }
  int nondet_ind = Frama_C_interval(1, argc - 1);
  int nondet_indlen = Frama_C_interval(0, strlen(argv[nondet_ind])-1);
  optarg = Frama_C_nondet_ptr(0, &argv[nondet_ind][nondet_indlen]);
  optind = Frama_C_interval(1, argc + 1);
  return Frama_C_nondet(-1, Frama_C_unsigned_char_interval(0, UCHAR_MAX));
}

__POP_FC_STDLIB
