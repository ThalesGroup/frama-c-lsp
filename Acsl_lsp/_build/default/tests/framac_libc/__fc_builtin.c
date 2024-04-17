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

#include "__fc_builtin.h"
__PUSH_FC_STDLIB
#include "limits.h"

/* Those builtins implementations could probably be removed entirely for
   Value, as the spec is informative enough. There remains a slight difference
   with Frama_C_float/double_interval and +0./-0., because the specification
   is not sufficient to to exclude -0. when requiring >= +0. */

int volatile Frama_C_entropy_source;

// Additional entropy sources for some interval functions
unsigned int volatile __fc_unsigned_int_entropy;
long volatile __fc_long_entropy;
unsigned long volatile __fc_unsigned_long_entropy;

//@ assigns Frama_C_entropy_source \from Frama_C_entropy_source;
void Frama_C_update_entropy(void) {
  Frama_C_entropy_source = Frama_C_entropy_source;
}

void Frama_C_make_unknown(char *p, size_t l) {
  Frama_C_update_entropy();
  for (size_t i = 0; i < l; i++) {
    p[i] = Frama_C_entropy_source;
  }
}

int Frama_C_nondet(int a, int b)
{
  Frama_C_update_entropy();
  return Frama_C_entropy_source ? a : b;
}

void *Frama_C_nondet_ptr(void *a, void *b)
{
  Frama_C_update_entropy();
  return Frama_C_entropy_source ? a : b;
}

int Frama_C_interval(int min, int max)
{
  int r, aux;
  Frama_C_update_entropy();
  aux = Frama_C_entropy_source;
  if (aux >= min && aux <= max)
    r = aux;
  else
    r = min;
  return r;
}

char Frama_C_char_interval(char min, char max)
{
  return (char)Frama_C_interval(min, max);
}

float Frama_C_float_interval(float min, float max)
{
  Frama_C_update_entropy();
  return Frama_C_entropy_source ? min : max;
}

double Frama_C_double_interval(double min, double max)
{
  Frama_C_update_entropy();
  return Frama_C_entropy_source ? min : max;
}

unsigned char Frama_C_unsigned_char_interval(unsigned char min, unsigned char max)
{
  return (unsigned char)Frama_C_interval(min, max);
}

unsigned int Frama_C_unsigned_int_interval(unsigned int min, unsigned int max)
{
  unsigned int r, aux;
  Frama_C_update_entropy();
  aux = __fc_unsigned_int_entropy;
  if (aux >= min && aux <= max)
    r = aux;
  else
    r = min;
  return r;
}

long Frama_C_long_interval(long min, long max)
{
  long r, aux;
  Frama_C_update_entropy();
  aux = __fc_long_entropy;
  if (aux >= min && aux <= max)
    r = aux;
  else
    r = min;
  return r;
}

unsigned long Frama_C_unsigned_long_interval(unsigned long min, unsigned long max)
{
  unsigned long r, aux;
  Frama_C_update_entropy();
  aux = __fc_unsigned_long_entropy;
  if (aux >= min && aux <= max)
    r = aux;
  else
    r = min;
  return r;
}

extern void __builtin_abort(void) __attribute__((noreturn)); // GCC builtin

void Frama_C_abort(void)
{
  __builtin_abort();
}


__POP_FC_STDLIB
