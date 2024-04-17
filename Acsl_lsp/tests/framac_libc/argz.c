/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 1995-2023                                               */
/*    Free Software Foundation, Inc.                                      */
/*  Copyright (C) 2021-2023                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  This file is derived from the GNU C Library. You can redistribute it  */
/*  and/or modify it under the terms of the GNU Lesser General Public     */
/*  License as published by the Free Software Foundation, version 2.1.    */
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

// Non-POSIX; this file is defined in the GNU libc.

#include "argz.h"

__PUSH_FC_STDLIB

void argz_stringify (char *argz, size_t len, int sep) {
  if (len > 0)
    while (1) {
      size_t part_len = strnlen (argz, len);
      argz += part_len;
      len -= part_len;
      if (len-- <= 1) break;
      *argz++ = sep;
    }
}

static void str_append (char **to, size_t *to_len, const char *buf,
                        const size_t buf_len) {
  size_t new_len = *to_len + buf_len;
  char *new_to = realloc (*to, new_len + 1);

  if (new_to) {
    *((char *) mempcpy (new_to + *to_len, buf, buf_len)) = '\0';
    *to = new_to;
    *to_len = new_len;
  } else {
    free (*to);
    *to = 0;
  }
}

error_t argz_replace (char **argz, size_t *argz_len, const char *str,
                      const char *with, unsigned *replace_count) {
  error_t er = 0;

  if (str && *str) {
    char *arg = 0;
    char *src = *argz;
    size_t src_len = *argz_len;
    char *dst = 0;
    size_t dst_len = 0;
    int delayed_copy = 1;
    size_t str_len = strlen (str), with_len = strlen (with);

    while (!er && (arg = argz_next (src, src_len, arg))) {
      char *match = strstr (arg, str);
      if (match) {
        char *from = match + str_len;
        size_t to_len = match - arg;
        char *to = strndup (arg, to_len);

        while (to && from) {
          str_append (&to, &to_len, with, with_len);
          if (to) {
            match = strstr (from, str);
            if (match) {
              str_append (&to, &to_len, from, match - from);
              from = match + str_len;
            } else {
              str_append (&to, &to_len, from, strlen (from));
              from = 0;
            }
          }
        }

        if (to) {
          if (delayed_copy) {
            if (arg > src)
              er = argz_append (&dst, &dst_len, src, (arg - src));
            delayed_copy = 0;
          }
          if (! er)
            er = argz_add (&dst, &dst_len, to);
          free (to);
        } else
          er = ENOMEM;

        if (replace_count)
          (*replace_count)++;
      } else if (! delayed_copy)
        er = argz_add (&dst, &dst_len, arg);
    }

    if (! er) {
      if (! delayed_copy) {
        free (src);
        *argz = dst;
        *argz_len = dst_len;
      }
    } else if (dst_len > 0)
      free (dst);
  }

  return er;
}

char * argz_next (const char *argz, size_t argz_len, const char *entry) {
  if (entry) {
    if (entry < argz + argz_len)
      entry = strchr (entry, '\0') + 1;

    return entry >= argz + argz_len ? NULL : (char *) entry;
  } else
    if (argz_len > 0)
      return (char *) argz;
    else
      return NULL;
}

error_t argz_insert (char **argz, size_t *argz_len, char *before,
                     const char *entry) {
  if (! before)
    return argz_add (argz, argz_len, entry);

  if (before < *argz || before >= *argz + *argz_len)
    return EINVAL;

  if (before > *argz)
    while (before[-1])
      before--;

  {
    size_t after_before = *argz_len - (before - *argz);
    size_t entry_len = strlen  (entry) + 1;
    size_t new_argz_len = *argz_len + entry_len;
    // before_offset must be computed before realloc, in case it will be used
    ptrdiff_t before_offset = before - *argz;
    char *new_argz = realloc (*argz, new_argz_len);

    if (new_argz) {
      before = new_argz + before_offset;
      memmove (before + entry_len, before, after_before);
      memmove (before, entry, entry_len);
      *argz = new_argz;
      *argz_len = new_argz_len;
      return 0;
    } else
      return ENOMEM;
  }
}

void argz_extract (const char *argz, size_t len, char **argv) {
  while (len > 0) {
    size_t part_len = strlen (argz);
    *argv++ = (char *) argz;
    argz += part_len + 1;
    len -= part_len + 1;
  }
  *argv = 0;
}

void argz_delete (char **argz, size_t *argz_len, char *entry)
{
  if (entry) {
    size_t entry_len = strlen (entry) + 1;
    *argz_len -= entry_len;
    memmove (entry, entry + entry_len, *argz_len - (entry - *argz));
    if (*argz_len == 0) {
      free (*argz);
      *argz = 0;
    }
  }
}

error_t argz_create_sep (const char *string, int delim, char **argz,
                         size_t *len) {
  size_t nlen = strlen (string) + 1;

  if (nlen > 1) {
    const char *rp;
    char *wp;

    *argz = (char *) malloc (nlen);
    if (*argz == NULL)
      return ENOMEM;

    rp = string;
    wp = *argz;
    do
      if (*rp == delim) {
        if (wp > *argz && wp[-1] != '\0')
          *wp++ = '\0';
        else
          --nlen;
      } else
        *wp++ = *rp;
    while (*rp++ != '\0');

    if (nlen == 0) {
      free (*argz);
      *argz = NULL;
      *len = 0;
    }

    *len = nlen;
  } else {
    *argz = NULL;
    *len = 0;
  }

  return 0;
}

error_t argz_create (char *const argv[], char **argz, size_t *len) {
  int argc;
  size_t tlen = 0;
  char *const *ap;
  char *p;

  for (argc = 0; argv[argc] != NULL; ++argc)
    tlen += strlen (argv[argc]) + 1;

  if (tlen == 0)
    *argz = NULL;
  else {
    *argz = malloc (tlen);
    if (*argz == NULL)
      return ENOMEM;

    for (p = *argz, ap = argv; *ap; ++ap, ++p)
      p = stpcpy (p, *ap);
  }
  *len = tlen;

  return 0;
}

size_t argz_count (const char *argz, size_t len) {
  size_t count = 0;
  while (len > 0) {
    size_t part_len = strlen(argz);
    argz += part_len + 1;
    len -= part_len + 1;
    count++;
  }
  return count;
}

error_t argz_append (char **argz, size_t *argz_len, const char *buf,
                     size_t buf_len) {
  size_t new_argz_len = *argz_len + buf_len;
  char *new_argz = realloc (*argz, new_argz_len);
  if (new_argz) {
    memcpy (new_argz + *argz_len, buf, buf_len);
    *argz = new_argz;
    *argz_len = new_argz_len;
    return 0;
  } else
    return ENOMEM;
}

error_t argz_add (char **argz, size_t *argz_len, const char *str) {
  return argz_append (argz, argz_len, str, strlen (str) + 1);
}

error_t argz_add_sep (char **argz, size_t *argz_len, const char *string,
                      int delim) {
  size_t nlen = strlen (string) + 1;

  if (nlen > 1) {
    const char *rp;
    char *wp;

    *argz = (char *) realloc (*argz, *argz_len + nlen);
    if (*argz == NULL)
      return ENOMEM;

    wp = *argz + *argz_len;
    rp = string;
    do
      if (*rp == delim) {
        if (wp > *argz && wp[-1] != '\0')
          *wp++ = '\0';
        else
          --nlen;
      }
      else
        *wp++ = *rp;
    while (*rp++ != '\0');

    *argz_len += nlen;
  }

  return 0;
}

__POP_FC_STDLIB
