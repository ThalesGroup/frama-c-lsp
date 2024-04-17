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

#ifndef __FC_PWD_H__
#define __FC_PWD_H__
#include "features.h"
__PUSH_FC_STDLIB

#include "__fc_builtin.h"
#include "__fc_define_uid_and_gid.h"
#include "__fc_string_axiomatic.h"

#include "errno.h"
// for size_t
#include "stddef.h"
__BEGIN_DECLS

struct passwd {
  char    *pw_name;
  char    *pw_passwd; // not POSIX, but allowed by it, and present in glibc
  uid_t    pw_uid;
  gid_t    pw_gid;
  char    *pw_gecos; // not POSIX, but present in most implementations
  char    *pw_dir;
  char    *pw_shell;
};

__FC_EXTERN char __fc_getpw_pw_name[64];
__FC_EXTERN char __fc_getpw_pw_passwd[64];
__FC_EXTERN char __fc_getpw_pw_gecos[64];
__FC_EXTERN char __fc_getpw_pw_dir[64];
__FC_EXTERN char __fc_getpw_pw_shell[64];

struct passwd __fc_pwd =
  {.pw_name = __fc_getpw_pw_name,
   .pw_passwd = __fc_getpw_pw_passwd,
   .pw_gecos = __fc_getpw_pw_gecos,
   .pw_dir = __fc_getpw_pw_dir,
   .pw_shell = __fc_getpw_pw_shell};

struct passwd *__fc_p_pwd = & __fc_pwd;

/*@
  // missing: may assign to errno: EIO, EINTR, EMFILE, ENFILE
  // missing: assigns \result, __fc_pwd[0..] \from 'password database'
  requires valid_name: valid_read_string(name);
  assigns \result \from __fc_p_pwd, indirect:name[0..];
  assigns __fc_pwd \from indirect:name[0..];
  ensures result_null_or_internal_struct:
    \result == \null || \result == __fc_p_pwd;
*/
extern struct passwd *getpwnam(const char *name);

/*@ // missing: assigns \result, __fc_pwd[0..] \from 'password database'
  assigns \result \from __fc_p_pwd, indirect:uid;
  assigns __fc_pwd \from indirect:uid;
  ensures result_null_or_internal_struct:
    \result == \null || \result == __fc_p_pwd;
*/
extern struct passwd *getpwuid(uid_t uid);

/*@
  // missing: assigns \result, *result, *pwd, __fc_pwd[0..] \from 'password database'
  requires valid_name: valid_read_string(name);
  requires valid_buf: \valid(buf+(0 .. buflen-1));
  requires valid_pwd: \valid(pwd);
  requires valid_result: \valid(result);
  assigns \result \from indirect:__fc_p_pwd, indirect:name[0..];
  assigns __fc_pwd \from indirect:name[0..];
  assigns *pwd, *result \from __fc_p_pwd;
  assigns buf[0 .. buflen-1] \from indirect:*__fc_p_pwd;
  ensures result_null_or_assigned:
    *result == \null || *result == pwd;
  ensures result_ok_or_error:
    \result == 0 || \result \in {EIO, EMFILE, ENFILE, ERANGE};
*/
extern int getpwnam_r(const char *name, struct passwd *pwd,
                      char *buf, size_t buflen, struct passwd **result);

/*@
  // missing: assigns \result, *result, *pwd, __fc_pwd[0..] \from 'password database'
  requires valid_buf: \valid(buf+(0 .. buflen-1));
  requires valid_pwd: \valid(pwd);
  requires valid_result: \valid(result);
  assigns \result \from indirect:__fc_p_pwd;
  assigns __fc_pwd \from indirect:uid;
  assigns *pwd, *result \from __fc_p_pwd;
  assigns buf[0 .. buflen-1] \from indirect:*__fc_p_pwd;
  ensures result_null_or_assigned:
    *result == \null || *result == pwd;
  ensures result_ok_or_error:
    \result == 0 || \result \in {EIO, EMFILE, ENFILE, ERANGE};
*/
extern int getpwuid_r(uid_t uid, struct passwd *pwd,
                      char *buf, size_t buflen, struct passwd **result);

extern void           endpwent(void);
extern struct passwd *getpwent(void);
extern void           setpwent(void);

__END_DECLS

__POP_FC_STDLIB
#endif
