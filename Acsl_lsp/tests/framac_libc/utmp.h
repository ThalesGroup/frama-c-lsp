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

// Note: this file is non-POSIX, but used by some coreutils.

#ifndef __FC_UTMP
#define __FC_UTMP
#include "features.h"
__PUSH_FC_STDLIB

#include "__fc_define_pid_t.h"
#include "sys/time.h"
#include "stdint.h"

__BEGIN_DECLS

#define _PATH_UTMP "/var/run/utmp"
#define UTMP_FILE     _PATH_UTMP
#define UTMP_FILENAME _PATH_UTMP
#define _PATH_WTMP "/var/log/wtmp"
#define WTMP_FILE     _PATH_WTMP
#define WTMP_FILENAME _PATH_WTMP

#define UT_LINESIZE  32
#define UT_NAMESIZE  32
#define UT_HOSTSIZE  256

struct lastlog
{
  time_t ll_time;
  char ll_line[UT_LINESIZE];
  char ll_host[UT_HOSTSIZE];
};

struct exit_status
{
  short int e_termination;
  short int e_exit;
};

struct utmp
{
  short int ut_type;
  pid_t ut_pid;
  char ut_line[UT_LINESIZE];
  char ut_id[4];
  char ut_user[UT_NAMESIZE];
  char ut_host[UT_HOSTSIZE];
  struct exit_status ut_exit;
  long int ut_session;
  struct timeval ut_tv;
  int32_t ut_addr_v6[4];
  char __glibc_reserved[20]; // used by who.c
};

#define ut_name ut_user
#define ut_time ut_tv.tv_sec

extern int login_tty (int fd);
extern void login (const struct utmp *entry);
extern int logout (const char *ut_line);
extern void logwtmp (const char *ut_line, const char *ut_name,
                     const char *ut_host);

extern void updwtmp (const char *wtmp_file, const struct utmp *utmp);

extern int utmpname (const char *file);

extern struct utmp *getutent (void);

extern void setutent (void);

extern void endutent (void);

extern struct utmp *getutid (const struct utmp *id);

extern struct utmp *getutline (const struct utmp *line);

extern struct utmp *pututline (const struct utmp *utmp_ptr);

extern int getutent_r (struct utmp *buffer, struct utmp **result);

extern int getutid_r (const struct utmp *id, struct utmp *buffer,
                      struct utmp **result);

extern int getutline_r (const struct utmp *line,
                        struct utmp *buffer, struct utmp **result);


__END_DECLS
__POP_FC_STDLIB
#endif
