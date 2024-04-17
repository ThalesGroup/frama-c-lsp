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

#ifndef __FC_SPAWN
#define __FC_SPAWN
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_mode_t.h"
#include "__fc_define_pid_t.h"
#include "__fc_define_sigset_t.h"

__BEGIN_DECLS


typedef struct __fc_posix_spawnattr_t {
  int __fc_psa;
} posix_spawnattr_t;

typedef struct __fc_posix_posix_spawn_file_actions_t {
  int __fc_psfa;
} posix_spawn_file_actions_t;

#define POSIX_SPAWN_RESETIDS 0x01
#define POSIX_SPAWN_SETPGROUP 0x02
#define POSIX_SPAWN_SETSCHEDPARAM 0x10
#define POSIX_SPAWN_SETSCHEDULER 0x20
#define POSIX_SPAWN_SETSIGDEF 0x04
#define POSIX_SPAWN_SETSIGMASK 0x08

extern int posix_spawn(pid_t *restrict pid, const char *restrict path,
                       const posix_spawn_file_actions_t *file_actions,
                       const posix_spawnattr_t *restrict attrp,
                       char *const *restrict argv,
                       char *const *restrict envp);

extern int posix_spawn_file_actions_addclose(posix_spawn_file_actions_t
                                             *file_actions,
                                             int fildes);

extern int posix_spawn_file_actions_adddup2(posix_spawn_file_actions_t
                                            *file_actions,
                                            int fildes, int newfildes);

extern int posix_spawn_file_actions_addopen(posix_spawn_file_actions_t
                                            *restrict file_actions,
                                            int fildes,
                                            const char *restrict path,
                                            int oflag, mode_t mode);

extern int posix_spawn_file_actions_destroy(posix_spawn_file_actions_t
                                            *file_actions);

extern int posix_spawn_file_actions_init(posix_spawn_file_actions_t
                                         *file_actions);

extern int posix_spawnattr_destroy(posix_spawnattr_t *attr);

extern int posix_spawnattr_getflags(const posix_spawnattr_t *restrict attr,
                                    short *restrict flags);

extern int posix_spawnattr_getpgroup(const posix_spawnattr_t *restrict attr,
                                     pid_t *restrict pgroup);

extern int posix_spawnattr_getschedparam(const posix_spawnattr_t *restrict attr,
                                         struct sched_param *restrict
                                         schedparam);

extern int posix_spawnattr_getschedpolicy(const posix_spawnattr_t *restrict
                                          attr,
                                          int *restrict schedpolicy);

extern int posix_spawnattr_getsigdefault(const posix_spawnattr_t *restrict attr,
                                         sigset_t *restrict sigdefault);

extern int posix_spawnattr_getsigmask(const posix_spawnattr_t *restrict attr,
                                      sigset_t *restrict);

extern int posix_spawnattr_init(posix_spawnattr_t *attr);

extern int posix_spawnattr_setflags(posix_spawnattr_t *attr, short flags);

extern int posix_spawnattr_setpgroup(posix_spawnattr_t *attr, pid_t pgroup);

extern int posix_spawnattr_setschedparam(posix_spawnattr_t *restrict attr,
                                         const struct sched_param *restrict
                                         schedparam);

extern int posix_spawnattr_setschedpolicy(posix_spawnattr_t *attr,
                                          int schedpolicy);

extern int posix_spawnattr_setsigdefault(posix_spawnattr_t *restrict attr,
                                         const sigset_t *restrict sigdefault);

extern int posix_spawnattr_setsigmask(posix_spawnattr_t *restrict attr,
                                      const sigset_t *restrict sigmask);

extern int posix_spawnp(pid_t *restrict pid, const char *restrict file,
                        const posix_spawn_file_actions_t *file_actions,
                        const posix_spawnattr_t *restrict attrp,
                        char *const *restrict argv,
                        char *const *restrict envp);

__END_DECLS

__POP_FC_STDLIB
#endif
