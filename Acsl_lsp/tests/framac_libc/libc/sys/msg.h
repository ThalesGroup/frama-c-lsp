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

#ifndef __FC_SYS_SHM
#define __FC_SYS_SHM
#include "../features.h"
__PUSH_FC_STDLIB
#include "__fc_define_pid_t.h"
#include "__fc_define_size_t.h"
#include "__fc_define_ssize_t.h"
#include "__fc_define_time_t.h"
#include "ipc.h"

__BEGIN_DECLS

typedef unsigned long msgqnum_t;
typedef unsigned long msglen_t;

#define MSG_NOERROR 010000

struct msqid_ds {
 struct ipc_perm msg_perm;
 msgqnum_t msg_qnum;
 msglen_t msg_qbytes;
 pid_t msg_lspid;
 pid_t msg_lrpid;
 time_t msg_stime;
 time_t msg_rtime;
 time_t msg_ctime;
};

extern int msgctl(int msqid, int cmd, struct msqid_ds *buf);

extern int msgget(key_t key, int msgflg);

extern ssize_t msgrcv(int msgid, void *msgp, size_t msgsz, long msgtyp,
                      int msgflg);

extern int msgsnd(int msqid, const void *msgp, size_t msgsz, int msgflg);

__END_DECLS

__POP_FC_STDLIB
#endif
