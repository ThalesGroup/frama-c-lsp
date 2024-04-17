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

#ifndef __FC_MQUEUE
#define __FC_MQUEUE
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_size_t.h"
#include "__fc_define_ssize_t.h"
#include "signal.h"

__BEGIN_DECLS

typedef int mqd_t;

struct mq_attr {
  long mq_flags;
  long mq_maxmsg;
  long mq_msgsize;
  long mq_curmsgs;
};

extern int mq_close(mqd_t mqdes);

extern int mq_getattr(mqd_t mqdes, struct mq_attr *mqstat);

extern int mq_notify(mqd_t mqdes, const struct sigevent *notification);

extern mqd_t mq_open(const char *name, int oflag, ...);

extern ssize_t mq_receive(mqd_t mqdes, char *msg_ptr, size_t msg_len,
                          unsigned *msg_prio);

extern int mq_send(mqd_t mqdes, const char *msg_ptr, size_t msg_len,
                   unsigned msg_prio);

extern int mq_setattr(mqd_t mqdes, const struct mq_attr *restrict mqstat,
                      struct mq_attr *restrict omqstat);

extern ssize_t mq_timedreceive(mqd_t mqdes, char *restrict msg_ptr,
                               size_t msg_len, unsigned *restrict msg_prio,
                               const struct timespec *restrict abstime);

extern int mq_timedsend(mqd_t mqdes, const char *msg_ptr, size_t msg_len,
                        unsigned msg_prio, const struct timespec *abstime);

extern int mq_unlink(const char *name);

__END_DECLS

__POP_FC_STDLIB
#endif
