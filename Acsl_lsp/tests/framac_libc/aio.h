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

#ifndef __FC_AIO
#define __FC_AIO
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_off_t.h"
#include "__fc_define_size_t.h"
#include "__fc_define_ssize_t.h"
#include "__fc_define_timespec.h"
#include "signal.h"

__BEGIN_DECLS

struct aiocb {
  int aio_fildes;
  off_t aio_offset;
  volatile void *aio_buf;
  size_t aio_nbytes;
  int aio_reqprio;
  struct sigevent aio_sigevent;
  int aio_lio_opcode;
};

#define AIO_ALLDONE 2
#define AIO_CANCELED 0
#define AIO_NOTCANCELED 1

#define LIO_NOP 2
#define LIO_NOWAIT 1

#define LIO_READ 0
#define LIO_WAIT 0
#define LIO_WRITE 1

extern int aio_cancel(int fildes, struct aiocb *aiocbp);

extern int aio_error(const struct aiocb *aiocbp);

extern int aio_fsync(int op, struct aiocb *aiocbp);

extern int aio_read(struct aiocb *aiocbp);

extern ssize_t aio_return(struct aiocb *aiocbp);

extern int aio_suspend(const struct aiocb *const list[], int nent,
                       const struct timespec *timeout);

extern int aio_write(struct aiocb *aiocbp);

extern int lio_listio(int mode, struct aiocb *restrict const list[restrict],
                      int nent, struct sigevent *restrict sig);

__END_DECLS

__POP_FC_STDLIB
#endif
