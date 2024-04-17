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

#ifndef FC_UIO
#define FC_UIO

#include "../features.h"
__PUSH_FC_STDLIB
#include "../__fc_define_ssize_t.h"
#include "../__fc_define_size_t.h"
#include "../__fc_define_iovec.h"
#include "../__fc_define_fds.h"
#include "../limits.h"

__BEGIN_DECLS



/*@
  requires valid_fd: 0 <= fd < __FC_MAX_OPEN_FILES;
  requires initialization:initialized_inputs:
    \forall integer i; 0 <= i < iovcnt ==>
      \initialized(&iov[i].iov_base) && \initialized(&iov[i].iov_len);
  requires valid_iov: \forall integer i; 0 <= i < iovcnt ==>
    \valid( ((char*)&iov[i].iov_base)+(0 .. iov[i].iov_len-1));
  requires bounded_iovcnt: 0 <= iovcnt <= IOV_MAX;
  assigns { ((char*)iov[i].iov_base)[j] |
            integer i, j; 0 <= i < iovcnt && 0 <= j < iov[i].iov_len }
    \from indirect:fd, indirect:iovcnt, indirect:__fc_fds[fd];
  assigns \result \from indirect:fd, indirect:__fc_fds[fd], indirect:iov[0 ..],
                        indirect:iovcnt;
  ensures result_error_or_read_bytes: \result == -1 ||
            0 <= \result <= \sum(0,iovcnt-1, \lambda integer k; iov[k].iov_len);
 */
extern ssize_t readv(int fd, const struct iovec *iov, int iovcnt);

/*@
  requires valid_fd: 0 <= fd < __FC_MAX_OPEN_FILES;
  requires initialization:initialized_inputs:
    \forall integer i; 0 <= i < iovcnt ==>
      \initialized(&iov[i].iov_base) && \initialized(&iov[i].iov_len);
  requires valid_read_iov: \valid_read(&iov[0 .. iovcnt-1]);
  requires valid_read_iov_bases:
    \forall integer i; 0 <= i < iovcnt ==>
      \valid_read(((char*)iov[i].iov_base) + (0 .. iov[i].iov_len-1));
  requires bounded_iovcnt: 0 <= iovcnt <= IOV_MAX;
  requires bounded_lengths:
     \sum(0, iovcnt-1, \lambda integer k; iov[k].iov_len) <= SSIZE_MAX;
  assigns \result \from indirect:fd, indirect:__fc_fds[fd], indirect:iov[0 ..],
                        indirect:iovcnt;
  assigns __fc_fds[fd] \from indirect:iov[0 ..], indirect:iovcnt;
  ensures result_error_or_written_bytes: \result == -1 ||
          0 <= \result <= \sum(0, iovcnt-1, \lambda integer k; iov[k].iov_len);
 */
extern ssize_t writev(int fd, const struct iovec *iov, int iovcnt);

__END_DECLS
__POP_FC_STDLIB
#endif
