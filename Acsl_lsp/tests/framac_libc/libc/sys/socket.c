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

#include "socket.h"
#include "../__fc_builtin.h"
__PUSH_FC_STDLIB

ssize_t sendto(int sockfd, const void *buf, size_t len, int flags,
               const struct sockaddr *address, socklen_t address_len) {
  // assuming preconditions already checked
  if (Frama_C_nondet(0, 1)) {
    // error
    int possible_errors[] = {
      EACCES,
      EAFNOSUPPORT,
      EAGAIN,
      EBADF,
      ECONNRESET,
      EDESTADDRREQ,
      EHOSTUNREACH,
      EINTR,
      EINVAL,
      EIO,
      EISCONN,
      ELOOP,
      EMSGSIZE,
      ENAMETOOLONG,
      ENETDOWN,
      ENETUNREACH,
      ENOBUFS,
      ENOENT,
      ENOMEM,
      ENOTCONN,
      ENOTDIR,
      ENOTSOCK,
      EOPNOTSUPP,
      EPIPE,
      EWOULDBLOCK,
    };
    errno = possible_errors[Frama_C_interval(0, sizeof(possible_errors)/sizeof(int)-1)];
    return -1;
  } else {
    // non-error
    return Frama_C_long_interval(0, len);
  }
}

ssize_t recvfrom(int sockfd, void *buf, size_t len, int flags,
                 struct sockaddr *addrbuf, socklen_t *addrbuf_len) {
  // assuming preconditions already checked
  if (Frama_C_nondet(0, 1)) {
    // error
    int possible_errors[] = {
      EAGAIN,
      EBADF,
      ECONNRESET,
      EINTR,
      EINVAL,
      EIO,
      ENOBUFS,
      ENOMEM,
      ENOTCONN,
      ENOTSOCK,
      EOPNOTSUPP,
      ETIMEDOUT,
      EWOULDBLOCK,
    };
    errno = possible_errors[Frama_C_interval(0, sizeof(possible_errors)/sizeof(int)-1)];
    return -1;
  } else {
    // non-error
    ((char*)buf)[Frama_C_interval(0, len-1)] = Frama_C_interval(0, 255);
    if (addrbuf) {
      Frama_C_make_unknown((char*)addrbuf, *addrbuf_len);
      /* From the "Linux Programmer's Manual:
         "Upon return, 'addrbuf_len' is updated to contain the actual size of
         the source address. The returned address is truncated if the buffer
         provided is too small; in this case, 'addrbuf_len' will return a value
         greater than was supplied to the call."
         Here we use _SS_MAXSIZE as the maximum address struct size. */
      *addrbuf_len = Frama_C_unsigned_int_interval(0, _SS_MAXSIZE);
    }
    return Frama_C_long_interval(0, len);
  }

}

__POP_FC_STDLIB
