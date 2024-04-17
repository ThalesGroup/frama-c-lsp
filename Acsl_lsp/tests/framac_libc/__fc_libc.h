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

// This file includes all compatible libc/POSIX/BSD headers known by the
// Frama-C standard library. It is used by some Frama-C scripts.

#define _XOPEN_SOURCE 600
#define _POSIX_C_SOURCE 200112L
#define _GNU_SOURCE 1

#include "aio.h"
#include "alloca.h"
#include "argz.h"
#include "arpa/inet.h"
#include "assert.h"
#include "byteswap.h"
//#include "complex.h"
#include "cpio.h"
#include "ctype.h"
#include "dirent.h"
#include "dlfcn.h"
#include "endian.h"
#include "err.h"
#include "errno.h"
#include "fcntl.h"
#include "features.h"
#include "fenv.h"
#include "float.h"
#include "fmtmsg.h"
#include "fnmatch.h"
#include "ftw.h"
#include "getopt.h"
#include "glob.h"
#include "grp.h"
#include "iconv.h"
#include "ifaddrs.h"
#include "inttypes.h"
#include "iso646.h"
#include "langinfo.h"
#include "libgen.h"
#include "limits.h"
#include "locale.h"
#include "malloc.h"
#include "math.h"
#include "memory.h"
#include "monetary.h"
#include "mqueue.h"
#include "ndbm.h"
#include "netdb.h"
#include "net/if.h"
#include "netinet/in.h"
#include "netinet/ip.h"
#include "netinet/tcp.h"
#include "nl_types.h"
#include "poll.h"
#include "pthread.h"
#include "pwd.h"
#include "regex.h"
#include "resolv.h"
#include "sched.h"
#include "search.h"
#include "semaphore.h"
#include "setjmp.h"
#include "signal.h"
#include "spawn.h"
#include "stdalign.h"
#include "stdarg.h"
#include "stdatomic.h"
#include "stdbool.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "stdnoreturn.h"
#include "string.h"
#include "strings.h"
#include "stropts.h"
#include "sys/file.h"
#include "sys/ioctl.h"
#include "sys/ipc.h"
#include "syslog.h"
#include "sys/mman.h"
#include "sys/msg.h"
#include "sys/param.h"
#include "sys/random.h"
#include "sys/resource.h"
#include "sys/select.h"
#include "sys/sem.h"
#include "sys/sendfile.h"
#include "sys/shm.h"
#include "sys/signal.h"
#include "sys/socket.h"
#include "sys/stat.h"
#include "sys/statvfs.h"
#include "sys/time.h"
#include "sys/times.h"
#include "sys/timex.h"
#include "sys/types.h"
#include "sys/uio.h"
#include "sys/un.h"
#include "sys/utsname.h"
#include "sys/vfs.h"
#include "sys/wait.h"
#include "tar.h"
#include "termios.h"
//#include "tgmath.h"
#include "time.h"
#include "trace.h"
#include "ulimit.h"
#include "unistd.h"
#include "utime.h"
#include "utmp.h"
#include "utmpx.h"
#include "wait.h"
#include "wchar.h"
#include "wctype.h"
#include "wordexp.h"
