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

#ifndef __FC_SYS_STATVFS_H__
#define __FC_SYS_STATVFS_H__

#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

#include "../__fc_define_fs_cnt.h"

struct statvfs {
  unsigned long f_bsize;   // File system block size.
  unsigned long f_frsize;  // Fundamental file system block size.
  fsblkcnt_t    f_blocks;  // Total number of blocks on file system in units of
                           // f_frsize.
  fsblkcnt_t    f_bfree;   // Total number of free blocks.
  fsblkcnt_t    f_bavail;  // Number of free blocks available to
                           // non-privileged process.
  fsfilcnt_t    f_files;   // Total number of file serial numbers.
  fsfilcnt_t    f_ffree;   // Total number of free file serial numbers.
  fsfilcnt_t    f_favail;  // Number of file serial numbers available to
                           // non-privileged process.
  unsigned long f_fsid;    // File system ID.
  unsigned long f_flag;    // Bit mask of f_flag values.
  unsigned long f_namemax; // Maximum filename length.
};

extern int fstatvfs(int fildes, struct statvfs *buf);

extern int statvfs(const char *restrict path, struct statvfs *restrict buf);

__END_DECLS
__POP_FC_STDLIB
#endif
