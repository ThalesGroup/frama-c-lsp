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

#ifndef __FC_SYS_STAT_H
#define __FC_SYS_STAT_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

#include "../__fc_define_stat.h"
#include "../__fc_string_axiomatic.h"
#include "../errno.h"

/*@
  // missing: assigns 'filesystem' \frompath, path[0..], mode;
  requires valid_path: valid_read_string(path);
  assigns \result, __fc_errno
          \from indirect:path, indirect:path[0 .. strlen(path)],
                indirect:mode; //missing: \from 'filesystem'
  ensures errno_set: __fc_errno == \old(__fc_errno) ||
                     __fc_errno \in {EACCES, EINTR, EINVAL, ELOOP,
                                     ENAMETOOLONG, ENOENT, ENOTDIR, EPERM,
                                     EROFS};
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int chmod(const char *path, mode_t mode);

/*@
  // missing: assigns 'filesystem' \from fd, path, path[0..], mode, flag;
  requires valid_path: valid_read_string(path);
  assigns \result, __fc_errno
          \from indirect:fd, indirect:path, indirect:path[0 .. strlen(path)],
                indirect:mode, indirect:flag; //missing: \from 'filesystem'
  ensures errno_set: __fc_errno == \old(__fc_errno) ||
                     __fc_errno \in {EACCES, EBADF, EINTR, EINVAL, ELOOP,
                                     ENAMETOOLONG, ENOENT, ENOTDIR, EOPNOTSUPP,
                                     EPERM, EROFS};
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int fchmodat(int fd, const char *path, mode_t mode, int flag);

/*@
  // missing: assigns 'filesystem' \from fildes, mode;
  assigns \result, __fc_errno
          \from indirect:fildes, indirect:mode; //missing: \from 'filesystem'
  ensures errno_set: __fc_errno == \old(__fc_errno) ||
                     __fc_errno \in {EBADF, EINTR, EINVAL, EPERM, EROFS};
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int fchmod(int fildes, mode_t mode);

/*@
  //TODO: define proper initialization postcondition; it involves only a few
  //      specific fields, and only a few bits within those fields.
  requires valid_buf: \valid(buf);
  assigns \result, *buf, __fc_errno
          \from indirect:fildes; //missing: \from 'filesystem'
  ensures errno_set: __fc_errno == \old(__fc_errno) ||
                     __fc_errno \in {EBADF, EIO, EOVERFLOW};
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int fstat(int fildes, struct stat *buf);

/*@
  // missing: assigns 'filesystem' \from fd, path, path[0..], flag;
  requires valid_path: valid_read_string(path);
  requires valid_buf: \valid(buf);
  assigns \result, *buf, __fc_errno
          \from indirect:fd, indirect:path, indirect:path[0 .. strlen(path)],
                indirect:flag; //missing: \from 'filesystem'
  ensures errno_set: __fc_errno == \old(__fc_errno) ||
                     __fc_errno \in {EACCES, EBADF, EINVAL, EIO, ELOOP,
                                     ENAMETOOLONG, ENOENT, ENOMEM, ENOTDIR,
                                     EOVERFLOW};
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int fstatat(int fd, const char *restrict path,
                   struct stat *restrict buf, int flag);

/*@ // missing: may assign to errno: EACCES, ELOOP, ENAMETOOLONG,
    //                               ENOENT, ENOMEM, ENOTDIR, EOVERFLOW,
    //                               EINVAL
    // missing: assigns \result, *buf \from 'filesystem'
  requires valid_path: valid_read_string(path);
  requires valid_buf: \valid(buf);
  assigns \result, *buf \from indirect:path, indirect:path[0..strlen(path)];
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int    lstat(const char *path, struct stat *buf);

/*@ // missing: may assign to errno: EACCES, EEXIST, ELOOP, EMLINK,
    //                               ENAMETOOLONG, ENOENT, ENOSPC,
    //                               ENOTDIR, EROFS
    // missing: assigns \result \from 'filesystem'
  requires valid_path: valid_read_string(path);
  assigns \result \from indirect:path, indirect:path[0..strlen(path)],
                        indirect:mode;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int    mkdir(const char *path, mode_t mode);

/*@ // missing: may assign to errno: EACCES, EEXIST, ELOOP, ENAMETOOLONG,
    //                               ENOENT, ENOTDIR, ENOSPC, EROFS
    // missing: assigns \result \from 'filesystem'
    // missing: assigns 'filesystem' \from indirect:path,
    //                                     indirect:path[0..strlen(path)], mode;
  requires valid_path: valid_read_string(path);
  assigns \result \from indirect:path, indirect:path[0..strlen(path)],
                        indirect:mode;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int    mkfifo(const char *path, mode_t mode);

/*@ // missing: may assign to errno: EACCES, EEXIST, EINVAL, EIO, ELOOP,
    //                               ENAMETOOLONG, ENOENT, ENOTDIR, ENOSPC,
    //                               EPERM, EROFS
    // missing: assigns \result \from 'filesystem'
    // missing: assigns 'filesystem' \from indirect:path,
    //          indirect:path[0..strlen(path)], mode, dev;
  requires valid_path: valid_read_string(path);
  assigns \result \from indirect:path, indirect:path[0..strlen(path)],
                        indirect:mode, indirect:dev;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int    mknod(const char *path, mode_t mode, dev_t dev);

/*@ //missing: assigns \from 'filesystem'
  requires valid_pathname: valid_read_string(pathname);
  requires valid_buf: \valid(buf);
  assigns \result, *buf \from pathname[0..strlen(pathname)];
  ensures result_ok_or_error: \result == 0 || \result == -1;
  ensures init_on_success:initialization:buf:
    \result == 0 ==> \initialized(buf);
*/
extern int    stat(const char *pathname, struct stat *buf);

/*@ //missing: assigns 'process umask' \from cmask;
    //missing: assigns \result \from 'old process umask'
  assigns \result \from indirect:cmask;
*/
extern mode_t umask(mode_t cmask);

#define S_TYPEISMQ(buf) ((buf)->st_mode - (buf)->st_mode)
#define S_TYPEISSEM(buf) ((buf)->st_mode - (buf)->st_mode)
#define S_TYPEISSHM(buf) ((buf)->st_mode - (buf)->st_mode)
#define S_TYPEISTMO(buf) (0)

// Non-POSIX; Linux-specific
#define S_IRWXUGO (S_IRWXU|S_IRWXG|S_IRWXO)
#define S_IALLUGO (S_ISUID|S_ISGID|S_ISVTX|S_IRWXUGO)
#define S_IRUGO (S_IRUSR|S_IRGRP|S_IROTH)
#define S_IWUGO (S_IWUSR|S_IWGRP|S_IWOTH)
#define S_IXUGO (S_IXUSR|S_IXGRP|S_IXOTH)

// Non-POSIX
#define S_IFDOOR 0
#define S_ISDOOR(m) 0

// Non-POSIX
#define UTIME_NOW ((1L << 30) - 1L)
#define UTIME_OMIT ((1L << 30) - 2L)

__END_DECLS
__POP_FC_STDLIB
#endif
