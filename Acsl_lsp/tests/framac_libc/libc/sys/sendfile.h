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

// Non-POSIX; Linux-specific

#ifndef __FC_SYS_SENDFILE_H__
#define __FC_SYS_SENDFILE_H__

#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

#include "../__fc_define_max_open_files.h"
#include "../__fc_define_off_t.h"
#include "../__fc_define_size_t.h"
#include "../__fc_define_ssize_t.h"
#include "../errno.h"

/*@
  //missing: requires in_fd opened for reading
  //missing: requires out_fd opened for writing
  requires valid_in_fd: 0 <= in_fd < __FC_MAX_OPEN_FILES;
  requires valid_out_fd: 0 <= out_fd < __FC_MAX_OPEN_FILES;
  requires valid_offset_or_null: offset == \null || \valid(offset);
  requires initialization:offset: offset == \null || \initialized(offset);
  assigns errno, \result, *offset \from indirect:out_fd, indirect:in_fd,
                                        indirect:offset, indirect:count;
  //missing: assigns "out_fd's state (offset/buffer if file; or socket's state)"
  //missing: assigns "in_fd's offset, if offset == null"
  ensures error_or_chars_sent: \result == -1 || 0 <= \result <= count;
  ensures initialization:offset: \initialized(offset);
 */
ssize_t sendfile(int out_fd, int in_fd, off_t *offset, size_t count);

__END_DECLS
__POP_FC_STDLIB
#endif
