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

#ifndef __FC_TRACE
#define __FC_TRACE
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_pid_t.h"
#include "__fc_define_pthread_types.h"
#include "__fc_define_size_t.h"
#include "__fc_define_timespec.h"

__BEGIN_DECLS

typedef unsigned int trace_event_id_t;

typedef unsigned int trace_id_t;

typedef struct __fc_trace_attr_t {
   int _fc ;
} trace_attr_t;

typedef struct __fc_trace_event_set_t {
   int _fc ;
} trace_event_set_t;

struct posix_trace_event_info {
  trace_event_id_t posix_event_id;
  pid_t posix_pid;
  void *posix_prog_address;
  pthread_t posix_thread_id;
  struct timespec posix_timestamp;
  int posix_truncation_status;
};

struct posix_trace_status_info {
  int posix_stream_full_status;
  int posix_stream_overrun_status;
  int posix_stream_status;
  int posix_log_full_status;
  int posix_log_overrun_status;
  int posix_stream_flush_error;
  int posix_stream_flush_status;
};

// Note: the constants below are not defined in most Linux systems,
// so arbitrary values were chosen.

#define POSIX_TRACE_ALL_EVENTS 1

#define POSIX_TRACE_APPEND 2

#define POSIX_TRACE_CLOSE_FOR_CHILD 3

#define POSIX_TRACE_FILTER 4

#define POSIX_TRACE_FLUSH 5
#define POSIX_TRACE_FLUSH_START 6
#define POSIX_TRACE_FLUSH_STOP 7
#define POSIX_TRACE_FLUSHING 8

#define POSIX_TRACE_FULL 9
#define POSIX_TRACE_LOOP 10
#define POSIX_TRACE_NO_OVERRUN 11

#define POSIX_TRACE_NOT_FLUSHING 12

#define POSIX_TRACE_NOT_FULL 13

#define POSIX_TRACE_INHERITED 14

#define POSIX_TRACE_NOT_TRUNCATED 15
#define POSIX_TRACE_OVERFLOW 16
#define POSIX_TRACE_OVERRUN 17
#define POSIX_TRACE_RESUME 18
#define POSIX_TRACE_RUNNING 19
#define POSIX_TRACE_START 20
#define POSIX_TRACE_STOP 21
#define POSIX_TRACE_SUSPENDED 22
#define POSIX_TRACE_SYSTEM_EVENTS 23
#define POSIX_TRACE_TRUNCATED_READ 24
#define POSIX_TRACE_TRUNCATED_RECORD 25
#define POSIX_TRACE_UNNAMED_USER_EVENT 26
#define POSIX_TRACE_UNTIL_FULL 27
#define POSIX_TRACE_WOPID_EVENTS 28

extern int posix_trace_attr_destroy(trace_attr_t *);

extern int posix_trace_attr_getclockres(const trace_attr_t *,
                                        struct timespec *);

extern int posix_trace_attr_getcreatetime(const trace_attr_t *,
                                          struct timespec *);

extern int posix_trace_attr_getgenversion(const trace_attr_t *, char *);

extern int posix_trace_attr_getinherited(const trace_attr_t *restrict,
                                         int *restrict);

extern int posix_trace_attr_getlogfullpolicy(const trace_attr_t *restrict,
                                             int *restrict);

extern int posix_trace_attr_getlogsize(const trace_attr_t *restrict,
                                       size_t *restrict);

extern int posix_trace_attr_getmaxdatasize(const trace_attr_t *restrict,
                                           size_t *restrict);

extern int posix_trace_attr_getmaxsystemeventsize(const trace_attr_t *restrict,
                                                  size_t *restrict);

extern int posix_trace_attr_getmaxusereventsize(const trace_attr_t *restrict,
                                                size_t, size_t *restrict);

extern int posix_trace_attr_getname(const trace_attr_t *, char *);

extern int posix_trace_attr_getstreamfullpolicy(const trace_attr_t *restrict,
                                                int *restrict);

extern int posix_trace_attr_getstreamsize(const trace_attr_t *restrict,
                                          size_t *restrict);

extern int posix_trace_attr_init(trace_attr_t *);

extern int posix_trace_attr_setinherited(trace_attr_t *, int);

extern int posix_trace_attr_setlogfullpolicy(trace_attr_t *, int);

extern int posix_trace_attr_setlogsize(trace_attr_t *, size_t);

extern int posix_trace_attr_setmaxdatasize(trace_attr_t *, size_t);

extern int posix_trace_attr_setname(trace_attr_t *, const char *);

extern int posix_trace_attr_setstreamfullpolicy(trace_attr_t *, int);

extern int posix_trace_attr_setstreamsize(trace_attr_t *, size_t);

extern int posix_trace_clear(trace_id_t);

extern int posix_trace_close(trace_id_t);

extern int posix_trace_create(pid_t, const trace_attr_t *restrict,
                              trace_id_t *restrict);

extern int posix_trace_create_withlog(pid_t, const trace_attr_t *restrict,
                                      int, trace_id_t *restrict);

extern void posix_trace_event(trace_event_id_t, const void *restrict, size_t);

extern int posix_trace_eventid_equal(trace_id_t, trace_event_id_t,
                                     trace_event_id_t);

extern int posix_trace_eventid_get_name(trace_id_t, trace_event_id_t, char *);

extern int posix_trace_eventid_open(const char *restrict,
                                    trace_event_id_t *restrict);

extern int posix_trace_eventset_add(trace_event_id_t, trace_event_set_t *);

extern int posix_trace_eventset_del(trace_event_id_t, trace_event_set_t *);

extern int posix_trace_eventset_empty(trace_event_set_t *);

extern int posix_trace_eventset_fill(trace_event_set_t *, int);

extern int posix_trace_eventset_ismember(trace_event_id_t,
                                         const trace_event_set_t *restrict,
                                         int *restrict);

extern int posix_trace_eventtypelist_getnext_id(trace_id_t,
                                                trace_event_id_t *restrict,
                                                int *restrict);

int posix_trace_eventtypelist_rewind(trace_id_t);

extern int posix_trace_flush(trace_id_t);

extern int posix_trace_get_attr(trace_id_t, trace_attr_t *);

extern int posix_trace_get_filter(trace_id_t, trace_event_set_t *);

extern int posix_trace_get_status(trace_id_t,
                                  struct posix_trace_status_info *);

extern int posix_trace_getnext_event(trace_id_t,
                                     struct posix_trace_event_info *restrict,
                                     void *restrict, size_t, size_t *restrict,
                                     int *restrict);

extern int posix_trace_open(int, trace_id_t *);

extern int posix_trace_rewind(trace_id_t);

extern int posix_trace_set_filter(trace_id_t, const trace_event_set_t *, int);

extern int posix_trace_shutdown(trace_id_t);

extern int posix_trace_start(trace_id_t);

extern int posix_trace_stop(trace_id_t);

extern int posix_trace_timedgetnext_event(trace_id_t,
                                          struct posix_trace_event_info
                                          *restrict, void *restrict, size_t,
                                          size_t *restrict, int *restrict,
                                          const struct timespec *restrict);

extern int posix_trace_trid_eventid_open(trace_id_t, const char *restrict,
                                         trace_event_id_t *restrict);

extern int posix_trace_trygetnext_event(trace_id_t,
                                        struct posix_trace_event_info
                                        *restrict, void *restrict, size_t,
                                        size_t *restrict, int *restrict);

__END_DECLS

__POP_FC_STDLIB
#endif
