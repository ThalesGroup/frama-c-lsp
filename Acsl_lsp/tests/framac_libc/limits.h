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

/* ISO C: 7.10 and 5.2.4.2.1 */
#ifndef __FC_LIMITS
#define __FC_LIMITS

#include "__fc_machdep.h"

/* Number of bits in a `char'.	*/
#define CHAR_BIT __CHAR_BIT


/* Minimum and maximum values a `signed char' can hold.  */
#  define SCHAR_MIN	__FC_SCHAR_MIN
#  define SCHAR_MAX	__FC_SCHAR_MAX

/* Maximum value an `unsigned char' can hold.  (Minimum is 0.)  */
#  define UCHAR_MAX	__FC_UCHAR_MAX

/* Minimum and maximum values a `char' can hold.  */
#  ifdef __CHAR_UNSIGNED__
#   define CHAR_MIN	0
#   define CHAR_MAX	UCHAR_MAX
#  else
#   define CHAR_MIN	SCHAR_MIN
#   define CHAR_MAX	SCHAR_MAX
#  endif

#define MB_LEN_MAX 16

/* Minimum and maximum values a `signed short int' can hold.  */
#  define SHRT_MIN	__FC_SHRT_MIN
#  define SHRT_MAX	__FC_SHRT_MAX

/* Maximum value an `unsigned short int' can hold.  (Minimum is 0.)  */
#  define USHRT_MAX     __FC_USHRT_MAX

/* Minimum and maximum values a `signed int' can hold.  */
#  define INT_MIN	__FC_INT_MIN
#  define INT_MAX	__FC_INT_MAX

/* Maximum value an `unsigned int' can hold.  (Minimum is 0.)  */
#  define UINT_MAX	__FC_UINT_MAX

/* Minimum and maximum values a `signed long int' can hold.  */
#  define LONG_MAX	__FC_LONG_MAX
#  define LONG_MIN	__FC_LONG_MIN

/* Maximum value an `unsigned long int' can hold.  (Minimum is 0.)  */
#define ULONG_MAX	__FC_ULONG_MAX

/* Minimum and maximum values a `signed long long int' can hold.  */
#   define LLONG_MAX	__FC_LLONG_MAX
#   define LLONG_MIN	__FC_LLONG_MIN

/* Maximum value an `unsigned long long int' can hold.  (Minimum is 0.)  */
#   define ULLONG_MAX	__FC_ULLONG_MAX

// POSIX-specific definitions

/*** Most restrictive values for the constants below, as mandated by POSIX */

// Maximum Values

#define _POSIX_CLOCKRES_MIN 20000000

// Minimum Values

#define _POSIX_AIO_LISTIO_MAX 2
#define _POSIX_AIO_MAX 1
#define _POSIX_ARG_MAX 4096
#define _POSIX_CHILD_MAX 25
#define _POSIX_DELAYTIMER_MAX 32
#define _POSIX_HOST_NAME_MAX 255
#define _POSIX_LINK_MAX 8
#define _POSIX_LOGIN_NAME_MAX 9
#define _POSIX_MAX_CANON 255
#define _POSIX_MAX_INPUT 255
#define _POSIX_MQ_OPEN_MAX 8
#define _POSIX_MQ_PRIO_MAX 32
#define _POSIX_NAME_MAX 14
#define _POSIX_NGROUPS_MAX 8
#define _POSIX_OPEN_MAX 20
#define _POSIX_PATH_MAX 256
#define _POSIX_PIPE_BUF 512
#define _POSIX_RE_DUP_MAX 255
#define _POSIX_RTSIG_MAX 8
#define _POSIX_SEM_NSEMS_MAX 256
#define _POSIX_SEM_VALUE_MAX 32767
#define _POSIX_SIGQUEUE_MAX 32
#define _POSIX_SSIZE_MAX 32767
#define _POSIX_SS_REPL_MAX 4
#define _POSIX_STREAM_MAX 8
#define _POSIX_SYMLINK_MAX 255
#define _POSIX_SYMLOOP_MAX 8
#define _POSIX_THREAD_DESTRUCTOR_ITERATIONS 4
#define _POSIX_THREAD_KEYS_MAX 128
#define _POSIX_THREAD_THREADS_MAX 64
#define _POSIX_TIMER_MAX 32
#define _POSIX_TRACE_EVENT_NAME_MAX 30
#define _POSIX_TRACE_NAME_MAX 8
#define _POSIX_TRACE_SYS_MAX 8
#define _POSIX_TRACE_USER_EVENT_MAX 32
#define _POSIX_TTY_NAME_MAX 9
#define _POSIX_TZNAME_MAX 6
#define _POSIX2_BC_BASE_MAX 99
#define _POSIX2_BC_DIM_MAX 2048
#define _POSIX2_BC_SCALE_MAX 99
#define _POSIX2_BC_STRING_MAX 1000
#define _POSIX2_CHARCLASS_NAME_MAX 14
#define _POSIX2_COLL_WEIGHTS_MAX 2
#define _POSIX2_EXPR_NEST_MAX 32
#define _POSIX2_LINE_MAX 2048
#define _POSIX2_RE_DUP_MAX 255
#define _XOPEN_IOV_MAX 16
#define _XOPEN_NAME_MAX 255
#define _XOPEN_PATH_MAX 1024

// Other Invariant Values

#define NL_ARGMAX 9
#define NL_LANGMAX 14
#define NL_MSGMAX 32767
#define NL_SETMAX 255
#define NL_TEXTMAX _POSIX2_LINE_MAX
#define NZERO 20

/*** POSIX constants. Frama-C usually takes Linux' values by default, but
     it can be overloaded by passing e.g. `-cpp-extra-args="-D__FC_XXXX=nnnn"`
     on Frama-C's command line. A value of -1 will deactivate the macro (all of
     them are optional).
*/
#define STR(S) # S
#define expand(macro) STR(macro)

/* Maximum number of bytes in a pathname, including the terminating
   null character. (Minimum is 256.) */
#ifdef __FC_PATH_MAX
#  if __FC_PATH_MAX >= 0
      _Static_assert(__FC_PATH_MAX >=_POSIX_PATH_MAX, "__FC_PATH_MAX is too small (" expand(__FC_PATH_MAX) "): minimal value is " expand( _POSIX_PATH_MAX));
#      define PATH_MAX __FC_PATH_MAX
#  else
#    undef PATH_MAX
#  endif
#else
#  define PATH_MAX  4096
#endif

/* Maximum length of a host name (not including the terminating null)
   as returned from the gethostname() function.
   Note: Mac OS does not define this constant, and Linux use a strictly smaller
   one than what POSIX mandate
*/
#ifdef __FC_HOST_NAME_MAX
#  if __FC_HOST_NAME_MAX >= 0
     // _Static_assert(__FC_HOST_NAME_MAX >=_POSIX_HOST_NAME_MAX, "__FC_HOST_NAME_MAX is too small (" expand(__FC_HOST_NAME_MAX) "): minimal value is " expand(_POSIX_HOST_NAME_MAX));
#    define HOST_NAME_MAX __FC_HOST_NAME_MAX
#  else
#    undef HOST_NAME_MAX
#  endif
#else
#  define HOST_NAME_MAX 255
#endif

/* Maximum length of a terminal device name. */
#ifdef __FC_TTY_NAME_MAX
#  if __FC_TTY_NAME_MAX >= 0
     _Static_assert(__FC_HOST_NAME_MAX >=_POSIX_TTY_NAME_MAX, "__FC_TTY_NAME_MAX is too small (" expand(__FC_TTY_NAME_MAX) "): minimal value is " expand(_POSIX_TTY_NAME_MAX));
#    define TTY_NAME_MAX __FC_TTY_NAME_MAX
#  else
#    undef TTY_NAME_MAX
#  endif
#else
#  define TTY_NAME_MAX 9
#endif

/* Maximum length of argument to the exec functions including environment data.
   Minimum Acceptable Value: {_POSIX_ARG_MAX} (4096 in POSIX.1-2008)
   "... the total space used to store the environment and the arguments to the
    process is limited to {ARG_MAX} bytes."
 */
#ifdef __FC_ARG_MAX
#  if __FC_ARG_MAX >= 0
     _Static_assert(__FC_ARG_MAX >=_POSIX_ARG_MAX, "__FC_ARG_MAX is too small (" expand(__FC_ARG_MAX) "): minimal value is " expand(__POSIX_ARG_MAX));
#    define ARG_MAX __FC_ARG_MAX
#  else
#    undef ARG_MAX
#  endif
#else
#  define ARG_MAX 4096
#endif

// POSIX; used by <sys/uio.h>.
// Must be >= _XOPEN_IOV_MAX, which is 16.
#ifdef __FC_IOV_MAX
#  if __FC_IOV_MAX >= 0
     _Static_assert(__FC_IOV_MAX >=_XOPEN_IOV_MAX, "__FC_IOV_MAX is too small (" expand(__FC_IOV_MAX) "): minimal value is " expand(_XOPEN_IOV_MAX));
#    define IOV_MAX __FC_IOV_MAX
#  else
#    undef IOV_MAX
#  endif
#else
#  define IOV_MAX 255
#endif

#endif
