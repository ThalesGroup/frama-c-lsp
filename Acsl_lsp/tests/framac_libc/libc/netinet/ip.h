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

// This is a non-POSIX file, provided for better compatibility on a
// best-effort basis.

#ifndef __NETINET_IP_H
#define __NETINET_IP_H
#include "../features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS
#include "../__fc_inet.h"

// The definitions below suppose a little endian architecture.

struct timestamp
{
  uint8_t len;
  uint8_t ptr;
  unsigned int flags:4;
  unsigned int overflow:4;
  uint32_t data[9];
};

struct iphdr
{
  unsigned int ihl:4;
  unsigned int version:4;
  uint8_t tos;
  uint16_t tot_len;
  uint16_t id;
  uint16_t frag_off;
  uint8_t ttl;
  uint8_t protocol;
  uint16_t check;
  uint32_t saddr;
  uint32_t daddr;
};

struct ip
{
  unsigned int ip_hl:4;
  unsigned int ip_v:4;
  uint8_t ip_tos;
  unsigned short ip_len;
  unsigned short ip_id;
  unsigned short ip_off;
  uint8_t ip_ttl;
  uint8_t ip_p;
  unsigned short ip_sum;
  struct in_addr ip_src, ip_dst;
};

#define IP_RF 0x8000
#define IP_DF 0x4000
#define IP_MF 0x2000
#define IP_OFFMASK 0x1fff

struct ip_timestamp
{
  uint8_t ipt_code;
  uint8_t ipt_len;
  uint8_t ipt_ptr;
  unsigned int ipt_flg:4;
  unsigned int ipt_oflw:4;
  uint32_t data[9];
};

#define IPVERSION 4
#define IP_MAXPACKET 65535

#define IPTOS_ECN_MASK 0x03
#define IPTOS_ECN(x) ((x) & IPTOS_ECN_MASK)
#define IPTOS_ECN_NOT_ECT 0x00
#define IPTOS_ECN_ECT1 0x01
#define IPTOS_ECN_ECT0 0x02
#define IPTOS_ECN_CE 0x03

#define IPTOS_DSCP_MASK 0xfc
#define IPTOS_DSCP(x) ((x) & IPTOS_DSCP_MASK)
#define IPTOS_DSCP_AF11 0x28
#define IPTOS_DSCP_AF12 0x30
#define IPTOS_DSCP_AF13 0x38
#define IPTOS_DSCP_AF21 0x48
#define IPTOS_DSCP_AF22 0x50
#define IPTOS_DSCP_AF23 0x58
#define IPTOS_DSCP_AF31 0x68
#define IPTOS_DSCP_AF32 0x70
#define IPTOS_DSCP_AF33 0x78
#define IPTOS_DSCP_AF41 0x88
#define IPTOS_DSCP_AF42 0x90
#define IPTOS_DSCP_AF43 0x98
#define IPTOS_DSCP_EF 0xb8

#define IPTOS_CLASS_MASK 0xe0
#define IPTOS_CLASS(class) ((class) & IPTOS_CLASS_MASK)
#define IPTOS_CLASS_CS0 0x00
#define IPTOS_CLASS_CS1 0x20
#define IPTOS_CLASS_CS2 0x40
#define IPTOS_CLASS_CS3 0x60
#define IPTOS_CLASS_CS4 0x80
#define IPTOS_CLASS_CS5 0xa0
#define IPTOS_CLASS_CS6 0xc0
#define IPTOS_CLASS_CS7 0xe0

#define IPTOS_CLASS_DEFAULT IPTOS_CLASS_CS0

#define IPTOS_TOS_MASK 0x1E
#define IPTOS_TOS(tos) ((tos) & IPTOS_TOS_MASK)
#define IPTOS_LOWDELAY 0x10
#define IPTOS_THROUGHPUT 0x08
#define IPTOS_RELIABILITY 0x04
#define IPTOS_LOWCOST 0x02
#define IPTOS_MINCOST IPTOS_LOWCOST

#define IPTOS_PREC_MASK IPTOS_CLASS_MASK
#define IPTOS_PREC(tos) IPTOS_CLASS(tos)
#define IPTOS_PREC_NETCONTROL IPTOS_CLASS_CS7
#define IPTOS_PREC_INTERNETCONTROL IPTOS_CLASS_CS6
#define IPTOS_PREC_CRITIC_ECP IPTOS_CLASS_CS5
#define IPTOS_PREC_FLASHOVERRIDE IPTOS_CLASS_CS4
#define IPTOS_PREC_FLASH IPTOS_CLASS_CS3
#define IPTOS_PREC_IMMEDIATE IPTOS_CLASS_CS2
#define IPTOS_PREC_PRIORITY IPTOS_CLASS_CS1
#define IPTOS_PREC_ROUTINE IPTOS_CLASS_CS0

#define IPOPT_COPY 0x80
#define IPOPT_CLASS_MASK 0x60
#define IPOPT_NUMBER_MASK 0x1f

#define IPOPT_COPIED(o) ((o) & IPOPT_COPY)
#define IPOPT_CLASS(o) ((o) & IPOPT_CLASS_MASK)
#define IPOPT_NUMBER(o) ((o) & IPOPT_NUMBER_MASK)

#define IPOPT_CONTROL 0x00
#define IPOPT_RESERVED1 0x20
#define IPOPT_DEBMEAS 0x40
#define IPOPT_MEASUREMENT IPOPT_DEBMEAS
#define IPOPT_RESERVED2 0x60

#define IPOPT_EOL 0
#define IPOPT_END IPOPT_EOL
#define IPOPT_NOP 1
#define IPOPT_NOOP IPOPT_NOP

#define IPOPT_RR 7
#define IPOPT_TS 68
#define IPOPT_TIMESTAMP IPOPT_TS
#define IPOPT_SECURITY 130
#define IPOPT_SEC IPOPT_SECURITY
#define IPOPT_LSRR 131
#define IPOPT_SATID 136
#define IPOPT_SID IPOPT_SATID
#define IPOPT_SSRR 137
#define IPOPT_RA 148

#define IPOPT_OPTVAL 0
#define IPOPT_OLEN 1
#define IPOPT_OFFSET 2
#define IPOPT_MINOFF 4

#define MAX_IPOPTLEN 40

#define IPOPT_TS_TSONLY 0
#define IPOPT_TS_TSANDADDR 1
#define IPOPT_TS_PRESPEC 3

#define IPOPT_SECUR_UNCLASS 0x0000
#define IPOPT_SECUR_CONFID 0xf135
#define IPOPT_SECUR_EFTO 0x789a
#define IPOPT_SECUR_MMMM 0xbc4d
#define IPOPT_SECUR_RESTR 0xaf13
#define IPOPT_SECUR_SECRET 0xd788
#define IPOPT_SECUR_TOPSECRET 0x6bc5

#define MAXTTL 255
#define IPDEFTTL 64
#define IPFRAGTTL 60
#define IPTTLDEC 1

#define IP_MSS 576

__END_DECLS
__POP_FC_STDLIB

#endif
