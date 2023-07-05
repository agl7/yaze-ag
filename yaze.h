/* Header file for the main program.
   Copyright (C) 1995  Frank D. Cringle.
   Modifications for CP/M 3.1 Copyright (C) 2000/2004 by Andreas Gerlich (agl)

This file is part of yaze-ag - yet another Z80 emulator by ag.

Yaze-ag is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

#define VERSION "2.51.3"
#define	DEVVER	"04"		/* developer version	*/
#define MAINVersion	0x02
#define SUBVersion	0x51
#define DEVELVersion	0

/* #define	DEVVER	"RC4"	\* release candidate */
/* Now it is in the Makefiles
#define	BUILD	" (build for INTEL x86_64Bit (core2 Duo))"
*/

#define	CCP_LENGTH	0x800
#define BDOS_LENGTH	0xe00
#define CPM_LENGTH	(CCP_LENGTH + BDOS_LENGTH)

#define CPM_COMMAND_LINE_LENGTH	0x80

extern int always128;	/* is set if all drives uses 128 byte sektors */

extern WORD	ccp_base;		/* base address of ccp */
extern WORD	bdos_base;		/* base address of bdos */
extern WORD	bios_base;		/* base address of bios */
extern WORD	bios_top;		/* end of bios, start of
					   global work area */

extern WORD	dirbuff;		/* common directory buffer for
					   all disks */
extern WORD	dptable;		/* base of disk parameter
					   headers table */
extern BYTE	*global_alv;		/* global allocation vector */

extern long	z3env;			/* z-system environment (none if z3env==0) */

extern void init_mmu();
extern void bios(int func);
extern int  bios_init(const char *initfile);
extern void initSIGWINSZ();
extern int  docmd(char *cmd);
extern void monitor(FASTWORK adr);
extern int  remount(int disk);
extern void showdisk(int disk, int verbose);
extern void *xmalloc(size_t size);
extern char *newstr(const char *str);

#ifdef USE_GNU_READLINE
#include <readline/readline.h>
void add_history(char *cmd);
#else
#define add_history(x)
#endif

#ifdef BSD
#if defined(sun)
#include <memory.h>
#include <string.h>
#endif
#ifndef strchr
#define strchr index
#endif
#ifndef strrchr
#define strrchr rindex
#endif
#define memclr(p,n)	bzero(p,n)
#define memcpy(t,f,n)	bcopy(f,t,n)
#define memcmp(p1,p2,n)	bcmp(p1,p2,n)
#define memset(p,v,n)							\
    do { size_t len = n;						\
	 char *p1 = p;							\
	 while (len--) *p1++ = v;					\
    } while (0)
#else
#include <string.h>
#define memclr(p,n)	(void) memset(p,0,n)
#endif
