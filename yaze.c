/* yaze-ag - yet another Z80 emulator by ag.
   Copyright (C) 1995,1998  Frank D. Cringle.
   Modifications for CP/M 3.1 Copyright (C) 2000/2004 by Andreas Gerlich (agl)

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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
/* #include <stdbool.h> */

#include "ytypes.h"

#include "mem_mmu.h"
#include "simz80.h"
#include "yaze.h"

/* Z80 registers */
WORD af[2];			/* accumulator and flags (2 banks) */
int af_sel;			/* bank select for af */

struct ddregs regs[2];		/* bc,de,hl */
int regs_sel;			/* bank select for ddregs */

WORD ir;			/* other Z80 registers */
WORD ix;
WORD iy;
WORD sp;
WORD pc;
WORD IFF;

/* definitiions for the brake */
FASTREG tStates_save;
FASTREG clockFrequency_save = 0;	/* default no brake */
FASTREG startTime_save;
FASTREG tStatesInSlice_save;
bool brakeini = true;		/* must be true! */

/* See definitions for memory in mem_mmu.[c,h] (agl) */

#ifndef LIBDIR
#define LIBDIR "/usr/local/lib/yaze"
#endif

#ifdef BOOTSYS
static char *bootfile = "yaze-cpm3.boot";
#else
static char *bootfile = "yaze.boot";
#endif
static char *startup = ".yazerc";
static char *progname;
#ifdef BOOTSYS
static long loadadr = 0x100;
#else
static long loadadr = -1;
#endif
#ifdef BIOS
static long basepage = 0xe4;
static int vflag;
int always128 = 0;	/* is set if all drives uses 128 byte sectors	*/
			/* only used under CP/M 3.1			*/
long z3env;

WORD	ccp_base;		/* base address of ccp */
WORD	bdos_base;		/* base address of bdos */
WORD	bios_base;		/* base address of bios */
WORD	bios_top;		/* end of bios, start of global work area */

WORD	dirbuff;		/* common directory buffer for all disks */
WORD	dptable;		/* base of disk parameter table */
BYTE	*global_alv;		/* global allocation vector */
#endif

extern bool rtc_avail; /* nur zum Testen */

static void
usage(void)
{
    char c;

    fprintf(stderr,
            "Usage: %s {flags} {commands}\n"
            "   -b <file>       boot from this file\n"
            "   -l <xxxx>       load bootfile at given (hex) address\n"
#ifdef BIOS
            "   -p <xx>         base page (top 2 hex digits of ccp base)\n"
#endif
            "   -s <file>       startup file\n" ,progname);

#ifdef BIOS
    fprintf(stderr,
	    "   -h <n>          screen height (default 25)\n"
	    "   -w <n>          screen width (default 80)\n"
            "   -v              verbose\n"
            "   -z <xxxx>       (hex) address of z3env (if desired)\n"
            "   -1              all drives uses 128 byte sectors (under CP/M 3.1)\n"
            "                   (normal 2048 byte sectors are used, 128 byte\n"
            "                   sectors are necessary to use a disk editor)\n\n"
	    "Press RETURN to continue ...");
    read(fileno(stdin), &c, 1);
    fprintf(stderr,
	    "\n"
	    "Features: MMU:     Memory Management Unit with 16 tables and 16 page pointers\n"
	    "   of   :          per table. This is the basis for running CP/M 3.1. A bank\n"
	    "YAZE-AG :          switch is simple done with a pointer to one of the 16 tables\n"
	    "                   and is very fast.\n\n"
	    "          KeyTr:   Key translation: Allows to use the cursor keys of the PC in\n"
	    "                   Wordstar and on the command line of CP/M 3.1. Commands can\n"
	    "                   be put on function keys and many more. This software module\n"
	    "                   is Copyright (c) by John Saxton (see yaze-keys.pdf).\n\n"
	    "          WinSz:   Window size handler/routine: This handler initializes the\n"
	    "                   lines and columns in the SCB of CP/M 3.1 like the lines and\n"
	    "                   columns of the used terminal emulator. The CP/M 3.1 system\n"
	    "                   utilities (like TYPE, DIR, ...) and the patched Wordstar\n"
	    "                   4.00 (A:WS.COM) uses this two variables to setup the screen.\n\n"
	    "          MHz:     You can define a clock rate in the monitor of yaze-ag.\n"
	    "                   This allows to run the emulator at the defined clockrate\n"
	    "                   like e.g. \"4000\" for 4000 kHz (4.0 MHz) or any other clock\n"
	    "                   rate. Clock rate \"0\" returns to max speed.\n\n"
	    "Press RETURN to continue ...");
    read(fileno(stdin), &c, 1);
    fprintf(stderr,
	    "\n"
	    "          ZSM4:    ZSM4 is a free Macro Assembler for the Z80/Z180/Z280. The\n"
	    "                   Assembler is created by Hector Peraza. The project page is\n"
	    "                   on \"https://github.com/hperaza/ZSM4\". The licence is the\n"
	    "                   GPLv2 (General Public Licence Version 2). The source files\n"
	    "                   of this Assembler can be mounted on drive O:.\n"
	    "                   This Assembler is now the standard for assemble BOOTSYS and\n"
	    "                   the BIOS3-Z80-files for YAZE-AG (see drive P:).\n\n"
	    "Copyright (c) 1995,1998 by Frank D.Cringle (until yaze-1.10, CP/M 2.2).\n\n"
	    "Keytranslation is Copyright (c) 2000,2015 by Jon Saxton (Australia).\n\n"
	    "The basis for the MMU was the idea of the Pagetables by Michael Haardt.\n\n"
	    "MMU, CP/M 3.1 and other extensions Copyright (c) 2000,2022 by Andreas Gerlich.\n"
	    "(See the file yaze-ag_doc.html in the documentation about the news features of\n"
	    " yaze-ag-2.xx.x since yaze-1.10.)\n\n"
	    "The License of this Open Source Project is the GNU GENERAL PUBLIC LICENSE\n"
	    "Version 2, June 1991 (GPLv2).\n\n"
	
	    "Press RETURN to continue ...");
    read(fileno(stdin), &c, 1);
#endif

    exit (1);
}

/* This routine loads the bootstrap file into the simulated processor's ram.

   If a load address is given with the -l flag, the boot file is
   loaded to that address, the z80 pc is set to it and hl is set to
   the ccp base address.  The bootfile might be a standalone program
   which ignores the bios vectors or it might contain a
   self-relocating ccp/bdos (clone), in which case it should relocate
   itself and jump to the bios cold boot vector.

   Otherwise, the boot file can contain 1 or 2 copies of cp/m.  If it
   contains 1 copy the specified or default basepage must correspond
   to the run-time location of the cp/m image.  If it contains 2
   copies of the same code assembled to run at different locations,
   the code can and will be relocated to fit the basepage in use. */

static void
load_cpm(const char *bfname)
{
    FILE *cpm;
#ifdef BIOS
    int i, skip = 0;
    WORD offs;
    struct stat	sb;
#endif

    if ((cpm = fopen(bfname, "r")) == NULL)
    {
	char *p = xmalloc(strlen(bfname) + strlen(LIBDIR) + 1);
	strcpy(p, LIBDIR);
	strcat(p, bfname);
	if ((cpm = fopen(p, "r")) == NULL)
	    free(p);
	else
	    bfname = p;
    }
    if (cpm == NULL)
    {
	perror(bfname);
	exit(1);
    }
    if (loadadr >= 0)
    {
	fread(ram+loadadr, 1, 64*1024-loadadr, cpm);
	fclose(cpm);
	pc = loadadr;
#ifdef BIOS
	regs[regs_sel].hl = ccp_base;
#endif
	printf("Running '%s'\n", bfname);
	return;
    }
#ifdef BIOS
    if (fstat(fileno(cpm), &sb) != 0)
    {
	perror(bfname);
	exit(1);
    }
    if ((sb.st_size >= CPM_LENGTH+0x880 && sb.st_size < 2*CPM_LENGTH) ||
	(sb.st_size >= 2*CPM_LENGTH+0x880))
    {
	skip = 0x880;
	fseek(cpm, skip, SEEK_SET);
    }
    fread(ram + ccp_base, CPM_LENGTH, 1, cpm);
    if (sb.st_size < 2*CPM_LENGTH) {
	/* single, non-relocatable copy */
	if (ram[ccp_base] == 0xc3 && (ram[ccp_base+2]&0xfc) != basepage)
	    printf("Warning: %s appears to need -p %2x\n", bfname,
		   ram[ccp_base+2]&0xfc);
    }
    else
    {
	if ((sb.st_size & 1) || ram[ccp_base] != 0xc3)
	{
	    fprintf(stderr, "%s does not appear to contain 2 cp/m images\n", bfname);
	    exit(1);
	}
	fseek(cpm, skip + sb.st_size/2, SEEK_SET);
	/* this loses if the initial jump points into the 2nd half of ccp */
	offs = (ccp_base >> 8) - (ram[ccp_base + 2] & 0xfc);
	for (i = 0; i < CPM_LENGTH; i++)
	{
	    if ((BYTE) getc(cpm) != ram[ccp_base + i])
		ram[ccp_base + i] += offs;
	}
    }
    fclose(cpm);
    printf("Running '%s'\n", bfname);
    PutBYTE(3, 0x95);				/* iobyte */
    PutBYTE(4, 0);				/* disk A: */
    pc = bios_base;
#endif
}

#ifdef BIOS
#define N_VEC	17			/* number of bios entry points */

static void
setvectors(void)
{
    int i;
    long cpm_brk = 64*1024;

    if (z3env == cpm_brk - 1024)	/* z3env at top of memory? */
	cpm_brk = z3env;
    cpm_brk -= 128;			/* space for directory buffer */
    dirbuff = cpm_brk;
    cpm_brk -= 16*(16+15);		/* space for 16 disk parameter headers
					   and disk parameter blocks */
    dptable = cpm_brk;

    if (basepage*256 + CPM_LENGTH + 6*N_VEC >= dptable) {
	fprintf(stderr, "Not enough space in simulated ram to accommodate base at %02lX\n",
		basepage);
	exit(1);
    }

    for (i = 0; i < N_VEC; i++) { /* build bios jump table */
	BYTE *p = ram + bios_base + 3*i;
	p[0] = 0xc3;		/* jump */
	p[1] = 3*(i+N_VEC);
	p[2] = (bios_base >> 8);
	p[3*N_VEC] = 0x76;	/* halt */
	p[3*N_VEC+1] = i;	/* function code */
	p[3*N_VEC+2] = 0xc9;	/* return */
    }
    bios_top = bios_base + 6*N_VEC;
    i = (dptable - bios_top + 7) / 8;
    global_alv = xmalloc(i);
    memclr(global_alv, i);
}
#endif

/* getopt declarations */
extern int getopt(int argc, char * const argv[],
		  const char *optstring);
extern char *optarg;
extern int optind, opterr, optopt;

/* Core Z80Core = simz80;	\* funktion pointer default simz80() */

int
main(int argc, char **argv)
{
    int
	c,
	gostart,
	term_height = 25,
	term_width  = 80;

#ifdef BIOS
    int goopt;
#endif

    progname = argv[0];

/*  printf("\033]0;YAZE-AG-%s-%s%s\007",VERSION,DEVVER,BUILD); */
    printf("\033]0;YAZE-AG-%s%s\007",VERSION,BUILD);

    puts("\nYet Another Z80 Emulator by AG, "
	"\033[32m"

	"final release " VERSION 
/*	"development v" VERSION "-" DEVVER "_beta" */
/*	"release candidate " VERSION "-" DEVVER */

	"\033[0m"

#ifdef UCSD
	"/UCSD"
#endif
/*
#ifdef CYGWIN
	"\033[33m"
#endif
	BUILD 
#ifdef CYGWIN
	"\033[0m"
#endif
*/
	/* " (devel version)" */
#ifdef MMU
	"\r\nFeatures: MMU,KeyTr,WinSz,MHz,ZSM4, Copyright (use the flag \"-?\" for details)."
#endif
	"\r\nYAZE-AG comes with ABSOLUTELY NO WARRANTY; for details "
	"see the file \"COPYING\".\r\n");

    while ((c = getopt(argc, argv, "b:l:s:h:w:"
#ifdef BIOS
                                           "p:vz:1"
#endif

                                           "?"       )) != EOF)
	switch (c)
	{
	case 'b':
	    bootfile = optarg;
	    break;
	case 'l':
	    loadadr = strtol(optarg, NULL, 16);
	    break;
	case 's':
	    startup = optarg;
	    break;
#ifdef BIOS
	case 'p':
	    basepage = strtol(optarg, NULL, 16);
	    break;
	case 'v':
	    vflag = 1;
	    break;
	case 'z':
	    z3env = strtol(optarg, NULL, 16);
	    break;
	case '1':
	    always128 = 1;
	    break;
#endif
	case 'h':
	    term_height = strtol(optarg, NULL, 10);
	    break;
	case 'w':
	    term_width = strtol(optarg, NULL, 10);
	    break;
	case '?':
	    usage();
	}

#ifdef MMU
    initMEM();	/* initialice memory */
    initMMU();  /* initialice MMU */
#endif

#ifdef BIOS
    ccp_base = basepage * 256;
    bdos_base = ccp_base + CCP_LENGTH;
    bios_base = bdos_base + BDOS_LENGTH;

    /* Plug screen height and width int SCB */
    ram[bios_base - 0x100 + 0xBA] = term_height - 1;
    ram[bios_base - 0x100 + 0xBC] = term_width - 1;

    if (z3env == bios_base)
	bios_base += 1024;
#endif

    load_cpm(bootfile);
#ifdef BIOS
    setvectors();

    if (vflag)
    {
	printf("bootfile:	%s\n", bootfile);
	if (loadadr >= 0)
	    printf("loadadr:	%4lx\n", loadadr);
	printf("startup file:	%s\n"
	       "basepage:	%2lx\n"
	       "ccp_base:	%4hx\n"
	       "bdos_base:	%4hx\n"
	       "bios_base:	%4hx\n"
	       "bios_top:	%4hx\n",
	       startup, basepage, ccp_base,
	       bdos_base, bios_base, bios_top);
	if (z3env)
	    printf("z3env:		%4lx\n", z3env);
	printf("dptable:	%4hx\n"
	       "dirbuf:		%4hx\n",
	       dptable, dirbuff);
    }
#endif

    gostart = bios_init(startup);

#ifdef BIOS
    for (goopt = 0; !goopt && optind < argc; optind++)
	goopt = docmd(argv[optind]);

    if (!gostart && !goopt)
	monitor(0);
#endif

#ifdef DEBUG
    do
    {
	FASTWORK adr = simz80(pc);
	if (adr & 0x10000)
	    monitor(adr);
	else
	    bios(ram[adr]);
    } while (1);
#elif !BIOS
    simz80(pc);
    fprintf(stderr,"HALT\n\r");
    fprintf(stderr,"PC   SP   IR   IX   IY   AF   BC   DE   HL   AF'  BC'  DE'  HL'\n\r");
    fprintf(stderr,"%04x %04x %04x %04x %04x %04x %04x %04x %04x %04x %04x %04x %04x\n\r",pc,sp,ir,ix,iy,af[af_sel],regs[regs_sel].bc,regs[regs_sel].de,regs[regs_sel].hl,af[1-af_sel],regs[1-regs_sel].bc,regs[1-regs_sel].de,regs[1-regs_sel].hl);
    exit(0);
#else
    do
    {
	pc = clockFrequency_save ? simz80_with_tStates(pc) : simz80(pc);
	bios(GetBYTE(pc));
    } while (1);
#endif
}

#ifndef USE_GNU_READLINE
void *
xmalloc(size_t size)
{
    void *p = malloc(size);

    if (p == NULL)
    {
	fputs("insufficient memory\n", stderr);
	exit(1);
    }
    return p;
}
#endif
