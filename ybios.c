/*  Basic i/o system module (yaze-bios).
    Copyright (C) 1995  Frank D. Cringle.
    Modifications for keytranslation by Jon Saxton (c) 2015
    Modifications for CP/M 3.1 Copyright (C) 2000/2013 by Andreas Gerlich (agl)

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

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <limits.h>
#include <fcntl.h>
#include <termios.h>
#include <ctype.h>
#include <signal.h>
#include <sys/types.h>
#include <time.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <dirent.h>
#include <sys/mman.h>
#include <string.h>
#include <glob.h>
/* #include <stdbool.h> */
#include <sys/ioctl.h>


#include "mem_mmu.h"
#include "simz80.h"
#include "yaze.h"
#include "ybios.h"
#include "ktt.h"

/* Z80 registers */
#define AF	af[af_sel]
#define BC	regs[regs_sel].bc
#define DE	regs[regs_sel].de
#define HL	regs[regs_sel].hl

/* cp/m 3 */

int cpm3 = 0;		/* is 1 then CP/M 3 is running		*/
WORD dtbl = 0xfffe;	/* address of @dtbl in CP/M 3.1 bios	*/
static WORD yct;	/* z80 - pointer to @YCT		*/
static WORD t_sssd;	/* z80 - pointer to translation table SSSD */
static WORD scb;	/* Pointer to SCB (System Control Block) */
WORD maxdsm = 0x0;		/* maximal DSM, reported by the bios	*/
WORD maxdrm = 0x0;		/* maximal DRM, reported by the bios	*/

/* location on current disk */
static WORD	track;
static WORD	sector;
/* memory pointer */
static WORD	dma;

#ifdef MMU
static WORD	cbnk = 0; /* current and */
static WORD	dbnk = 0; /* destination bank, default 0 */
static WORD	bnkdest, bnksrc;

/* static pagetab_struct *dmmu = &MMUtable[0];  \* <-- see mem_mmu.c */
#endif

typedef unsigned long COUNT;
static COUNT	bioscount;
static BYTE	old_iobyte;

static void new_iobyte(int io)
{
    switch ((io >> 6) & 3)
    {
    case 0:
        chn[CHNlst] = TTYout;
        break;
    case 1:
        chn[CHNlst] = CRTout;
        break;
    case 2:
        chn[CHNlst] = LPTout;
        break;
    case 3:
        chn[CHNlst] = UL1out;
        break;
    }
    switch ((io >> 4) & 3)
    {
    case 0:
        chn[CHNpun] = TTYout;
        break;
    case 1:
        chn[CHNpun] = PUNout;
        break;
    case 2:
        chn[CHNpun] = UP1out;
        break;
    case 3:
        chn[CHNpun] = UP2out;
        break;
    }
    switch ((io >> 2) & 3)
    {
    case 0:
        chn[CHNrdr] = TTYin;
        break;
    case 1:
        chn[CHNrdr] = RDRin;
        break;
    case 2:
        chn[CHNrdr] = UR1in;
        break;
    case 3:
        chn[CHNrdr] = UR2in;
        break;
    }
    switch (io & 3)
    {
    case 0:
        chn[CHNconin] = TTYin;
        chn[CHNconout] = TTYout;
        break;
    case 1:
        chn[CHNconin] = CRTin;
        chn[CHNconout] = CRTout;
        break;
    case 2:
        chn[CHNconin] = chn[CHNrdr];
        chn[CHNconout] = chn[CHNlst];
        break;
    case 3:
        chn[CHNconin] = UC1in;
        chn[CHNconout] = UC1out;
        break;
    }
    old_iobyte = io;
    chn[CHNaux] = AUX;	/* necessary for CP/M 3 */
}

static struct sio *getsiop(int chan)
{
    if (GetBYTE(3) != old_iobyte)
        new_iobyte(GetBYTE(3));
    return &siotab[chn[chan]];
}

int bios_init(const char *initfile)
{
    int go = 0;
    FILE *su;
    const char *fn = initfile;
    char *name;
    char buf[BUFSIZ];

#ifdef DEBUG
    signal(SIGINT, sighand);
#endif
    if (z3env && z3env > bios_top && z3env < dptable)
    {
        /* reserve space for a z-system environment page */
        WORD p = z3env - bios_top;
        int i = 1024;
        while (i--)
        {
            global_alv[p >> 3] |= (0x80 >> (p & 7));
            p++;
        }
    }
    siotab[CRTin].fp = stdin;
    name = ttyname(fileno(stdin));
    siotab[CRTin].filename = name ? newstr(name) : "(stdin)";
    siotab[CRTin].tty = isatty(fileno(stdin));
    siotab[CRTout].fp = stdout;
    name = ttyname(fileno(stdout));
    siotab[CRTout].filename = name ? newstr(name) : "(stdout)";
    siotab[CRTout].tty = isatty(fileno(stdout));
    if (siotab[CRTin].tty)
    {
        ttyflags = ISATTY;
        if (tcgetattr(fileno(stdin), &cookedtio) != 0)
        {
            perror("tcgetattr");
            exit(1);
        }
        rawtio = cookedtio;
        rawtio.c_iflag = 0;
        rawtio.c_oflag = 0;
        rawtio.c_lflag = interrupt ? ISIG : 0;
        memclr(rawtio.c_cc, NCCS);
        rawtio.c_cc[VINTR] = interrupt;
        rawtio.c_cc[VMIN] = 1;
    }
    if (*initfile != '/' && access(initfile, R_OK) != 0)
    {
        char *h = getenv("HOME");
        if (h)
        {
            fn = buf;
            sprintf((char *) fn, "%s/%s", h, initfile);
        }
    }
    if ((su = fopen(fn, "r")) == NULL)
        return 0;
    while (!go && fgets(buf, BUFSIZ - 1, su))
        go = docmd(buf);
    fclose(su);
    atexit(ttycook);
    ttyraw();
    return go;
}


/********* handler for window size update *********/

volatile WORD lines = 0;
volatile WORD columns = 0;
volatile bool winsize_updated = false;
volatile int signal_number = 0;

bool update_winsize() {
   struct winsize winsize;
   if (ioctl(fileno(stdin), TIOCGWINSZ, &winsize) < 0) {
	return false;
   } else {
	lines = winsize.ws_row;
	columns = winsize.ws_col;
	return true;
   }
}

void winsize_handler(int signo) {
   signal_number = signo;
   winsize_updated = update_winsize();
}

void initSIGWINSZ()
{
   static struct sigaction action; /* must be static !!! */

   if (!update_winsize()) {
	perror("stdin"); exit(1);
   }
   /*
   else {
	printf("initSIGWINSZ: columns %hu, lines %hu\r\n", columns, lines);
   }
   */

   action.sa_handler = winsize_handler;

   if (sigaction(SIGWINCH, &action, 0) != 0) {
	perror("sigaction"); exit(1);
   }

   /* Set windows size in the SCB of CP/M 3 */
   if (cpm3) {
	PutBYTE(scb + SCB_columns, lreg(columns-1));
	PutBYTE(scb + SCB_lines, lreg(lines-1));
   }
   winsize_updated = true;
   /* printf("initSIGWINSZ: durchlaufen! SCB-Adresse: %4X\r\n", scb); */
}


/*  The constat() routine is complicated by the need to deal with CP/M programs
    that poll console status without doing any other bios calls.
    Examples are Wordstar and Backgrounder II.  In Wordstar the purpose is to
    act as a crude timer that can be interrupted by the user pressing a key
    (delay before a menu is shown).  In Backgrounder it's an artifact of the
    pseudo multitasking logic.

    A simple-minded constat() implementation leads to such CP/M programs soaking
    up 100% of the host CPU and getting confused about the system speed.

    The 2 constants CSTTIME and CSTCOUNT control the emulation.
    constat() normally returns the current console status immediately.
    However, after it has been called CSTCOUNT times without any other bios
    activity, it waits CSTTIME microseconds (or until a character is available)
    before returning.
*/

#define CSTTIME			100000
#define CSTCOUNT		  2000
#define INTERKEY_TIMEOUT	 50000

static const struct timeval immediate = { 0, 0 };
static const struct timeval delay = { 0, CSTTIME };

static int constat(void)
{
    static int consecutive;		/* number of consecutive calls */
    static COUNT lastcount;		/* previous call */
    struct timeval t = immediate;
    fd_set rdy;
    struct sio *s = getsiop(CHNconin);
    int fd;

    if (s->fp == NULL)			/* no file */
        return 0;
    if (s->tty == 0)			/* disk files are always ready */
        return 1;
    if (bioscount != lastcount + 1)
        consecutive = 0;
    else if (++consecutive == CSTCOUNT)
    {
        consecutive = 0;
        t = delay;
    }
    lastcount = bioscount;

    fd = fileno(s->fp);
    FD_ZERO(&rdy);
    FD_SET(fd, &rdy);
    (void) select(fd + 1, &rdy, NULL, NULL, &t);
    return FD_ISSET(fd, &rdy);
}

/*
    This is a slight variant of the constat() routine which does
    not spin many times before doing a timed wait.  It is used
    by the BIOS console input routine to handle input sequences
    which can be prefixes of multi-byte keystrokes.
*/

int contest()
{
    struct timeval t = { 0, INTERKEY_TIMEOUT };
    fd_set rdy;
    struct sio *s = getsiop(CHNconin);
    int fd;

    if (s->fp == NULL)			/* no file */
        return 0;
    if (s->tty == 0)			/* disk files are always ready */
        return 1;
    fd = fileno(s->fp);
    FD_ZERO(&rdy);
    FD_SET(fd, &rdy);
    (void) select(fd + 1, &rdy, NULL, NULL, &t);
    return FD_ISSET(fd, &rdy);
}

static int
lststat(void)
{
    static int consecutive;		/* number of consecutive calls */
    static COUNT lastcount;		/* previous call */
    struct timeval t = immediate;
    fd_set rdy;
    struct sio *s = getsiop(CHNlst);
    int fd;

    if (s->fp == NULL)			/* no file */
        return 0;
    if (s->tty == 0)			/* disk files are always ready */
        return 1;
    if (bioscount != lastcount + 1)
        consecutive = 0;
    else if (++consecutive == CSTCOUNT)
    {
        consecutive = 0;
        t = delay;
    }
    lastcount = bioscount;

    fd = fileno(s->fp);
    FD_ZERO(&rdy);
    FD_SET(fd, &rdy);
    (void) select(fd + 1, &rdy, NULL, NULL, &t);
    return FD_ISSET(fd, &rdy);
}

#ifdef MMU
static int
auxistat(void)
{
    static int consecutive;		/* number of consecutive calls */
    static COUNT lastcount;		/* previous call */
    struct timeval t = immediate;
    fd_set rdy;
    struct sio *s = getsiop(CHNaux);
    int fd;

    if (s->fp == NULL)			/* no file */
        return 0;
    if (s->tty == 0)			/* disk files are always ready */
        return 1;
    if (bioscount != lastcount + 1)
        consecutive = 0;
    else if (++consecutive == CSTCOUNT)
    {
        consecutive = 0;
        t = delay;
    }
    lastcount = bioscount;

    fd = fileno(s->fp);
    FD_ZERO(&rdy);
    FD_SET(fd, &rdy);
    (void) select(fd + 1, &rdy, NULL, NULL, &t);
    return FD_ISSET(fd, &rdy);
}

static int
auxostat(void)
{
    static int consecutive;		/* number of consecutive calls */
    static COUNT lastcount;		/* previous call */
    struct timeval t = immediate;
    fd_set rdy;
    struct sio *s = getsiop(CHNaux);
    int fd;

    if (s->fp == NULL)			/* no file */
        return 0;
    if (s->tty == 0)			/* disk files are always ready */
        return 1;
    if (bioscount != lastcount + 1)
        consecutive = 0;
    else if (++consecutive == CSTCOUNT)
    {
        consecutive = 0;
        t = delay;
    }
    lastcount = bioscount;

    fd = fileno(s->fp);
    FD_ZERO(&rdy);
    FD_SET(fd, &rdy);
    /* (void) select(fd+1, &rdy, NULL, NULL, &t); */
    (void) select(fd + 1, NULL, &rdy, NULL, &t);
    return FD_ISSET(fd, &rdy);
}
#endif

int
serin(int chan)
{
    char c;
    int ch;
    int count;
    struct sio *s = getsiop(chan);

    if (s->fp == NULL)
        return 0x1a;
    if (s->tty)
/*  {	if ( fileno(s->fp) == fileno(stdin) )  */
    {	if ( s->fp == stdin ) {

	  if (!winsize_updated && cpm3) {
		initSIGWINSZ();  /* install win size haendler only when cpm3
				    runs */
		if (columns > 99) {
		   printf("\r\n\nWARNING of the Window size signal handler: ");
		   printf("Current window size: %hu columns, %hu lines\r\n",
							columns, lines);
		   printf("ATTENTION: Wordstar 4.00 can only"
			  " correct display the screen"
			  " up to 99 columns!\r\n");
		   printf("\nPAUSE Press any Key ... ");
		   fflush(stdout);
		}
		/* 
		printf("serin: initSIGWINSZ durchlaufen!\r\n");
		printf("serin: columns %hu, lines %hu\r\n", columns, lines);
		printf("serin: GetByte(columns): %u, GetByte(lines): %u\r\n",
			GetBYTE(scb + SCB_columns), GetBYTE(scb + SCB_lines));
		*/
	  }
    
	  for(;;)
	    switch ( count = read(fileno(s->fp), &c, 1) ) {

		case 0:	  /*  printf("EOF (End Of File)\r\n");
			      exit(0);
			  */
			 return 0x1a;

		case -1: switch (errno) {
			  case EAGAIN:
				perror("read");
				printf("Error EAGAIN: fd refers to a file "
					"other than a socket and has been "
					"marked nonblocking (O_NONBLOCK), "
					"and the read would block."
					"See open(2) for further details on "
					"the O_NONBLOCK flag.");
				exit(1);
			  case EBADF:
				perror("read");
				printf("Error EBADF: fd is not a valid file "
					"descriptor\r\n"
					"or is not open for reading.\r\n");
				exit(1);
			  case EFAULT:
				perror("read");
				printf("Error EFAULT: buf is outside your "
					"accessible adress space.\r\n");
				exit(1);
			  case EINTR: /* signal */
				if ( (signal_number == SIGWINCH)
					&& winsize_updated ) {
				   printf("window size was updated: "
					  "%hu columns, %hu lines\r\n",
					   columns, lines);
				   if (columns > 99) {
				     printf("ATTENTION: Wordstar 4.00 can only"
					    " correct display the screen"
					    " up to 99 columns!\r\n");
				   }
				   if (cpm3) {
				     PutBYTE(scb + SCB_columns,lreg(columns-1));
				     PutBYTE(scb + SCB_lines, lreg(lines-1));
				   }
				   break;
				}
				if (signal_number != SIGWINCH) {
				    perror("read");
				    printf("An another signal as SIGWINCH was "
					   "generated!\r\nsignal number is: "
					   "%d\r\n",signal_number);
				}
				if (!winsize_updated) {
				    perror("read");
				    printf("Error in ioctl call in handler "
				 	   "routine.\r\n");
				}
				exit(1);
			  case EINVAL:
				perror("read");
				printf("Error EINVAL: fd is attached to an "
					"object which is unsuitable for "
					"reading.\r\n");
				exit(1);
			  case EIO:
				perror("read");
				printf("Error EIO: I/O error.\r\n");
				exit(1);
			  case EISDIR:
				perror("read");
				printf("Error EISDIR: fd refers to a "
					"directory.\r\n");
				exit(1);
			  default:
				perror("read");
				exit(1);
			 } /* end of switch (errno) */
			 break;

		case 1: return c; /* if one character -> return the character */

		default: printf("count: %d\n\r",count);
			 return c;

	   } /* end of switch ( read ...) and end of for(;;) */
	}
	else if (read(fileno(s->fp), &c, 1) == 0)
            return 0x1a;
        else
	    return c;
    } /* end of if (s->tty) */
    if ((ch = getc(s->fp)) == EOF)
        return 0x1a;
    else
        return ch & 0xff;
}

static void
serout(int chan, char c)
{
    struct sio *s = getsiop(chan);

    if (s->fp == NULL)
        return;
    if (s->tty)
        write(fileno(s->fp), &c, 1);
    else
        fwrite(&c, 1, 1, s->fp);
}

static WORD
seldsk(int disk)
{
    /*  ACHTUNG: Wenn die Translationtable gesetzt ist, muss, diese
    	    bei jedem select uebertragen werden ! Die Tabelle
    	    darf in bank0 im banked Bereich stehen (bisher ist noch
    	    Platz fuer 1 Tabelle im Common ohne das bios_base
    	    unterhalb von FE00H faellt). In Yaze wird allerdings
    	    lediglich die Tabelle fuer die IBM-Diskette unterstuetzt
    	    (siehe mount in monitor.c */
    /*  NOTE: If the Translationtable is set then it has to be
    	 transferred with each SELECT.  The table may reside in
    	 bank 0.  (There is room for one table in common memory
    	 without the bios_base dropping below FE00H).  Yaze only
    	 supports the table for the IBM diskette.
    	 (See mount in monitor.c */

    if (disk < 0 || disk > 15 || !(mnttab[disk].flags & MNT_ACTIVE))
        return 0;

    /*	 Superdos (CP/M 2.2 derivative) uses, like CP/M 3.1 (or ZPM3), the
        "initial select flag" (bit 0 of register E) to indicate if a disks
        is selected for the first time (bit0=0). I use this information to
        force a reread of a disk which is connected to a unix directory */

    if (!(DE & 0x0001) && (mnttab[disk].flags & MNT_UNIXDIR))
    {
        /*	 printf("\r\nSELDSK: BC=%04X DE=%04X (FIRST SELECT of a unix "
        		"directory)\r\n",BC,DE);
        */
        if (!remount(disk))
        {
            printf("\rBIOS-SELDSK: mount/remount of the directory failed !"
                   "\007\r\n"
                   "             Is the directory deleted or renamed ?"
                   "\r\n");
            return 0;
        }
    }
    curdisk = mnttab + disk;
    return curdisk->dph;
}

#ifdef BGii_BUG
/*  I cannot persuade Backgrounder ii to output the H at the end of a
    VT-100/xterm cursor-positioning sequence.  This kludges it. */
void
BGiiFix(int c)
{
    static const char cc[] = "\033[0;0";
    static const char *p = cc;

    if (c == 0)
        return;
    if ('0' <= c && c <= '9')
        return;
    if (*p == '0')
        p++;
    if (*p == c)
    {
        p++;
        return;
    }
    if (*p)
    {
        p = cc;
        return;
    }
    p = cc;
    if (c == 'H')
        return;
    serout(CHNconout, 'H');
}
#else
#define BGiiFix(x)
#endif

#define FCACHE_SIZE	4
static struct fc
{
    FILE *f;
    struct mnt *disk;
    struct fdesc *fd;
} fcache[FCACHE_SIZE];


/* clear the file cache when disk is unmounted */
void
clearfc(struct mnt *dp)
{
    int i;

    for (i = 0; i < FCACHE_SIZE; i++)
    {
        struct fc *f = fcache + i;
        if (f->f && f->disk == dp)
        {
            fclose(f->f);
            f->f = NULL;
        }
    }
}

static int
#ifdef MULTIO
readsec(int sec_cnt)
#else
readsec(void)
#endif
{
    unsigned long offset, blockno;
    BYTE buf[128];
#ifdef MMU
    FASTREG tmp2;
#endif

    if (curdisk == NULL || !(curdisk->flags & MNT_ACTIVE))
        return 1;

    if (cpm3)
        /*
        	offset = (track*GetWORD(curdisk->dpb) + sector)*128;
        */

        offset = (track * GetWORD(curdisk->dpb)
                  + (sector << curdisk->psh)) * 128;

    /* the shift with psh make it posible to use sektorsize >128 */
    /* IMPORTANT: The disk geometrie must fit the sektors. */
    else
        /* offset = (track*GetWORD(curdisk->dph+16) + sector)*128; */
        offset = (track * GetWORD(curdisk->dpb) + sector) * 128;

    if ((curdisk->flags & MNT_UNIXDIR) == 0)
    {
        /* the 'disk image' case is easy */
        /*  modified by agl
            memcpy(ram + dma, curdisk->data + offset, 128);
        */
        /*  once more changed by agl
            memcpy_put_z(dma, curdisk->data + offset, 128);
        */
        /* once more modified by agl: dmmu - destination mmu/bank  */
        /* once more modified with cpm3 and shift with curdisk->psh */
        if (cpm3)
        {
          #ifdef MULTIO
            memcpy_M_put_z(
                dmmu,			     /* use Destination-MMU	*/
                dma,			     /* destination DMA-Address	*/
                curdisk->data + offset,    /* position of source DATA	*/
                (128 << curdisk->psh)	     /* sector size		*/
                * sec_cnt	     /* Count of sectors */
            );
          #else
            memcpy_M_put_z(
                dmmu,			     /* use Destination-MMU	*/
                dma,			     /* destination DMA-Address	*/
                curdisk->data + offset,    /* position of source DATA	*/
                (128 << curdisk->psh)	     /* sector size		*/
            );
          #endif
        }
        else
        {
            memcpy_M_put_z(
                dmmu,				/* use Destination-MMU	*/
                dma,				/* DMA-Address		*/
                curdisk->data + offset,	/* position of DATA	*/
                128				/* sector size		*/
            );
        }
        /*
        	printf("YAZE: readsec dbnk= %d, DMA=%4X, track=%d, sec=%d\r\n",dbnk,
        							dma,track,sector);
        */
        return 0;
    }

    /* handle a 'unix directory' disc */

    blockno = offset / BLOCK_SIZE;

    if (blockno < 16)
    {
        /* directory access */
        if (offset / 32 < curdisk->nde)
            /* memcpy(ram+dma, curdisk->data+offset, 128); */
            /* memcpy_put_z(dma, curdisk->data+offset, 128); */
            memcpy_M_put_z(dmmu, dma, curdisk->data + offset, 128);
        else
            memset_M_z(dmmu, dma, 0xe5, 128);
    }
    else
    {
        /* file access */
        int i;
        for (i = 0; i < FCACHE_SIZE; i++)
        {
            struct fc *f = fcache + i;
            if ((f->f == NULL) ||
                    (f->disk == curdisk && blockno >= f->fd->firstblock &&
                     blockno <= f->fd->lastblock))
                break;
        }
        if (i == FCACHE_SIZE)
        {
            /* cache overflow */
            fclose(fcache[FCACHE_SIZE - 1].f);
            memmove(fcache + 1, fcache, (FCACHE_SIZE - 1) * sizeof(struct fc));
            i = 0;
            fcache[0].f = NULL;
        }
        if (fcache[i].f == NULL)
        {
            struct fc *f = fcache + i;
            struct fdesc *fd = curdisk->fds;
            int j;
            f->disk = curdisk;
            for (j = 0; j < curdisk->nfds; j++, fd++)
                if (blockno >= fd->firstblock && blockno <= fd->lastblock)
                    break;
            if (j >= curdisk->nfds)
            {
                /* cant happen */
                return 1;
            }
            if ((f->f = fopen(fd->fullname, "rb")) == NULL)
            {
                perror(fd->fullname);
                return 1;
            }
            f->fd = fd;
        }
        if (i != 0)
        {
            /* sort least-recently-used to front */
            struct fc temp;
            temp = fcache[i];
            /* orig of fdc: memmove(fcache+i, fcache, i * sizeof(struct fc));*/
            /*                             ^-- that's the error !!!	    */
            /* it moves over the end of fcache and causes a segmentation   */
            /* fault */
            memmove(fcache + 1, fcache, i * sizeof(struct fc)); /*changed by agl*/
            /*             ^-- it must be 1, that works, 20.12.2003 */
            fcache[0] = temp;
        }
        if (fseek(fcache[0].f, offset - fcache[0].fd->firstblock * BLOCK_SIZE,
                  SEEK_SET) < 0)
            return 1;
        /*
            modified by agl
        	if ((i = fread(ram+dma, 1, 128, fcache[0].f)) < 128)
        	    memset_z(dma+i, 0x1a, 128-i); (* EOF *) <-- ACHTUNG !!!
        */

        if ((i = fread(buf, 1, 128, fcache[0].f)) < 128)
        {
            int l_dma = dma + i;
            memset_M_z(dmmu, l_dma, 0x1a, 128 - i); /* EOF */
        }
        memcpy_M_put_z(dmmu, dma, buf, i);
    }
    return 0;
}

static int
#ifdef MULTIO
writesec(int stype, int sec_cnt)	/* sec_cnt is only used by CP/M 3.1 */
#else
writesec(int stype)
#endif
{
#ifdef MMU
    FASTREG tmp2;
#endif

    if (curdisk == NULL ||
            ((curdisk->flags & (MNT_ACTIVE | MNT_RDONLY)) != MNT_ACTIVE))
        return 1;
    /*  memcpy(curdisk->data +
        (track*GetWORD(curdisk->dph+16) + sector)*128, ram + dma, 128);
    */
    /*  once more changed by agl
        memcpy_get_z(curdisk->data + (track*GetWORD(curdisk->dph+16)+sector)*128,
    	 dma, 128);
    */
    if (cpm3)
    {
#ifdef MULTIO
        memcpy_M_get_z(dmmu,
                       curdisk->data + (track * GetWORD(curdisk->dpb)
                                        + (sector << curdisk->psh)) * 128,
                       dma,
                       (128 << curdisk->psh) * sec_cnt);
#else
        memcpy_M_get_z(dmmu,
                       curdisk->data + (track * GetWORD(curdisk->dpb)
                                        + (sector << curdisk->psh)) * 128,
                       dma,
                       (128 << curdisk->psh));
#endif
    }
    else
        /**********************
            memcpy_M_get_z(dmmu,
        	curdisk->data + (track*GetWORD(curdisk->dph+16) + sector)*128,
        	dma,
        	128);
        ***********************/
        memcpy_M_get_z(dmmu,
                       curdisk->data + (track * GetWORD(curdisk->dpb) + sector) * 128,
                       dma,
                       128);
    return 0;
}

/*  This only works for single-byte sector numbers
    (but then, so does the CP/M 2.2 CBIOS code).
*/
static WORD
sectrans(WORD sec, WORD table)
{
    /*  ACHTUNG: Bei CP/M 3 darf die Sektortranslation-
        Table in Bank 0 im Systembereich sein, da vom
        BDOS aus Bank 0 selektiert wird (siehe Seite 68
        Bios-Handbuch) !!! */
    if (table == 0)
        return sec;
    /***
        printf("sectrans: sec: %2d, table %4X, tranlated sec: %2X\r\n",
    		sec, table, GetBYTE(table+sec) );
    ***/
    return GetBYTE(table + sec);
}


void
setup_cpm3_dph_dpb(int disk)
{
    struct mnt *dp = mnttab + disk;
    WORD   w, ww;
    BYTE   version = dp->buf[16]; /* get version indentifier */
#ifdef MMU
    FASTREG tmp2;
#endif

    dp->dph = GetWORD(yct + 2 * disk); /* get pointer to dph from YCT */

    dp->dpb = GetWORD(dp->dph + DPH_dpb);	/* get DPB of drive */

    if (dp->xlt)	/* xlt gesetzt (#0) */
        dp->xlt = t_sssd;
    else
        dp->xlt = 0;
    PutWORD(dp->dph + DPH_xlt, dp->xlt);

    PutBYTE(dp->dph + DPH_mf, 0xff);	/* set Media Flag in DPH */
    PutBYTE(scb + SCB_MEDIA, 0xff);	/* set @MEDIA in SCB */

    w = GetWORD(dp->dpb + DPB_cks);	/* get DPB.cks		*/
    ww = w & 0x8000;		/* mask MSB		*/

    if (version == 0 || always128)	/* if diskfile is created by */
    {
        /* yaze-1.10/1.06 or flag -1 */
        dp->buf[32 + 15] = 0;	/* setup PSH and PHM for */
        dp->buf[32 + 16] = 0;	/* 128 byte sectors	 */
    }

    /* if version = 1 nothing is to do and the whole DPB will be copied */

    memcpy_put_z(dp->dpb, dp->buf + 32, 17);
    /* copy dpb from buf+32 to dp->dpb */
    /*  The first 128 Byte are read into buf from
        the disk-file when a drive(file) is mounted.
    */

    if (w & 0x7fff)   /* if CKS is set  */
    {
        /* calculate and set CKS */
        w = GetWORD(dp->dpb + DPB_drm);	/* get DRM (dir entries)     */
        w = (w >> 2);			/* calculate CKS (DRM/4)     */
        if (w) ++w;			/* only if # 0 ((DRM/4)+1)   */
        w |= ww;			/* set MSB if set in DPB.cks */
        PutWORD(dp->dpb + DPB_cks, w);	/* set CKS		     */
    }

    dp->bls = (128 << GetBYTE(dp->dpb + DPB_bsh));  /* calculate BLS */
    dp->dsm = GetWORD(dp->dpb + DPB_dsm);
    dp->drm = GetWORD(dp->dpb + DPB_drm);

    dp->psh = GetBYTE(dp->dpb + DPB_psh); /* get PSH */

    if (!(dp->flags & MNT_UNIXDIR))    /* only if NOT UNIXDIR */
    {
        /* calculate memory requirement */
        /* (((DSM+1)<<BSH) + OFFS*SPT + 1)*128 */
        dp->isize = (((GetWORD(dp->dpb + DPB_dsm) + 1) << GetBYTE(dp->dpb + DPB_bsh))
                     + GetWORD(dp->dpb + DPB_off) * GetWORD(dp->dpb + DPB_spt) + 1)
                    * 128;
    }
}


/*	 date function:

	dayfaktor calculates the theoretical days since 1.1.0000
	(Gregorian calendar)

	You'll find on drive M: (Turbo-Modula-2) the source in Modula-2.
	(Look for DAYS.MOD/DEF and BIRTHDAY.MOD)

	I extracted the formula from a TI-58 pocket calculator!

	calculate days between two dates: dayfaktor(date2) - dayfaktor(date1)
	weekdays: dayfaktor(date) mod 7 --> 0 Saturday ... 6 Friday
*/

/*  The original date function
    unsigned long
    dayfaktor( unsigned long day, unsigned long month, unsigned long year)
    { unsigned long m3, y3;

    if (month > 2L) {
	m3 = (unsigned long)(0.4 * (float)month + 2.3);
	y3 = year;
    } else {
	m3 = 0L;
	y3 = year - 1L;
    }

    return( 365L * year + day + 31L*(month-1L) - m3
	    + (unsigned long)(y3 / 4)
	    - (unsigned long)((float)((unsigned long)(year / 100) + 1L) * 0.75)
	  );
    }
*/

unsigned long
dayfaktor(unsigned long day, unsigned long month, unsigned long year)
{
    unsigned long m3, y3;

    if (month > 2L)
    {
        m3 = (unsigned long)(0.4 * (float)month + 2.3);
        y3 = year;
    }
    else
    {
        m3 = 0L;
        y3 = year - 1L;
    }

    return (365L * year + day + 31L * (month - 1L) - m3
            + (y3 >> 2)
            - (unsigned long)(((unsigned long)(year / 100L) + 1L) * 3L / 4L)
           );
}

static char cpmCommandLine[CPM_COMMAND_LINE_LENGTH];

static void getCPMCommandLine(void)
{
    int
    s, d,
    len = GetBYTE(0x80) & 0x7f; /* Get length of filepath at 0080H */
    /* Drop any leading whitespace */
    for (s = d = 0; s < len && isspace(GetBYTE(0x81 + s)); ++s);
    while (s < len)
    {
        cpmCommandLine[d++] = GetBYTE(0x81 + s);
        ++s;
    }
    cpmCommandLine[d] = 0; /* make C string */
    /* printf("ufilepath:%s:\r\n",ufilepath); */
}


static BYTE savcpm[CPM_LENGTH];		/* saved copy of cpm */

#ifdef MMU
#define CCP3_LENGTH 0xC80
#define CCP3_BASE   0x100
#define SYSBNK      0		/* MMU of SYS Bank */
#define TPABNK      1		/* MMU of TPA Bank */
#endif

extern BYTE conin();
extern struct _ci ci;

void bios(int func)
{

    static int cold_boot = 1;

    /* for read/write files from/to host system */
    static FILE *uf; /* unix file */
    static int d, i;
    static glob_t globS;
    static int globPosNameList       = 0;
    static int globPosName           = 0;
    static int globError             = 0;

#ifdef MMU
    static pagetab_struct *tpammu = &MMUtable[TPABNK]; /* TPA MMU-pagetable */
    static struct mnt *dp;
    static int xmove_op = 0;
    static WORD h_dtbl;
    static int h_mmut;
    static struct tm *t;
    static time_t now;
    static FASTREG tmp2;
#endif
#ifdef MULTIO
    static int mcnt = 0;		/* must be 0, very important !!! */
    static int mio_rw_done = 0;	/* boolean */
#endif

#ifdef UCSD
    static int ucsdmode = 0;	/* must be 0 */
#endif
    BYTE ch;

    ++bioscount;
    switch (func)
    {
    case 0:			/* cold boot (only used under CP/M 2.2) */
        if (cold_boot)
        {
            cold_boot = 0;	/* only one time! */
            memcpy_get_z(savcpm, ccp_base, CPM_LENGTH);
            /* PutBYTE(0x0F, 0xFF);	\* only for Test */
#ifdef DEBUG
            puts("DEBUG: Cold Boot\r");
#endif
        }
    /*
    	else
    	    printf("Cold Boot (jmp bios_base (%4X)) --> make a warm "
    					"boot!\n\r",bios_base);
    */
    case 1:			/* warm boot */
wboot:
        memcpy_put_z(ccp_base, savcpm, CPM_LENGTH);
        dma = 0x80;
        PutBYTE(0, 0xc3);		/* ram[0] = 0xc3; */
        PutBYTE(1, 0x03);		/* ram[1] = 0x03; */
        PutBYTE(2, hreg(bios_base));	/* ram[2] = hreg(bios_base);  */
        PutBYTE(5, 0xc3);		/* ram[5] = 0xc3;  */
        PutBYTE(6, 0x06);		/* ram[6] = 0x06; */
        PutBYTE(7, hreg(bdos_base));	/* ram[7] = hreg(bdos_base); */
        Setlreg(BC, GetBYTE(4));	/* Setlreg(BC, ram[4]); */
        pc = ccp_base;
#ifdef DEBUG
        puts("DEBUG: Warm Boot\r");
#endif
#ifdef UCSD
        ucsdmode = 0; /* switch of UCSD-Mode */
#endif
        return;

    case 2:			/* console status */
        Sethreg(AF, ci.size || constat() ? 0xff : 0x00);
        break;

    case 3:			/* console input */
        ch = conin();
        Sethreg(AF, ch);
        break;

    case 4:			/* console output */
        BGiiFix(lreg(BC));
        /* serout(CHNconout, (lreg(BC) & 0x7f) ); */
        serout(CHNconout, lreg(BC));
        break;

    case 5:			/* list output */
        serout(CHNlst, lreg(BC));
        break;

    case 6:			/* punch output */
        if (cpm3)
            serout(CHNaux, lreg(BC));	/* CP/M 3: AuxOut */
        else
            serout(CHNpun, lreg(BC));	/* CP/M 2.2 Punch */
        break;

    case 7:			/* tape reader input */
        if (cpm3)
            Sethreg(AF, serin(CHNaux));	/* CP/M 3 */
        else
            Sethreg(AF, serin(CHNrdr));	/* CP/M 2.2 */
        break;

    case 8:			/* home disk */
        track = 0;
        break;

    case 9:			/* select disk */
        /* printf("\n\rSELDSK: BC=%04X DE=%04X\n\r",BC,DE); */
        HL = seldsk(lreg(BC));
        break;

    case 10:			/* set track */
#ifndef UCSD
        track = BC;
#else
        if (!ucsdmode)
            track = BC;
        else
            track = lreg(BC);
#endif
        break;
    case 11:			/* set sector */
#ifndef UCSD
        sector = BC;
#else
        if (!ucsdmode)
            sector = BC;
        else
        {
            sector = lreg(BC);
            --sector    /* decrement one because the first sector for the
			   UCSD P-System is sector one and not zero, like the
			   definitions of the IBM 8" disk */
        }
#endif
        break;
    case 12:			/* set dma */
        dma = BC;
        break;
    case 13:			/* read sector */
        /* printf("YAZE: READ into bank %d\r\n",dbnk); */
#ifdef MULTIO
        if (mcnt)
        {
            if (mio_rw_done)
            {
                --mcnt;
              #ifdef RWDEBUG
                printf(".%d",mcnt);
                fflush(stdout);  /***/
              #endif
                Sethreg(AF, 0);	/* OK for read */
                break;
            }
            else
            {
                Sethreg(AF, readsec(mcnt--));
              #ifdef RWDEBUG
                printf("r");
                fflush(stdout); /***/
              #endif
                mio_rw_done = 1;
                break;
            }
        }
        else
        {
            Sethreg(AF, readsec(1));

            #ifdef RWDEBUG
            printf("R");
            fflush(stdout); /***/
            #endif
        }
#else /* of MULTIO */
        Sethreg(AF, readsec());
        #ifdef RWDEBUG
        printf("R");
        fflush(stdout); /***/
        #endif
#endif
        break;

    case 14:			/* write sector */
#ifdef MULTIO
        if (mcnt)
        {
            if (mio_rw_done)
            {
                mcnt--;
#ifdef RWDEBUG
                printf(":");
                fflush(stdout);  /***/
#endif
                Sethreg(AF, 0);	/* OK for write */
                break;
            }
            else
            {
                Sethreg(AF, writesec(lreg(BC), mcnt--));
#ifdef RWDEBUG
                printf("w");
                fflush(stdout); /***/
#endif
                mio_rw_done = 1;
                break;
            }
        }
        else
        {
            Sethreg(AF, writesec(lreg(BC), 1));
#ifdef RWDEBUG
            printf("W");
            fflush(stdout); /***/
#endif
        }
#else
        Sethreg(AF, writesec(lreg(BC)));
#ifdef RWDEBUG
        printf("W");
        fflush(stdout); /***/
#endif
#endif
        break;

    case 15:			/* list status */
        Sethreg(AF, lststat() ? 0xff : 0x00);
        break;
    case 16:			/* translate sector */
        HL = sectrans(BC, DE);
        break;


#ifdef MMU

    /* CP/M 3.1 functions: */

    case 17:			/* console output status */
        Sethreg(AF, 0xff);	/* always ready */
        break;
    case 18:			/* aux input status */
        Sethreg(AF, auxistat() ? 0xff : 0x00);
        /* Sethreg(AF, 0xff);	// always ready */
        break;
    case 19:			/* aux output status */
        Sethreg(AF, auxostat() ? 0xff : 0x00);
        /* Sethreg(AF, 0xff);	// always ready */
        break;
    case 20:			/* device table */
	printf("______________________________________________________");
	printf("_____________________\r\n\n");
	printf("You can attach devices only in the monitor of YAZE-AG!\r\n");
	printf("You go into the monitor with the SYS.COM utility without ");
	printf("any parameter!\r\n");
	printf("Give there the command \"help attach\"!\r\n\n");
	printf("Example 1: \"attach aux /dev/ttyUSB0\" if you want to connect");
	printf(" the AUX: device\r\n");
	printf("	   of CP/M 3 to a serial line which is connected to ");
	printf("an USB port.\r\n\n");
	printf("Example 2: \"attach lpt test.lpt\" if you want to connect the");
	printf(" LPT: \r\n	   printer device to a UNIX/Linux file.\r\n");
	printf("	   Any printer output goes into");
	printf(" the file \"test.lpt\".\r\n\n");
	printf("You go back to CP/M with the monitor command \"go\".\r\n");
	printf("______________________________________________________");
	printf("_____________________\r\n\n");
	printf("CP/M 3 reports:\r\n");
	HL = 0x0000;
	break;			/* The Instruction there must be a LD HL,@dtbl
				   and a RET. Have a look at Page 53 of the
				   System Guide. Function 22: DEVTBL.
				   HL = 0 If you don't want to use DEVTBL. */

    case 21:			/* Initialize Character I/O Device */
	break;

    case 22:			/* Return address of Disk Drive Table */
        /* defined in BIOSKRNL.Z80 */  /* (HL = *(yct-2)); */
        /* first entry of the YCT */
        break;			/* In the implementation of the bnkbios.z80 the
				   address of the @dtbl will be given directly
				   back there. This is nessessary because
				   GENCPM.COM uses this table for CP/M 3 -
				   generation */
    case 23:			/* Multio */
        /* Reg C is the multisector count */
#if MULTIO
        if (curdisk->flags & MNT_UNIXDIR)
            break;		/* Wenn eine Unix Directory gemountet ist */
        if (curdisk->xlt)	/* is set translation table ? */
            break;		/* yes --> go back (no multi i/o) */
        mcnt = lreg(BC);
        mio_rw_done = 0;	/* false: not yet read */
#endif

#ifdef RWDEBUG
        printf("YAZE: Multi=%d\r\n", lreg(BC));
#endif

        break;
    case 24:			/* Flush */
        Sethreg(AF, 0x00);	/* A=00 if no error occurred */
        /* A=01 if physical error occurred */
        /* A=02 if disk is Read-Only */
        break;
    case 25:			/* MOVE */
        /* ------------------implemented in the bioskrnl.z80 (uses LDIR) */
        /* ENTRY:   HL=Destination address, DE=Source address, BC=Count */
        /* RETURN: HL and DE must point to next bytes following move operation*/

        /*
        	if (xmove_op == 0) {
        		printf("YAZE: Move BC(Count)=%4X HL(Dest)=%4X DE(Source)=%4X"
        			" Bank %d %d %d\r\n",BC,HL,DE,mmutab,bnkdest,bnksrc);
        	} else {
        		printf("YAZE: IB-Move BC(Count)=%4X HL(Dest)=%4X DE(Source)=%4X"
        		" Cbnk %d DBnk %d SBnk %d\r\n",BC,HL,DE,mmutab,bnkdest,bnksrc);
        	}
        */

        while (BC--)
        {
            FASTREG byte = MRAM_pp(mmuget, DE);

            MRAM_pp(mmuput, HL) = byte;
        }

        xmove_op = 0;	/* xmove operation abgeschlossen, fuer den Fall es
			   wurde eine iniziiert. MOVE wird immer direkt nach
			   xmove aufgerufen. See Page 66 System Guide. */
        break;

    case 26:			/* Get and Set Time */

#define SCB_DATE (0x58)	/* position of @DATE in SCB        */
#define SCB_HOUR (0x5A)	/* position of @HOUR in SCB (BCD) */
#define SCB_MIN  (0x5B)	/* position of @MIN in SCB (BCD) */
#define SCB_SEC  (0x5C)	/* position of @SEC in SCB (BCD)*/

        time(&now);
        t = localtime(&now);
        PutBYTE(scb + SCB_SEC, (((t->tm_sec / 10) << 4) | (t->tm_sec % 10)));
        PutBYTE(scb + SCB_MIN, (((t->tm_min / 10) << 4) | (t->tm_min % 10)));
        PutBYTE(scb + SCB_HOUR, (((t->tm_hour / 10) << 4) | (t->tm_hour % 10)));
        /*  the old one
        	{ register int i,days;
        	  for (days=0,i=1978; i < (1900 + t->tm_year); i++)
        	  {
        		days += 365;
        		if (i % 4 == 0) days++;
        	  }
        	  days += t->tm_yday + 1;
        	  PutWORD(scb + SCB_DATE, days);
        	}
        */
        /*	 {
        	  register int days;
        	  days = dayfaktor( t->tm_mday, (t->tm_mon + 1), (1900 + t->tm_year) )
        		 - DayFaktor_CPMSTART;
        	  PutWORD(scb + SCB_DATE, days);
        	}
        */
        PutWORD(scb + SCB_DATE, (WORD)(dayfaktor(t->tm_mday,
                                       (t->tm_mon + 1),
                                       (1900 + t->tm_year)
                                                ) - DayFaktor_CPMSTART)
               );
        break;

    case 27:			/* SELMEM: Select Memory Bank */
        /* see bioskrnl.z80 and func 0xF0 */
        if (xmove_op == 0)
        {
            mmuget = mmuput = ChooseMMUtab(bnkdest = bnksrc = cbnk = hreg(AF));
            /* printf("YAZE: SELMEM: bnkdest = bnksrc = %d\r\n",cbnk); */
        }
        else
        {
            ChooseMMUtab(cbnk = hreg(AF));
            /* printf("YAZE: SELMEM: cbnk %d (xmove_op!)\r\n",cbnk); */
        }
        break;

    case 28:			/* SETBNK: Specify bank for DISK DMA operation*/
        dmmu = &MMUtable[dbnk = hreg(AF)];
        /* printf("YAZE: SETBNK A=%d\r\n",dbnk); */
        break;
    case 29:			 /* XMOVE: Set banks for following MOVE */
        /* B=destination bank, C=source bank   */
        mmuput = &MMUtable[ bnkdest = hreg(BC) ];
        mmuget = &MMUtable[ bnksrc  = lreg(BC) ];
        xmove_op = 1;
        /* printf("YAZE: XMOVE B=%d C=%d\n\r",bnkdest,bnksrc); */
        break;
    case 30:			/* USERF */
        break;
    case 31:			/* Reserv1  Reseved for Future Use */
        break;
    case 32:			/* RESERV2 */
        break;

#endif

    /* ------------------------------------------------------------------ */
    /* special functions for comunication between cp/m 3 and Unix: ------ */

    case 0xA0:			/* Version of YAZE-AG */
        HL = 256 * MAINVersion + SUBVersion;
        /* printf("HL = %04X \r\n",HL); */
        break;

    case 0xA1:			/* HostOSPathSeparator */
        HL = '/';   /* slash in UNIX and also in Windows (with Cygwin) */
        break;

    case 0xA2:			/* timexprint funktion is in monitor.c */
	timexprint();
	break;

    case 0xA8:		/* Report/load keyboard translation */
	HL = 0;
        getCPMCommandLine();
        if (strcmp(cpmCommandLine, "-") == 0)
            ktt_load("-");
        else if (strlen(cpmCommandLine))
        {
            struct stat
                    fdata;
            if (stat(cpmCommandLine, &fdata))
        {
                char
                *original = newstr(cpmCommandLine);
                strcat(cpmCommandLine, ".ktt");
                if (stat(cpmCommandLine, &fdata))
                {
                    HL = 1;
                    perror(original);
                    putchar('\r');
                    fflush(stdout);
                    perror(cpmCommandLine);
                }
                free(original);
            }
            if (HL == 0)
                ktt_load(cpmCommandLine);
        }
        printf("\r\nK-T file: %s\r\nElements: %d\r\n",
               ktt_name() ? ktt_name() : "<none>",
               ktt_elements());
        fflush(stdout);
        break;

    /* ------------------------------------------------------------------ */
    /* function for init the windows size handler ----------------------- */

    case 0xB0:
	winsize_updated =  false; /* reinstall the win size haendler */
	/* printf("winsize_updated: Set to false.\r\n"); */
	break;

    /* ------------------------------------------------------------------ */
    /* special functions for comunication between cp/m 3 and Unix: ------ */
    /* write CP/M files to a Unix directory ----------------------------- */

    case 0xD0:			/* open file (with path) write */
        /* printf("openUFileWrite\r\n"); fflush(stdout); */
        HL = 0; /* default no error */
        getCPMCommandLine();
        if ((uf = fopen(cpmCommandLine, "w")) == NULL)
        {
            perror(cpmCommandLine);
            HL = 1;
        }
        d = 0;
        break;

    case 0xD1:			/* sendbyte (write)*/
        /* putchar('.'); fflush(stdout); */
        putc(lreg(DE), uf);
        d++;
        break;

    case 0xD2:			/* close unix file */
        printf("closeFile, transfered bytes: %d\r\n", d);
        fflush(stdout);
        HL = 0;
        if (ferror(uf) || fclose(uf) != 0)
        {
            perror(cpmCommandLine);
            HL = 1;
        }
        break;

    case 0xD3:			/* open file (with path) read */
        /* printf("openUFileRead\r\n"); fflush(stdout); */
        HL = 0; /* default no error */
        getCPMCommandLine();
        if ((uf = fopen(cpmCommandLine, "r")) == NULL)
        {
            perror(cpmCommandLine);
            HL = 1;
        }
        /* printf("\rrufilepath:%s:\r\n",cpmCommandLine); fflush(stdout); */
        d = 0;
        break;

    case 0xD4:		/* getc_and_status */
        i = getc(uf);
        if (i == EOF)
            HL = 0x011A;
        else
        {
            Setlreg(HL, (BYTE) i);
            d++;
        }
        break;

    case 0xD5:		/* globHostFilenames */
        HL = 0; /* default no error */
        globPosNameList = globPosName = 0;
        getCPMCommandLine();
        printf("Host: search pattern \"%s\"\r\n", cpmCommandLine);
        fflush(stdout);
        globError = glob(cpmCommandLine, GLOB_ERR, NULL, &globS);
        if (globError)
        {
            switch (globError)
            {
            case GLOB_NOSPACE:
                printf("Host: no memory space\r\n");
                break;
            case GLOB_ABORTED:
                printf("Host: read error\r\n");
                break;
            case GLOB_NOMATCH:
                printf("Host: no matches\r\n");
                break;
            }
            fflush(stdout);
            globfree(&globS);
            HL = 1;  /* error or no matches */
        }
        break;

    case 0xD6:		/* getHostFilename */
        if (globPosNameList < globS.gl_pathc)
        {
            if (!(i = globS.gl_pathv[globPosNameList][globPosName++]))
            {
                globPosNameList++;
                globPosName = 0;
            }
        }
        else
        {
            globfree(&globS);
            i = 0;
        }
        HL = i;
        break;

#ifdef MMU

    /* ------------------------------------------------------------------*/
    /* special functions for comunication between cp/m 3 and yaze: ------*/


    case 0xE0:   /* 224 = INIT CP/M 3.1 */

        cpm3 = 1;			/* NOW CP/M 3.1 is running ! */

        yct = HL;			/* Z80-Pointer to the @YCT, usage */
        /* with GetWORD/GetBYTE */
        dtbl = GetWORD(yct - 2);	     /* address of @dtbl     */
        scb = GetWORD(yct - 4);	    /* address of SCB       */
        bios_base = GetWORD(yct - 6); /* address of the BIOS  */
        t_sssd = GetWORD(yct - 8);  /* address of translation table SSSD */
        maxdsm = GetWORD(yct - 10); /* maximal DSM */
        maxdrm = GetWORD(yct - 12); /* maximal DRM */

        bdos_base = GetWORD(scb + SCB_MXTPA);
        ccp_base = CCP3_BASE;

        /*  printf("CP/M 3 init: yct=%X, PC=%X, cbnk=%d, SP=%X\n",
        						yct, pc, cbnk, sp);
        */

#ifdef SHOWDRV
        printf("\nYAZE: 0xE0 - function:\n\n\rHL = %X (YCT)\n\r", HL);
        /* printf("@dtbl from YCT : %X\n\r", (*(yct-2) + (*(yct-1) << 8)) ); */
        printf("@dtbl from YCT : %X\n\r", dtbl);
        printf("@bnkbf (^BC) %X\r\n", GetWORD(BC));
        printf("SCB from DE : %X\n\r", DE);
        printf("SCB from yct : %X\n\r", scb);
        printf("dph0 from YCT : %X\n\n\r", GetWORD(yct));
        printf("bios_base : %X\r\n", bios_base);
        printf("bdos_base : %X\r\n", bdos_base);
        printf("ccp_base  : %X\r\n\n", ccp_base);
        printf("---------------------------------\n\r");
#else
        printf("\r\n DRIVES: ");
#endif
        for (d = 0; d < 16; d++)
        {
            dp = mnttab + d;
            h_dtbl = dtbl + (2 * d);
            if (dp->flags & MNT_ACTIVE)
            {
                setup_cpm3_dph_dpb(d);
                PutWORD(h_dtbl, dp->dph);/* put pointer to dph in @dtbl */
                /* now the drive is for CP/M 3 present*/
            }
            else
            {
                PutWORD(h_dtbl, 0); /* delete pointer in @dtbl */
            }

#ifdef SHOWDRV
            showdisk(d, 1);
#else
            if (dp->flags & MNT_ACTIVE)
                printf(dp->flags & MNT_UNIXDIR ? " %c/" : " %c", ('A' + d));
            else
                printf(" .");
#endif
        }

#ifdef SHOWDRV
        printf("---------------------------------\r\n");
#else
        printf("\r\n"); /* to print DRIVES*/
#endif

        break;

    case 0xE1:   /*225*/  /* save CP/M 3 CCP */
        memcpy_M_get_z(tpammu, savcpm, CCP3_BASE, CCP3_LENGTH);
        break;

    case 0xE2:   /*226*/  /* put CP/M 3 CCP into TPA (bank 1) and start CCP */
        /* selected Bank must be TPA */

        mmuget = mmuput = ChooseMMUtab(TPABNK);

        memcpy_put_z(CCP3_BASE, savcpm, CCP3_LENGTH);

        PutBYTE(0, 0xc3);		/* ram[0] = 0xc3; */
        PutBYTE(1, 0x03);		/* ram[1] = 0x03; */
        PutBYTE(2, hreg(bios_base));	/* ram[2] = hreg(bios_base);  */
        PutBYTE(3, 0x95);		/* standart I/O-Byte */
        PutBYTE(5, 0xc3);		/* ram[5] = 0xc3;   */
        /* PutBYTE(6, 0x06);		\* ram[6] = 0x06;  */
        /* PutBYTE(7, hreg(bdos_base));	\* ram[7] = hreg(bdos_base); */

        bdos_base = GetWORD(scb + SCB_MXTPA); /* IMPORTANT !!! */
        /* ^^^^^^ changed at any warm boot !!! */

        PutWORD(6, bdos_base);		/* From SCB_MXTPA !!! */

        /*	 printf("\n\rYAZE: bios_base: %X, bdos_base: %X\r\n",
        						bios_base, bdos_base);
        ***/
        pc = CCP3_BASE;
        return; /* no break! */

    /* -------------------------------------------------------------------*/
    /* functions for the MMU: -------------------------------------------*/

    case 0xF0:			/* (240) MMU-Function: select MMU-Table */
        if ((h_mmut = hreg(AF)) < MMUTABLES)
        {
            /* hreg(par) made an (par & 0xff) --> only 0..255 possible */
            ChooseMMUtab(h_mmut);
        }
        else
        {
            Sethreg(AF, 0xff);	/* A=FFh <-- Error: Nr for MMU */
#ifdef MMUTEST
            puts("\r\nYAZE: Select MMU-table --> Number for selection "
                 "is out of range!");
#endif
        }
        break;
    case 0xF1:			/* (241) MMU: load MMU-table */
        /*  structure for load MMUtable in the z80-mem (HL is the pointer):
        	first-Byte: adr of MMUtable
        	2..16:	    16 bytes which will be translated to pointers
        		    and put in to the MMUTab.
            return-codes (reg A & HL):
        	A = 0xFE: PagePointer is wrong (out of Memory). HL points
        		  to the wrong PP.
        	A = 0xFF: MMUtable (first Byte) addresses an MMUtable which does
        		  not exist.
        */
        loadMMU();
        break;
    case 0xF2:			/* (242) MMU: print MMU  */
        printMMU();
        break;
    case 0xF3:			/* (243) MMU: give back	the No. of sel. MMU*/
        Sethreg(AF, mmutab);	/* A = selected MMU		*/
        break;			/* ... */
    case 0xF4:			/* (244) MMU: give back	the MMU-status */
        Sethreg(AF, mmutab);	/* A = selected MMU		*/
        BC = MMUTABLES * 256	/* B = MMUTABLES		*/
             + MMUPAGEPOINTERS;	/* C = MMUPAGEPOINTER		*/
        DE = RAMPAGES;		/* DE = No. of pages of the RAM	*/
        HL = MEMSIZE;		/* HL = size of memory		*/
        break;
    case 0xF5:			/* (245) MMU-Function: sel. MMU-Table + WBOOT */
        if ((h_mmut = hreg(AF)) < MMUTABLES)
        {
            /* hreg(par) made an (par & 0xff) --> only 0..255 possible */
            WORD o_iobyte = GetWORD(3);	  /* save old iobyte and drive */
            ChooseMMUtab(h_mmut);		 /* choose the MMU table      */
            dmmu = mmu;			/* set dmmu		     */
            PutWORD(3, o_iobyte);		/* set old iobyte and drive */
            printMMU();
            /*  puts("wboot after mmutsel ...\r"); */
            goto wboot;
        }
        else
        {
            Sethreg(AF, 0xff);	/* A=FFh <-- Error: Nr for MMU */
#ifdef MMUTEST
            puts("\r\nYAZE: Select MMU-table --> Number for selection "
                 "is out of range!");
#endif
        }
        break;
#else /* of MMU */
    case 17:	/* cp/m 3 functions */
    case 18:
    case 19:
    case 20:
    case 21:
    case 22:
    case 23:
    case 24:
    case 25:
    case 26:
    case 27:
    case 28:
    case 29:
    case 30:
    case 31:
    case 32:
    case 0xE0: /* special function for comunication between cp/m 3.1 and yaze */
    case 0xE1:
    case 0xE2:
    case 0xF0: /* functions for the MMU */
    case 0xF1:
    case 0xF2:
    case 0xF3:
    case 0xF4:
    case 0xF5:
        fprintf(stderr, "\r\n\nYAZE: Invalid bios function: %d (0x%X)\r\n\n",
                func, func);
        fprintf(stderr, "YAZE: You are trying use MMU or CP/M 3.1 functions.\r\n");
        fprintf(stderr, "YAZE: Please first compile YAZE-AG with the flag "
                "-DMMU to use it.\r\n\n");
        /* goto wboot; */
        exit(0);

#endif /* of MMU */

#ifdef UCSD
    case 250:   /* 0xFA: switch on ucsd-modus */
        ucsdmode = 1;
        break;
#endif
    /* case 253:			\* return from recursive call */
    /* return; */
    case 254:			/* meta-level command */
        ++pc; /*<-- mu\DF vor GetByte geschehen, da bei -DMMU & GetBYTE(++pc) das
		    GetBYTE-Macro aufgrund des zweimaligen Zugriffs auf "++pc"
		    den pc zweimal incementieren */
        if (GetBYTE(pc) == 0)
        {
#ifdef MMU
            if (cpm3) mmuget = mmuput = ChooseMMUtab(SYSBNK);
#endif
            monitor(0);
#ifdef MMU
            if (cpm3) mmuget = mmuput = ChooseMMUtab(TPABNK);
#endif
        }
        else
        {
            /* need a copy because docmd() scratches its argument */
            char *sav = newstr((char *) ram + pc);
#ifdef MMU
            if (cpm3) mmuget = mmuput = ChooseMMUtab(SYSBNK);
#endif
            ttycook();
            (void) docmd(sav);
            ttyraw();
            free(sav);
#ifdef MMU
            if (cpm3) mmuget = mmuput = ChooseMMUtab(TPABNK);
#endif
            pc += strlen((char *) ram + pc);
        } /* endif */
        break;
    case 255:			/* quit */
        exit(0);
    default:
        fprintf(stderr, "YAZE: Invalid bios function: %d (0x%X)\r\n", func, func);
        /* goto wboot; */
        exit(0);
    }
    pc++;
}
