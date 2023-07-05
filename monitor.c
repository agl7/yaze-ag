/*  System monitor.
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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#ifndef CYGWIN
#include <libgen.h>
#endif
#include <strings.h>
#include <time.h>
#include <limits.h>
#include <fcntl.h>
#include <termios.h>
#include <ctype.h>
#include <signal.h>
#include <time.h>
#include <sys/times.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <sys/mman.h>

#include "mem_mmu.h"
#include "simz80.h"
#include "yaze.h"
#include "ybios.h"

/* TTY management */

struct termios cookedtio, rawtio;
int ttyflags;
int interrupt;

void
ttyraw(void)
{
    if ((ttyflags & (ISATTY | ISRAW)) == ISATTY)
    {
        tcsetattr(fileno(stdin), TCSAFLUSH, &rawtio);
        ttyflags |= ISRAW;
    }
}

void
ttycook(void)
{
    if (ttyflags & ISRAW)
    {
        tcsetattr(fileno(stdin), TCSAFLUSH, &cookedtio);
        putc('\n', stdout);
        ttyflags &= ~ISRAW;
    }
}


/*  memory management routines for disk descriptors
    (we need to allocate chunks of cp/m ram for these) */

/* inefficient but robust bit-wise algorithm (we're not doing this all day) */
#define bytefree(x)	(!(global_alv[(x-bios_top) >> 3] &		\
			   (0x80 >> ((x-bios_top) & 7))))

static WORD
cpmalloc(WORD len)
{
    WORD p = bios_top;

    while (p < dptable - len)
        if (!bytefree(p))
            p++;
        else
        {
            int i;
            for (i = 1; i < len; i++)
                if (!bytefree(p + i))
                    break;
            if (i == len)
            {
                WORD p1 = p - bios_top;
                while (len--)
                {
                    global_alv[p1 >> 3] |= (0x80 >> (p1 & 7));
                    p1++;
                }
                return p;
            }
            p += i;
        }
    return 0;
}


static void
cpmfree(WORD adr, WORD len)
{
    WORD p = adr - bios_top;

    while (len--)
    {
        global_alv[p >> 3] &= ~(0x80 >> (p & 7));
        p++;
    }
}


/* Disk management */

/*  There are two kinds of simulated disk:
    a unix file containing an image of a cp/m disk and
    a unix directory which is made to look like a cp/m disk on the fly */

#define MNT_ACTIVE	1
#define MNT_RDONLY	2
#define MNT_UNIXDIR	4

struct mnt mnttab[16], *curdisk;      /* mount table (simulated drives A..P) */

/* display a mount table entry */
void
showdisk(int disk, int verbose)
{
    struct mnt *dp = mnttab + disk;
    BYTE   version = dp->buf[16]; /* get version indentifier */

    printf("%c: ", disk + 'A');
    if (!(dp->flags & MNT_ACTIVE))
    {
        puts("not mounted\r\n");
        return;
    }
    printf(dp->flags & MNT_UNIXDIR ? "%s %s/\r\n" : "%s %s\r\n",
           dp->flags & MNT_RDONLY ? "r/o " : "r/w ", dp->filename);
    if (!verbose)
        return;
    if (cpm3)
    {
        if (dp->flags & MNT_UNIXDIR)
            printf("\r\n  Drive connected to a directory!");
        else
        {
            printf("\r\n  Disk file created under ");
            switch (version)
            {
            case 0:
                printf("yaze 1.10/1.06 (version 0)");
                break;
            case 1:
                printf("yaze-ag 2.xx (version 1)");
                break;
            default:
                printf("- not known");
            }
        }
        printf("\r\n\n  CP/M 3.1 DPH (%04X)\r\n", dp->dph);
        printf("  xlt=%04X, mf=%02X, dpb=%04X,"
               " csv=%04X, alv=%04X, dirbcb=%04X\r\n",
               GetWORD(dp->dph), GetBYTE(dp->dph + 11), GetWORD(dp->dph + 12),
               GetWORD(dp->dph + 14), GetWORD(dp->dph + 16), GetWORD(dp->dph + 18));
        printf("  dtabcb=%04X, hash=%04X, hbank=%02X\r\n",
               GetWORD(dp->dph + 20), GetWORD(dp->dph + 22), GetBYTE(dp->dph + 24));
        printf("\r\n  CP/M 3.1 DPB (%04X)\r\n", dp->dpb);
        printf("  spt=%04X, bsh=%02X, blm=%02X, exm=%02X, dsm=%04X, drm=%04X,"
               " al=%02X%02X, cks=%04X,\r\n  off=%04X, psh=%02X,"
               " phm=%02X ",
               GetWORD(dp->dpb), GetBYTE(dp->dpb + 2), GetBYTE(dp->dpb + 3),
               GetBYTE(dp->dpb + 4), GetWORD(dp->dpb + 5), GetWORD(dp->dpb + 7),
               GetBYTE(dp->dpb + 9), GetBYTE(dp->dpb + 10), GetWORD(dp->dpb + 11),
               GetWORD(dp->dpb + 13), GetBYTE(dp->dpb + 15), GetBYTE(dp->dpb + 16));
        printf("(sektor size: %d)\r\n\n", (128 << GetBYTE(dp->dpb + 15)));

    }
    else
    {

        printf("  dph=%04X, xlt=%04X, dirbuf=%04X, dpb=%04X,"
               " csv=%04X, alv=%04X, spt=%04X\r\n",
               dp->dph, GetWORD(dp->dph), GetWORD(dp->dph + 8), GetWORD(dp->dph + 10),
               GetWORD(dp->dph + 12), GetWORD(dp->dph + 14), GetWORD(dp->dph + 16));
        printf("  bsh=%02X, blm=%02X, exm=%02X, dsm=%04X, drm=%04X,"
               " al=%02X%02X, cks=%04X, off=%04X\r\n",
               GetBYTE(dp->dph + 18), GetBYTE(dp->dph + 19), GetBYTE(dp->dph + 20),
               GetWORD(dp->dph + 21), GetWORD(dp->dph + 23),
               GetBYTE(dp->dph + 25), GetBYTE(dp->dph + 26),
               GetWORD(dp->dph + 27), GetWORD(dp->dph + 29));
    }
}

/* unmount a disk */
static int
umount(int disk)
{
    struct mnt *dp = mnttab + disk;
    /* WORD xlt; <- deletet by agl, is in dp */

    if (!(dp->flags & MNT_ACTIVE))
        return 0;
    if (dp->flags & MNT_UNIXDIR)
    {
        int i;
        clearfc(dp);			/* clear the bios's file cache */
        for (i = 0; i < dp->nfds; i++)
            free(dp->fds[i].fullname);
        free(dp->data);
        free(dp->fds);
    }
    else if (munmap(dp->header, dp->isize) == -1 ||
             close(dp->ifd) == -1)
        perror(dp->filename);
    dp->flags = 0;
    free(dp->filename);
    /*  modified by agl ...
        if ((xlt = GetWORD(dp->dph)) != 0)
    	cpmfree(xlt, GetWORD(dp->dph+16));
    */
    /* by agl */
    if ((dp->xlt = GetWORD(dp->dph)) != 0)
    {
        if (!cpm3) cpmfree(dp->xlt, GetWORD(dp->dph + 16));
        dp->xlt = 0;
    }
    if (cpm3)
        PutWORD((dtbl + (2 * disk)), 0);  /* delete entry in dtbl (CP/M 3.1) */
    else
        cpmfree(GetWORD(dp->dph + 14), (GetWORD(dp->dph + 16 + 5) >> 3) + 1);
    return 0;
}

/* stash a string away on the heap */
char *
newstr(const char *str)
{
    char *p = xmalloc(strlen(str) + 1);
    (void) strcpy(p, str);
    return p;
}

/* format of a cp/m 3.1 date stamp entry */
struct s_cpmdate
{
    WORD day;	/* count days since 1.1.1978 */
    BYTE hour;
    BYTE min;
    /*    BYTE sec; */
};

/* converts a time information of unix to a date information under cp/m 3.1 */
void
cpmdate(time_t *unixtime, struct s_cpmdate *cpmdat)
{
    struct tm *t;

    t = localtime(unixtime);

    cpmdat->day = (WORD)(dayfaktor(t->tm_mday,
                                   (t->tm_mon + 1),
                                   (1900 + t->tm_year)
                                  ) - DayFaktor_CPMSTART);

    cpmdat->hour = (BYTE)(((t->tm_hour / 10) << 4) | (t->tm_hour % 10));
    cpmdat->min  = (BYTE)(((t->tm_min / 10) << 4) | (t->tm_min % 10));
    /*    cpmdat->sec  = (BYTE)( ((t->tm_sec/10)<<4) | (t->tm_sec%10) ); */
}

/*  Decide if a unix file is eligible to be included as a cp/m file in a
    simulated disk constructed from a unix directory.  Return the filesize in
    bytes if so, 0 if not. */
static off_t
cpmsize(const char *dir_name, const char *filename, unsigned char *cpmname,
        char **unixname, struct s_cpmdate *acc_time, struct s_cpmdate *mod_time)
{
    int i, fd;
    const char *p = filename;
    unsigned char *p1 = cpmname;
    struct stat st;
    char *path;

    /* construct cpm filename, rejecting any that dont fit */
    for (i = 0; i < 8; i++)
    {
        if (*p == 0 || *p == '.')
            break;
        if (*p <= ' ' || *p >= '{' || strchr("=_:,;<>", *p))
            return 0;
        *p1++ = toupper(*p);
        p++;
    }
    for (; i < 8; i++)
        *p1++ = ' ';
    if (*p)
    {
        if (*p == '.')
            p++;
        else
            return 0;
    }
    for (; i < 11; i++)
    {
        if (*p == 0)
            break;
        if (*p <= ' ' || *p >= '{' || strchr(".=_:,;<>", *p))
            return 0;
        *p1++ = toupper(*p);
        p++;
    }
    if (*p)
        return 0;
    for (; i < 11; i++)
        *p1++ = ' ';

    /* construct unix filename */
    path = xmalloc(strlen(dir_name) + strlen(filename) + 2);
    sprintf(path, "%s/%s", dir_name, filename);

    /* check that file is readable, regular and non-empty */
    if ((fd = open(path, O_RDONLY)) < 0)
    {
        free(path);
        return 0;
    }
    if (fstat(fd, &st) < 0)
    {
        close(fd);
        free(path);
        return 0;
    }
    close(fd);
#ifdef __EXTENSIONS__
    if (((st.st_mode & S_IFMT) != S_IFREG) || st.st_size == 0)
    {
#else
    if (((st.st_mode & __S_IFMT) != __S_IFREG) || st.st_size == 0)
    {
#endif
        free(path);
        return 0;
    }

    cpmdate(&st.st_atime, acc_time);   /* convert access time in cp/m format */
    cpmdate(&st.st_mtime, mod_time);   /* modification time (will be update) */
    /* added by agl (26.1.2004 */
    *unixname = path;
    return st.st_size;
}

/* mount a unix directory as a simulated cpm disk */
/* expanded Jan 2004 by Andreas Gerlich to handle cp/m date stamps entries */
/* (It's a terrible hack ;-) */

static struct cpmlabel   /* record of a cp/m 3.1 label */
{
    char user;
    char labelname[8 + 3];
    char LB;		/* Label byte */
    char PB;		/* Used to decode label password */
    char RR1, RR2;	/* reserved */
    char Password[8];	/* password */
    /* Label create/access datestamp */
    BYTE cr_day_low;
    BYTE cr_day_high;
    BYTE cr_hour;
    BYTE cr_min;
    /* Label update datestamp */
    BYTE up_day_low;
    BYTE up_day_high;
    BYTE up_hour;
    BYTE up_min;
} stdlabel =
{
    0x20,
    "           ",
    0x21,		/* LB: update + label exists */
    0,			/* PB */
    0, 0,		/* RR */
    "\x00\x00\x00\x00\x00\x00\x00\x00", /* no password */
    0x31, 0x25, 0x22, 0x55,			/* 25.1.2004, 22:55 */
    0x31, 0x25, 0x22, 0x56			/* 25.1.2004, 22:56 */
};

/* LB - byte, modified by function dosetacc */
#define LBL_existsbit	0x01
#define LBL_createbit	0x10
#define LBL_updatebit	0x20
#define LBL_accessbit	0x40
unsigned int lbl = 0x21; /* default: exists & update */


struct s_datestamps  	/* record of one datestamp in the dates stamps */
{
    /* CP/M create/access time */   /* directory Entry (10 bytes) */
    BYTE cr_day_low;
    BYTE cr_day_high;
    BYTE cr_hour;
    BYTE cr_min;
    /* CP/M update time */
    BYTE up_day_low;
    BYTE up_day_high;
    BYTE up_hour;
    BYTE up_min;
    BYTE pm;	/* password mode */
    BYTE null;
};


static int
mountdir(struct mnt *dp, const char *filename, unsigned int labelb)
{
    DIR *dirp;
    struct dirent *direntp;
    unsigned char *cpmdir = xmalloc(N_ENTRIES * 32);
    unsigned long blockno = COVER((N_ENTRIES * 32), BLOCK_SIZE);
    int direno = 0;
    int direno2 = 0;
    int nfiles = 0;
    int i;
    WORD alv, w;
    static unsigned long serialno = 0;
    unsigned char *cp;	/* pointer to cp/m directory entry   */
    char *cp2;
    /* for time */
    time_t now;
    struct s_cpmdate cpm_now, acc_time, mod_time;
    struct cpmlabel *cpmlbl;
    unsigned char *datestamps;
    struct s_datestamps filedate;

    dp->fds = xmalloc(N_ENTRIES * sizeof(struct fdesc));
    if ((dirp = opendir(filename)) == NULL)
    {
        perror(filename);
        free(cpmdir);
        free(dp->fds);
        return 0;
    }
    memclr(cpmdir, (N_ENTRIES * 32));

    /* setup the first 4 directory entries (first 128 byte) */
    cp = cpmdir;
    *cp = 0xE5; /* E5H --> erased directory entry */
    cp += 32;
    *cp = 0xE5;
    cp += 32;
    *cp = 0xE5;
    cp += 32;
    *cp = 0x21; /* 21H -> date stamps directory entry */
    /* datestamps = cpmdir + 3*32 + 1; */
    datestamps = cp + 1;  /* pointer to the date stamp of the first file */

    /* Setup label entry */
    memcpy(cpmdir, &stdlabel, sizeof(stdlabel));
    cpmlbl = (struct cpmlabel *) cpmdir;
    /* points to the cp/m label entry (first entry in */
    /* the cp/m directory */
    /* copy name of directory into the labelname of the CP/M disk label */
#ifndef CYGWIN
    cp = cpmdir + 1;
    cp2 = basename((char *) filename);
    for (i = 0; *cp2 && i < 11 ; i++)
        *cp++ = *cp2++;
#endif
    /* cpmlbl->LB = labelb;	 \* Label byte */
    /*  LB wird nicht gesetzt (bleibt auf 0x21). Wenn das
        accessBit gesetzt ist versucht CP/M beim lesen einer File
        in die directory mit den access time informationen zu
        schreiben. Dies verursacht ein Fehler (da R/O):
        CP/M verschluckt sich dabei in der Weise, dass die
        betroffenen Dateien nicht
        mehr sichtbar sind (mit DIR) aber noch z.b. mit type
        geoeffnet werden koennen.
    */

    time(&now);		/* get time/date */
    cpmdate(&now, &cpm_now);   /* convert into cp/m 3.1 format */

    /* set create time in label */
    cpmlbl->cr_day_low  = cpm_now.day & 0x00ff; /* % 256; */
    cpmlbl->cr_day_high = cpm_now.day >> 8;    /* / 256; */
    cpmlbl->cr_hour = cpm_now.hour;
    cpmlbl->cr_min  = cpm_now.min;

    /* set update time in label */
    cpmlbl->up_day_low  = cpm_now.day & 0x00ff; /* % 256; */
    cpmlbl->up_day_high = cpm_now.day >> 8;    /* / 256; */
    cpmlbl->up_hour = cpm_now.hour;
    cpmlbl->up_min  = cpm_now.min;


    /* time stamps into the date stamps directory entry */

    memclr(&filedate, sizeof(struct s_datestamps));

    if (labelb & LBL_accessbit)
    {
        filedate.cr_day_low  = cpm_now.day & 0x00ff;	/* mod 256 */
        filedate.cr_day_high = cpm_now.day >> 8;	/* div 256 */
        filedate.cr_hour = cpm_now.hour;
        filedate.cr_min  = cpm_now.min;
    }

    filedate.up_day_low  = cpm_now.day & 0x00ff;  /* mod 256 */
    filedate.up_day_high = cpm_now.day >> 8;	  /* div 256 */
    filedate.up_hour = cpm_now.hour;
    filedate.up_min  = cpm_now.min;

    memcpy(datestamps, &filedate, sizeof(struct s_datestamps));

    direno++;

    while ((direntp = readdir(dirp)) != NULL)
    {
        char *fullname;
        off_t size;		/* file size in bytes */
        int ndirents;		/* number of directory entries required for file */
        int nlogexts;		/* number of full logical extents occupied by */
        /* file */
        unsigned long nblocks;	/* number of blocks occupied by file */

        if ((size = cpmsize(filename, direntp->d_name, cpmdir + 32 * direno + 1,
                            &fullname, &acc_time, &mod_time)) == 0)
            continue;
        for (i = 0; i < direno; i++)
            if (memcmp(cpmdir + 32 * i + 1, cpmdir + 32 * direno + 1, 11) == 0)
            {
                free(fullname);		/* discard case-collapsed duplicates */
                size = 0;		/* added by agl, 20.12.2003 */
                break;			/* added by agl */
                /* continue;		\* deleted by agl, this does not work */
            }
        if (size == 0)			/* added by agl, 20.12.2003 */
            continue;

        /* setup date stamp of the directory entry */
        /* adr of the related datestamp := datestamps + (direno % 4)*10; */

        memclr(&filedate, sizeof(struct s_datestamps));

        /* unix access time as create/access time under cp/m */
        if (labelb & LBL_accessbit)
        {
            filedate.cr_day_low  = acc_time.day & 0x0ff; /* mod 256 */
            filedate.cr_day_high = acc_time.day >> 8;	 /* div 256 */
            filedate.cr_hour = acc_time.hour;	/* time under cp/m 3.1 */
            filedate.cr_min  = acc_time.min;
        }

        /* unix modification time as update time under cp/m */
        filedate.up_day_low  = mod_time.day & 0x0ff;	/* mod 256 */
        filedate.up_day_high = mod_time.day >> 8;	/* div 256 */
        filedate.up_hour = mod_time.hour;	/* time under cp/m 3.1 */
        filedate.up_min  = mod_time.min;

        memcpy(datestamps + (direno % 4) * 10, &filedate,
               sizeof(struct s_datestamps));

        ndirents = COVER(size, 8 * BLOCK_SIZE);
        /* nlogexts = size/(16*1024);  orig changed at 2008-06-15 */
        nlogexts = (size - 1) / (16 * 1024);
        nblocks = COVER(size, BLOCK_SIZE);

        if ((direno + ndirents > (N_ENTRIES - 1)) ||
                ((blockno + nblocks) * BLOCK_SIZE > 0xffff * 128))
        {
            fprintf(stderr, "not all files in %s will fit on disk\n", filename);
            free(fullname);
            break;
        }
        dp->fds[nfiles].fullname = fullname;
        dp->fds[nfiles].firstblock = blockno;
        dp->fds[nfiles].lastblock = blockno + nblocks - 1;
        dp->fds[nfiles++].serial = serialno++;
        direno2 = direno;
        for (i = 0; i < ndirents; i++)
        {
            int ex = nlogexts < 2 * i + 1 ? nlogexts : 2 * i + 1;
            int j;
            cp = cpmdir + (direno2) * 32;
            *cp = 0;	/* user = 0 */
            if (i)
                memcpy(cp + 1, cpmdir + direno * 32 + 1, 11);
            cp[12] = ex & 0x1f;
            cp[13] = 0;
            cp[14] = ex >> 5;
            /*
                cp[15] = nblocks <= 8 ?
                COVER((size-16*1024*nlogexts), 128) : 128;
            */
            /* added the followng 2 lines 2008-06-02, AG */
            if ((size % 16384) == 0)
                cp[15] = 128;
            else
            {
                cp[15] = nblocks <= 8 ?
                         COVER((size - 16 * 1024 * nlogexts), 128) : 128;
            }
            cp += 16;
            for (j = 0; j < 8; j++)
            {
                if (nblocks > 0)
                {
                    *cp++ = blockno;
                    *cp++ = blockno >> 8;
                    ++blockno;
                    --nblocks;
                }
                else
                {
                    *cp++ = 0;
                    *cp++ = 0;
                }
            }
            direno2++;
            /* handle last dir entry (date stamps entry) */
            if ((direno2 % 4) == 3)   /* date stamps entry ? */
            {
                cp = cpmdir + (direno2) * 32;
                *cp = 0x21;
                if (++direno2 < N_ENTRIES)
                {
                    /* setup next sektor */
                    cp = cpmdir + (direno2) * 32;
                    *cp = 0xE5;
                    cp += 32;
                    *cp = 0xE5;
                    cp += 32;
                    *cp = 0xE5;
                    cp += 32;
                    *cp = 0x21; /* date stamps */
                    datestamps = cp + 1;
                }
            }
        }
        direno = direno2;
    }
    while ((direno % 4) != 0) direno++; /* direno begins at next sector */

    closedir(dirp);
    if (nfiles == 0)
    {
        fprintf(stderr, "no suitable files in %s\n", filename);
        free(cpmdir);
        free(dp->fds);
        return 0;
    }
    dp->nde = direno;
    while (direno & 3)
    {
        memset(cpmdir + direno * 32, 0xe5, 32);
        ++direno;
    }
    dp->nfds = nfiles;
    /* dp->fds = realloc(dp->fds, nfiles*sizeof(struct fdesc)); \* del by agl */
    /* dp->data = realloc(cpmdir, direno*32); \* deleted by agl */
    dp->data = cpmdir;  /* */

    /* always setup buf[] if setup_cpm3_dph_dpb is running */

    dp->xlt = 0;		/* No translation table */

    dp->buf[16] = 1;		/* version indentifier DPB for CP/M 3.1	*/

    dp->buf[32 + 0] = 0x00;	/* LOW(SPT) 256 SPT	*/
    dp->buf[32 + 1] = 0x01;	/* HIGH(SPT)		*/

    dp->buf[32 + 2] = 5;	/* block shift factor   */
    dp->buf[32 + 3] = 31;	/* block mask           */
    dp->buf[32 + 4] = 1;	/* extend mask		*/

    w = blockno > 256 ? blockno - 1 : 256;
    dp->buf[32 + 5] = (BYTE)(w & 0xff); /* LOW(DSM)	*/
    dp->buf[32 + 6] = (BYTE)(w >> 8);	 /* HIGH(DSM)	*/

    w = N_ENTRIES - 1;
    dp->buf[32 + 7] = (BYTE)(w & 0xff); /* LOW(DRM)	*/
    dp->buf[32 + 8] = (BYTE)(w >> 8);	 /* HIGH(DRM)	*/

    dp->buf[32 + 9] = 0xff;	/* AL0			*/
    dp->buf[32 + 10] = 0xff;	/* AL1			*/

    dp->buf[32 + 11] = 0;	/* LOW(CKS)		*/
    dp->buf[32 + 12] = 0;	/* HIGH(CKS)		*/

    dp->buf[32 + 13] = 0;	/* LOW(OFF)		*/
    dp->buf[32 + 14] = 0;	/* HIGH(OFF)		*/

    dp->buf[32 + 15] = 0;	/* PSH			*/
    dp->buf[32 + 16] = 0;	/* PHM			*/

    if (!cpm3)
    {
        /* set up disk parameter header for CP/M 2.2 */
        dp->dph = dptable + (16 + 15) * (dp - mnttab);
        memclr(ram + dp->dph, 16 + 15);
        PutWORD(dp->dph + 8, dirbuff);	/* pointer to directory buffer */
        dp->dpb = dp->dph + 16;		/* Pointer to dpb (for yaze-ag) (agl)*/
        /* PutWORD(dp->dph+10, dp->dph+16);	\* pointer to dpb  */
        PutWORD(dp->dph + 10, dp->dpb);	/* set pointer to dpb in cp/m 2.2 (a) */

        /* setting up dpb CP/M 2.2 */
        PutWORD(dp->dpb + 0,  256);		/* sectors per track */
        PutBYTE(dp->dpb + 2,  5);		/* block shift factor */
        PutBYTE(dp->dpb + 3,  31);		/* block mask */
        PutBYTE(dp->dpb + 4,  1);		/* extent mask */
        PutWORD(dp->dpb + 5,  blockno > 256 ? blockno - 1 : 256); /* DSM */
        PutWORD(dp->dpb + 7,  N_ENTRIES - 1);	/* DRM */
        PutWORD(dp->dpb + 9,  0xffff);	/* AL0,AL1 */
        /*
            PutWORD(dp->dph+16, 256);		\* sectors per track *\
            PutBYTE(dp->dph+18, 5);		\* block shift factor *\
            PutBYTE(dp->dph+19, 31);		\* block mask *\
            PutBYTE(dp->dph+20, 1);		\* extent mask *\
            PutWORD(dp->dph+21, blockno > 256 ? blockno-1 : 256); \* DSM *\
            PutWORD(dp->dph+23, N_ENTRIES-1);	\* DRM *\
            PutWORD(dp->dph+25, 0xffff);	\* AL0,AL1 *\
        */

        alv = cpmalloc((GetWORD(dp->dph + 16 + 5) >> 3) + 1);
        if (alv == 0)
        {
            fprintf(stderr, "insufficient space to mount %s\n", filename);
            for (i = 0; i < dp->nfds; i++)
                free(dp->fds->fullname);
            free(dp->data);
            free(dp->fds);
            return 0;
        }
        PutWORD(dp->dph + 14, alv);	/* pointer to allocation vector  */
    }
    dp->filename = newstr(filename);
    dp->flags = MNT_ACTIVE | MNT_RDONLY | MNT_UNIXDIR;

    return 1;
}

static struct
{
    char magic[32];
    char dpb[15];
} sssd =
{
    "<CPM_Disk>  Drive x",
    "\x1a\x00\x03\x07\x00\xf2\x00\x3f\x00\xc0\x00\x00\x00\x02\x00"
};

static char *xlt26 =
    "\x00\x06\x0c\x12\x18\x04\x0a\x10\x16\x02\x08\x0e\x14"
    "\x01\x07\x0d\x13\x19\x05\x0b\x11\x17\x03\x09\x0f\x15";

static int
mount(int disk, const char *filename, int readonly)
{
    struct mnt *dp = mnttab + disk;
    int prot = PROT_READ | PROT_WRITE;
    int doffs, r;
    WORD alv;
    int disksize, ldisksize;	/* for calculation of disk size */
    /* BYTE buf[128];  <-- it is now defined in struct mnt (for cp/m 3) */
    struct stat st;

    if (dp->flags & MNT_ACTIVE)
        umount(disk);

    dp->flags = 0;
    if (stat(filename, &st) < 0)
    {
        perror(filename);
        return 0;
    }

#ifdef __EXTENSIONS__
    if ((st.st_mode & S_IFMT) == S_IFDIR)
    {
#else
    if ((st.st_mode & __S_IFMT) == __S_IFDIR)
    {
#endif

        if ((r = mountdir(dp, filename, lbl))
                && cpm3)
            /* if (cpm3) */
            setup_cpm3_dph_dpb(disk);
        return r;
    }

#ifdef __EXTENSIONS__
    if ((st.st_mode & S_IFMT) != S_IFREG)
    {
#else
    if ((st.st_mode & __S_IFMT) != __S_IFREG)
    {
#endif

        fprintf(stderr, "%s is neither a regular file nor a directory\n", filename);
        return 0;
    }

    if (readonly || (dp->ifd = open(filename, O_RDWR)) < 0)
    {
        prot = PROT_READ;
        dp->flags |= MNT_RDONLY;
        if ((dp->ifd = open(filename, O_RDONLY)) < 0)
        {
            perror(filename);
            return 0;
        }
    }

    /* peek at descriptor page */
    if (read(dp->ifd, dp->buf, 128) != 128)
    {
        perror(filename);
        close(dp->ifd);
        return 0;
    }
    if (memcmp(dp->buf, "<CPM_Disk>", 10) != 0)
    {
        /* WORD xlt; */
        if (st.st_size != 256256)
        {
            fprintf(stderr, "%s is not a valid <CPM_Disk> file\n", filename);
            close(dp->ifd);
            return 0;
        }
        /* assume this is an image of a sssd floppy */
        memcpy(dp->buf, &sssd, sizeof(sssd));
        if (cpm3)
        {
            dp->buf[sizeof(sssd)]   = 0;	/* psh = 0 */ /* added 27.3.2005 */
            dp->buf[sizeof(sssd) + 1] = 0;	/* phm = 0 */
            dp->xlt = 1;		/* look to setup_cpm3_dph_dpb() in bios.c */
        }
        else
        {
            dp->dph = dptable + (16 + 15) * disk;
            memclr(ram + dp->dph, 16 + 15);
            dp->xlt = cpmalloc(26);	/* space for sector translation table */
            memcpy(ram + dp->xlt, xlt26, 26);
            PutWORD(dp->dph, dp->xlt);
        }
        doffs = 0;
    }
    else
    {
        dp->xlt = 0;			/* no translation table */
        if (!cpm3)
        {
            dp->dph = dptable + (16 + 15) * disk;
            memclr(ram + dp->dph, 16 + 15);
        }
        doffs = 128;
    }
    if (cpm3)
    {
        setup_cpm3_dph_dpb(disk);
        if (dp->dsm > maxdsm
                || (dp->bls == 1024 && dp->drm > 511) /* see page 43 of the SYSTEM */
                || (dp->bls == 2048 && dp->drm > 1023) /* GUIDE, table 3-6 */
                || (dp->bls == 4096 && dp->drm > 2047)
                || (dp->bls == 8192 && dp->drm > 4095)
                || (dp->bls == 16384 && dp->drm > 8191)
                || (dp->drm > maxdrm))
        {
            fprintf(stderr, "Error to mount '%s'\n", filename);
            fprintf(stderr, "  Either the parameters at creation time of '%s' "
                    "are wrong\n  or you have created a disk file greater "
                    "8 MB with\n  the default parameters.\n", filename);
            fprintf(stderr, "  If you want to mount disks with greater size "
                    "than\n");
            fprintf(stderr, "  8 MB then use the following create commands:\n\n");
            ldisksize = (int)(2048 * (maxdsm + 1) / (1024 * 1024) + 1);
            disksize = (int)(4096 * (maxdsm + 1) / (1024 * 1024));
            fprintf(stderr, "    'create -b %d -d %d <filename> <x>M' for a "
                    "%d - %d MB disk\n", 4096, maxdrm, ldisksize, disksize);
            ldisksize = disksize + 1;
            disksize = (int)(8192 * (maxdsm + 1) / (1024 * 1024));
            fprintf(stderr, "    'create -b %d -d %d <filename> <x>M' for a "
                    "%d - %d MB disk\n", 8192, maxdrm, ldisksize, disksize);
            ldisksize = disksize + 1;
            disksize = (int)(16384 * (maxdsm + 1) / (1024 * 1024));
            fprintf(stderr, "    'create -b %d -d %d <filename> <x>M' for a "
                    "%d - %d MB disk\n\n", 16384, maxdrm, ldisksize, disksize);
            fprintf(stderr, "  Or edit MAXDSM and MAXDRM in sysdef.lib of the Z80 "
                    "bios files and\n  compile the CP/M 3.1 BIOS and "
                    "generate CPM3.SYS and CPM3.COM and\n  transfere "
                    "CPM3.COM to the UNIX file yaze-cpm3.boot.\n"
                    "  ATTENTION! Before you edit refer the sections "
                    "\"Disk Parameter Header\"\n"
                    "  and \"Disk Parameter Block\" "
                    "page 36 - 44 of the System Guide.\n");
            close(dp->ifd);
            return 0;
        }
    }
    else
    {
        PutWORD(dp->dph + 8, dirbuff);	/* pointer to directory buffer */
        dp->dpb = dp->dph + 16;		/* pointer to dpb  */
        /* PutWORD(dp->dph+10, dp->dph+16);\* set pointer to dpb in cp/m 2.2 */
        PutWORD(dp->dph + 10, dp->dpb);	/* set pointer to dpb in cp/m 2.2 */
        memcpy(ram + dp->dph + 16, dp->buf + 32, 15); /* copy dpb into cp/m ram */
        PutWORD(dp->dph + 16 + 11, 0);	/* check vector size = 0 (fixed disk) */

        /* calculate memory requirement */
        /* (((DSM+1)<<BSH) + OFFS*SPT + 1)*128 */
        dp->isize = (((GetWORD(dp->dph + 16 + 5) + 1) << GetBYTE(dp->dph + 16 + 2)) +
                     GetWORD(dp->dph + 16 + 13) * GetWORD(dp->dph + 16 + 0) + 1) * 128;

        alv = cpmalloc((GetWORD(dp->dph + 16 + 5) >> 3) + 1);
        if (alv == 0)
        {
            fprintf(stderr, "insufficient space to mount %s\n", filename);
            close(dp->ifd);
            return 0;
        }
        PutWORD(dp->dph + 14, alv);	/* pointer to allocation vector  */
    } /* of if (cpm3) */

#ifndef MAP_FILE
#define MAP_FILE 0
#endif

#ifdef __BOUNDS_CHECKING_ON
    /* absurd -1 return code blows bgcc's mind */
    dp->header = mmap(NULL, dp->isize, prot, MAP_FILE | MAP_SHARED, dp->ifd, 0);
#else
    if ((dp->header = mmap(NULL, dp->isize, prot, MAP_FILE | MAP_SHARED,
                           dp->ifd, 0)) == (char *) - 1)
    {
        perror(filename);
        close(dp->ifd);
        return 0;
    }
#endif
    dp->filename = newstr(filename);
    dp->data = (BYTE *) dp->header + doffs;
    dp->flags |= MNT_ACTIVE;

    return 1;
}

int
remount(int disk)
{
    struct mnt *dp = mnttab + disk;
    char *filename;
    int r;

    if (!(dp->flags & MNT_ACTIVE))
        return 0;
    filename = newstr(dp->filename);
    r = (dp->flags & MNT_RDONLY) ? 1 : 0;
    /*    printf("remount: disk = %d, filename = %s, r = %d\r\n",disk,filename,r);
    */
    r = mount(disk, filename, r);
    free(filename);
    return r;
}

static const char *white = " \t";

static int
dosetacc(char *cmd)
{
    char *tok = strtok(NULL, white);

    if (tok)
    {
        if (strcmp(tok, "on") == 0)
            lbl = LBL_existsbit | LBL_updatebit | LBL_accessbit;
        else if (strcmp(tok, "off") == 0)
            lbl = LBL_existsbit | LBL_updatebit;
        else
            fprintf(stderr, "setaccess needs the parameter \"on\" or "
                    "\"off\" (see \"help setaccess\")\r\n");
    }
    else
        printf("access stamps = %s\n", (lbl & LBL_accessbit) ? "ON" : "OFF");
    return 0;
}

static int
doremount(char *cmd)
{
    int d;
    char *tok = strtok(NULL, white);

    if (tok)
    {
        d = *tok - 'A';
        if (d < 0 || d > 15)
            d = *tok - 'a';
        if (d < 0 || d > 15 || tok[1])
        {
            fprintf(stderr, "illegal disk specifier: %s\n", tok);
            return 0;
        }
        /* printf("doremount: disk = %d\n",d); */
        if (mnttab[d].flags & MNT_ACTIVE)
            remount(d);
        else
            fprintf(stderr, "drive %c is not mounted\n", d + 'A');
    }
    else
        fprintf(stderr, "remount needs a disk specifier\n");
    return 0;
}

static int
domount(char *cmd)
{
    int d, v, r;
    char *tok = strtok(NULL, white);

    if ((v = (tok && (strcmp(tok, "-v") == 0))))
        tok = strtok(NULL, white);
    if ((r = tok && (strcmp(tok, "-r") == 0)))
        tok = strtok(NULL, white);
    if (tok && !v)
    {
        d = *tok - 'A';
        if (d < 0 || d > 15)
            d = *tok - 'a';
        if (d < 0 || d > 15 || tok[1])
        {
            fprintf(stderr, "illegal disk specifier: %s\r\n", tok);
            return 0;
        }
	tok = strtok(NULL, white);
	mount(d, tok, r);
	/* printf("domount: disk: %c:, ydsk-file: %s, read only = %c\r\n",'A'+d,tok,r?'Y':'N'); */
    }
    else
        for (d = 0; d < 16; d++)
            if (mnttab[d].flags & MNT_ACTIVE)
                showdisk(d, v);
    return 0;
}

static int
doumount(char *cmd)
{
    int d;
    char *tok = strtok(NULL, white);

    if (tok)
    {
        d = *tok - 'a';
        if (d < 0 || d > 15 || tok[1])
        {
            fprintf(stderr, "illegal disk specifier: %s\n", tok);
            return 0;
        }
        umount(d);
    }
    else
        fprintf(stderr, "umount needs a disk specifier\n");
    return 0;
}

struct sio siotab[MAXPSTR] =
{
    { NULL, NULL, "ttyin", 0, ST_IN2 },
    { NULL, NULL, "ttyout", 0, ST_OUT2 },
    { NULL, NULL, "crtin", 0, ST_IN2 },
    { NULL, NULL, "crtout", 0, ST_OUT2 },
    { NULL, NULL, "uc1in", 0, ST_IN2 },
    { NULL, NULL, "uc1out", 0, ST_OUT2 },
    { NULL, NULL, "rdr", 0, ST_IN },
    { NULL, NULL, "ur1", 0, ST_IN },
    { NULL, NULL, "ur2", 0, ST_IN },
    { NULL, NULL, "pun", 0, ST_OUT },
    { NULL, NULL, "up1", 0, ST_OUT },
    { NULL, NULL, "up2", 0, ST_OUT },
    { NULL, NULL, "lpt", 0, ST_OUT },
    { NULL, NULL, "ul1", 0, ST_OUT },
    { NULL, NULL, "aux", 0, ST_INOUT }
};


/* Table of logical streams */
int chn[6];

static int
doattach(char *cmd)
{
    int fd, i, opflags;
    struct sio *s;
    char *tok = strtok(NULL, white);

    if (tok)
    {
        char *p = tok + strlen(tok);
        if (p > tok && *--p == ':')
            *p = 0;
        for (i = 0; i < MAXPSTR; i++)
        {
            s = siotab + i;
            if (strncmp(tok, s->streamname, 3) == 0)
                break;
        }
        if (i == MAXPSTR)
        {
            fprintf(stderr, "stream not recognized: %s\n", tok);
            return 0;
        }
        if (s->strtype == ST_INOUT)
        {
            opflags = O_RDWR;
            /* printf("ST_INOUT\n"); */
        }
        else if (s->strtype == ST_IN2)
        {
            if (strcmp(tok, (s + 1)->streamname) == 0)
            {
                s++;
                opflags = O_WRONLY | O_CREAT | O_TRUNC;
            }
            else if (strcmp(tok, s->streamname) == 0)
                opflags = O_RDONLY;
            else
                opflags = O_RDWR | O_CREAT;
        }
        else
            opflags = s->strtype ==
                      ST_IN ? O_RDONLY : O_WRONLY | O_CREAT | O_TRUNC;
        tok = strtok(NULL, white);
        if (!tok || !*tok)
        {
            fputs("need a filename\n", stderr);
            return 0;
        }
        if (s->fp)
        {
            fclose(s->fp);
            s->fp = NULL;
            free(s->filename);
        }
        if ((fd = open(tok, opflags, 0666)) < 0)
            perror(tok);
        else
        {
            char *mode = "rb";
            if (opflags & O_WRONLY)
                mode = "wb";
            else if (opflags & O_RDWR)
                mode = "r+b";
            s->filename = newstr(tok);
            s->fp = fdopen(fd, mode);
            s->tty = isatty(fd);
            /* printf("isatty %d\n",s->tty); */
        }
    }
    else for (i = 0; i < MAXPSTR; i++)
        {
            s = siotab + i;
            if (s->fp)
                printf("%s:\t%s\n", s->streamname, s->filename);
        }
    return 0;
}

static int
dodetach(char *cmd)
{
    int i;
    struct sio *s;
    char *tok = strtok(NULL, white);

    if (tok)
    {
        char *p = tok + strlen(tok);
        if (p > tok && *--p == ':')
            *p = 0;
        for (i = 0; i < MAXPSTR; i++)
        {
            s = siotab + i;
            if (strncmp(tok, s->streamname, 3) == 0)
                break;
        }
        if (i == MAXPSTR)
        {
            fprintf(stderr, "stream not recognized: %s\n", tok);
            return 0;
        }
        if (s->fp)
        {
            fclose(s->fp);
            s->fp = NULL;
            free(s->filename);
        }
    }
    return 0;
}


static long
getval(char *s)
{
    char *tok = s + 2;

    if (*tok == 0)
        tok = strtok(NULL, white);
    if (tok && *tok)
    {
        long unit = 1;
        char u = tok[strlen(tok) - 1];
        switch (toupper(u))
        {
        case 'K':
            unit = 1024;
            break;
        case 'M':
            unit = 1024 * 1024;
            break;
        }
        return unit * strtol(tok, NULL, 10);
    }
    else
    {
        fprintf(stderr, "option needs a value: %s\n", s);
        return -1;
    }
}

static void
checkval(int ok, long val, char *msg)
{
    if (!ok)
        fprintf(stderr, "bad %s value: %ld\n", msg, val);
}

/* count ones in a 16-bit value */
static int
popcount(long v)
{
    int total;

    total = ((v >> 1) & 0x5555) + (v & 0x5555);
    total = ((total >> 2) & 0x3333) + (total & 0x3333);
    total = ((total >> 4) & 0x0f0f) + (total & 0x0f0f);
    return (total & 0xff) + (total >> 8);
}

static void
makedisk(FILE *f, char *fn, long diroffs, long dirsize, long fullsize)
{
    long n;
    BYTE sector[128];

    memset(sector, 0xe5, sizeof sector);

    /* skip offset tracks */
    if (fseek(f, diroffs, SEEK_CUR) < 0)
    {
        fclose(f);
        perror(fn);
        return;
    }
    /* write empty directory */
    for (n = 0; n < dirsize; n += sizeof sector)
        if (fwrite(sector, sizeof sector, 1, f) == 0)
        {
            fclose(f);
            perror(fn);
            return;
        }
    /* seek to end of disk and write last sector to define size */
    if (fseek(f, fullsize - sizeof sector, SEEK_SET) < 0 ||
            fwrite(sector, sizeof sector, 1, f) == 0 ||
            fclose(f) != 0)
        perror(fn);
}

/* create a new CP/M disk */
static int
docreate(char *tok)
{
    char *fn = NULL;
    FILE *f;
    long size = 1024 * 1024;
    char head[128];
    long dsm, spt = -1, bsize = -1, drm = -1, offs = -1;
    long psh = 0;	/* by agl for CP/M 3.1. */
    long phm = 0;	/* default 128 byte sectors*/
    int dblocks;
    WORD al01;

    while ((tok = strtok(NULL, white)) != NULL)
    {
        if (*tok == '-')
            switch (tok[1])
            {
            case 'b':
                bsize = getval(tok);
                break;
            case 'd':
                drm = getval(tok);
                break;
            case 'o':
                offs = getval(tok);
                break;
            case 's':
                spt = getval(tok);
                break;
            default:
                fprintf(stderr, "unrecognized option: %s\n", tok);
            }
        else
        {
            fn = tok;
            break;
        }
    }

    if (fn == NULL)
    {
        fputs("need a filename\n", stderr);
        return 0;
    }
    if ((tok = strtok(NULL, white)) != NULL)
    {
        char unit = 'b';
        int n = sscanf(tok, "%ld%c", &size, &unit);
        if (n == 2)
            switch (toupper(unit))
            {
            case 'B':
                break;
            case 'K':
                size *= 1024;
                break;
            case 'M':
                size *= 1024 * 1024;
                break;
            default:
                fprintf(stderr, "units not recognized: %s\n", tok);
                return 0;
            }
        else if (n != 1)
        {
            fprintf(stderr, "need numeric size: %s\n", tok);
            return 0;
        }
    }
    if ((f = fopen(fn, "w")) == NULL)
    {
        perror(fn);
        return 0;
    }
    if (size == 256256 && (spt == -1 || spt == 26) &&
            (bsize == -1 || bsize == 1024) &&
            (drm == -1 || drm == 63) &&
            (offs == -1 || offs == 2))
    {
        /* raw standard sssd floppy format */
        spt = 26;
        drm = 63;
        offs = 2;
        /*  we clear all tracks that might contain directory sectors,
            thus avoiding messing with the sector translation table */
        makedisk(f, fn, 128 * spt * offs, 128 * (((drm + 4) / 4 + spt - 1) / spt)*spt, size);
        return 0;
    }
    else if (size < 256 * 1024)
    {
        if (bsize == -1)
            bsize = 1024;
        if (drm == -1)
            drm = 63;
        if (spt == -1)
            spt = 26;
        if (offs == -1)
            offs = 0;
    }
    else
    {
        if (bsize == -1)
            bsize = 2048;
        if (drm == -1)
            drm = 1023;
        if (spt == -1)
        {
            spt = 128;  /* Version 1 uses 128 sectors per track */
            psh = 4;
            phm = 15; /* sector size also 2048 */
        }
        if (offs == -1)
            offs = 0;
    }
    dsm = (size - offs * spt * 128) / bsize - 1;
    checkval(spt <= 0xffff, spt, "sectors per track");
    checkval(size / (spt * 128) + offs <= 0xffff, size / (spt * 128) + offs, "tracks");
    checkval(((bsize & (bsize - 1)) == 0) &&
             (bsize >= ((dsm < 256) ? 1024 : 2048)) &&
             bsize <= 16384, bsize, "block size");
    dblocks = ((drm + 1) * 32 + bsize - 1) / bsize;
    checkval(dblocks <= 16 && dblocks < dsm, drm, "max directory entry");
    memclr(head, sizeof head);
    sprintf(head, "<CPM_Disk>");
    head[16] = 1;	/* version identifier by agl */
    /* yaze-1.10 have Version 0 */
    /* V 1: uses also PSH and PHM */
    head[32] = spt;
    head[33] = spt >> 8;
    head[34] = popcount(bsize - 1) - 7;	/* bsh */
    head[35] = (bsize / 128 - 1);		/* blm */
    head[36] = dsm < 256 ? bsize / 1024 - 1 : bsize / 2048 - 1; /* exm */
    head[37] = dsm;
    head[38] = dsm >> 8;
    head[39] = drm;
    head[40] = drm >> 8;
    al01 = ~((1 << (16 - dblocks)) - 1);
    head[41] = al01 >> 8;
    head[42] = al01;
    head[45] = offs;
    head[46] = offs >> 8;

    head[47] = psh;	/* PSH */ /* Version 1 */
    head[48] = phm;	/* PHM */

    if (fwrite(head, sizeof head, 1, f) == 0)
    {
        fclose(f);
        perror(fn);
        return 0;
    }
    makedisk(f, fn, 128 * spt * offs, 128 * (drm + 4) / 4, sizeof head + size);
    return 0;
}

static int
hexdig(char c)
{
    if ('0' <= c && c <= '9')
        return c - '0';
    if ('A' <= c && c <= 'F')
        return c - 'A' + 10;
    if ('a' <= c && c <= 'f')
        return c - 'a' + 10;
    return -1;
}

static int
doint(char *cmd)
{
    int d1, d2;
    char *tok = strtok(NULL, white);

    if (tok)
    {
        if (strlen(tok) != 2)
        {
bad:
            printf("%s invalid key specifier\n", tok);
            return 0;
        }
        /* let's face it: this doesn't work if the host character set is not ascii */
        if (tok[0] == '^' && '@' <= tok[1])
            interrupt = tok[1] & 0x1f;
        else
        {
            if ((d1 = hexdig(tok[0])) < 0)
                goto bad;
            if ((d2 = hexdig(tok[1])) < 0)
                goto bad;
            interrupt = (d1 << 4) + d2;
        }
        rawtio.c_lflag = interrupt ? ISIG : 0;
        rawtio.c_cc[VINTR] = interrupt;
    }
    else
    {
        fputs("interrupt key is ", stdout);
        if (interrupt == 0)
            puts("disabled");
        else if (interrupt < 0x20)
            printf("^%c\n", interrupt + '@');
        else
            printf("%2x\n", interrupt);
    }
    return 0;
}

extern char *perl_params;

static clock_t lastreal,tickspersec;

static int
dotime(char *cmd)
{
    /* static clock_t lastreal; */
    clock_t now;
    static struct tms lastbuf;
    struct tms tbuf;
    /* static clock_t tickspersec; */
    /* extern char *perl_params; */

    if (tickspersec == 0)
        tickspersec = sysconf(_SC_CLK_TCK);
    now = times(&tbuf);

    printf("elapsed=%.3f, user=%.3f, sys=%.3f (%s)\n",
           ((double)(now - lastreal)) / tickspersec,
           ((double)(tbuf.tms_utime - lastbuf.tms_utime)) / tickspersec,
           ((double)(tbuf.tms_stime - lastbuf.tms_stime)) / tickspersec,
           perl_params);
    lastreal = times(&tbuf);
    lastbuf = tbuf;
    return 0;
}


#define	reference 46276.0 /* Konstante aus 11569 sec * 4 MHz (Referenz) */

double t, mhz;

void timexprint()
{
	struct tms tbuf;

	if (tickspersec == 0)
	    tickspersec = sysconf(_SC_CLK_TCK);
	t = ((double)(times(&tbuf) - lastreal)) / tickspersec; /* time */ 
	if (t == 0.0) {
	    printf("Time ist 0.0 second!\n");
	} else {
	    mhz = reference / t;
	    printf("\nTime of zexall is %.3f seconds!\r\n",t);
	    printf("That is like a Z80 with %.3f MHz (%.3f GHz).\r\n",
			mhz,mhz/1000.0);
	}
}


static int
dogo(char *cmd)
{
    return 1;
}

static int
doshell(char *cmd)
{
    char *shell = getenv("SHELL");
#ifdef DEBUG
    void (*sigint)(int);

    sigint = signal(SIGINT, SIG_IGN);
#endif
    if (shell == NULL)
        shell = "/bin/sh";
    if (cmd[1])
        system(cmd + 1);
    else
    {
        system(shell);
        printf("Back in yaze-ag\n\07");
    }
#ifdef DEBUG
    (void) signal(SIGINT, sigint);
#endif
    return 0;
}

static int
do128(char *cmd)
{
    int d;
    struct mnt *dp;

    if (!cpm3)
    {
        puts("only used under CP/M 3.1");
        return 0;
    }
    always128 = 1;
    for (d = 0; d < 16; d++)
    {
        dp = mnttab + d;
        if (dp->flags & MNT_ACTIVE)
            setup_cpm3_dph_dpb(d);
    }
    puts("All disks have sektor size 128.");
    return 0;
}

static int
doclock(char *tok)
{
    if ((tok = strtok(NULL, white)) != NULL) {
	clockFrequency_save = (FASTREG) strtol(tok, NULL, 10);
	brakeini = true;
	/*
	printf("token:%s\n", tok);
	*/
    }
    if (clockFrequency_save) {
	printf("Frequency: %d kHz (%d.%d MHz)\n", clockFrequency_save,
						clockFrequency_save/1000,
						clockFrequency_save%1000);
    } else {
	puts("Frequency: max speed");
    }
    return 0;
}

static int
doquit(char *cmd)
{
    exit(0);
}

static int dohelp(char *cmd);

typedef struct
{
    char *name;				/* User printable name of the function. */
    int (*func)(char *);		/* Function to call to do the job. */
    char *doc;				/* Short documentation.  */
    char *detail;			/* Long documentation. */
} COMMAND;

static COMMAND commands[] =
{
    {
        "help",   dohelp,   "Display this text or give help about a command",
        "help <cmd>                 displays more information about <cmd>"
    },
    { "?",      dohelp,   "Synonym for `help'", NULL },
    {
        "attach", doattach, "Attach CP/M device to a unix file",
        "attach                     without arguments lists the current attachments\n"
        "attach <physdev> <file>    attaches <physdev> to the unix <file>,\n"
        "                           where <physdev> is one of ttyin, ttyout,\n"
        "                           crtin, crtout, uc1in, uc1out, rdr,\n"
        "                           ur1, ur2, pun, up1, up2, lpt, ul1"
    },
    {	"clock", doclock, "Set/Display the frequency of the Z80 simulator",
	"clock                      without argument lists the current frequency.\n"
	"                           The frequency is shown in kHz and MHz.\n"
	"                           If the frequency is 0 the emulator runs with\n"
	"                           \"max speed\".\n\n"
	"clock <frequency in kHz>   Set a frequency. The frequency must be in kHz.\n"
	"                           For example for 4 MHz place 4000.\n"
	"                           A \"0\" returns to max speed."
    },
    {
        "detach", dodetach, "Detach CP/M device from file",
        "detach <physdev>           closes the file attached to <physdev>\n"
        "                           (see attach)"
    },
    {
        "setaccess", dosetacc, "Turns on/off access time stamps for mounted directories",
        "setaccess on/off           turns on/off access time stamps for "
        "mounted\n"
        "                           directories connected to a CP/M drive\n"
        "                           (see CP/M command \"SET [ACCESS=ON/OFF]\")\n\n"
        "                           without parameter status is printed\n\n"
        "                           default : off\n\n"
        "                           (update time stamps are always on)\n"
        "                           (create time stamps are not supported by "
        "host system)"
    },
    {
        "mount",  domount,  "Mount a unix file or directory as a CP/M disk",
        "mount                      without arguments lists the mount table\n"
        "mount -v                   lists the mount table verbosely\n"
        "mount <drive> <file>       mounts <file> as CP/M disk <drive>\n"
        "                           (a letter from a..p).\n"
        "        If <file> is a plain file it must contain a CP/M filesystem.\n"
        "        If <file> is a unix directory its contents may be accessed\n"
        "           as a read-only CP/M disk\n"
        "mount -r <drive> <file>    mounts the <file> read/only."
    },
    {
        "remount", doremount, "Remount a CP/M disk",
        "remount <drive>            remounts the file/directory associated with"
        " <drive>\n"
        "                           (a directory will be fully rereaded)"
    },
    {
        "umount", doumount, "Unmount a CP/M disk",
        "umount <drive>             closes the file associated with <drive>\n"
        "                           and frees the resources"
    },
    {
        "create", docreate, "Create a new disk",
        "create {flags} <file> {size}  creates a unix <file> initialized as a\n"
        "                              CP/M disk of size {size} (default 1MB).\n"
        "       -b <block size>        default 1024 if size < 256K, else 2048\n"
        "       -d <# dir entries - 1> default 1023\n"
        "       -o <track offset>      default 0\n"
        "       -s <sectors per track> default 128\n"
        "create <file> 256256          create a raw SSSD disk image"
    },
    {
        "128", do128, "Set sektor size to 128 for all disks (only CP/M 3.1)",
        "128    Set sektor size to 128 for all disks (only CP/M 3.1)\n\n"
        "       If you create a disk file under yaze-ag and you use the default\n"
        "       blocksize and the default sectors per track (see create) the\n"
        "       sektor size is also set to 2048 bytes like the blocksize.\n\n"
        "       If you use software like a disk edit utility under CP/M 3.1\n"
        "       it can be necessary to set the sektor size to 128 bytes.\n\n"
        "       To reverse this option you must restart yaze-ag."
    },
    {
        "interrupt", doint, "Set user interrupt key",
        "interrupt <key>            makes <key> interrupt CP/M back to the monitor\n"
        "        <key> may be a 2-digit hex number or ^x where x is one of a..z[\\]^_\n"
        "        ^@ makes CP/M uninterruptible (from the keyboard)\n"
        "interrupt                  without an argument displays the current setting"
    },
    { "go",     dogo,     "Start/Continue CP/M execution", NULL },
    {
        "!",      doshell,  "Execute a unix command",
        "!                          escape to a unix shell\n"
        "!cmd                       execute unix cmd"
    },
    { "quit",   doquit,   "Terminate yaze-ag", NULL },
    {
        "time",   dotime,   "Display elapsed time since last `time' command",
        "displays elapsed, user and system time in seconds,\n"
        "         along with simulator options"
    },
    { NULL, NULL, NULL, NULL }
};

static int
dohelp(char *cmd)
{
    char *tok = strtok(NULL, white);
    int tlen;
    COMMAND *cp;

    if (tok)
    {
        for (tlen = strlen(tok), cp = commands; cp->name; cp++)
            if (strncmp(tok, cp->name, tlen) == 0)
                break;
        if (cp->name)
        {
            puts(cp->detail ? cp->detail : cp->doc);
            return 0;
        }
    }
    for (cp = commands; cp->name; cp++)
        printf("%-10s  %s\n", cp->name, cp->doc);
    return 0;
}

int
docmd(char *cmd)
{
    char *tok;
    int tlen;
    COMMAND *cp;
    int (*func)(char *) = NULL;

    if (cmd == NULL)
        return 0;
    if (*cmd == '#')
        return 0;
    while (*cmd == ' ' || *cmd == '\t' || *cmd == '\n')
        cmd++;
    for (tok = cmd + strlen(cmd) - 1; tok >= cmd; tok--)
        if (*tok == ' ' || *tok == '\t' || *tok == '\n')
            *tok = 0;
        else
            break;
    if (*cmd == 0)
        return 0;
    add_history(cmd);
    if (*cmd == '!')
    {
        /* special case */
        doshell(cmd);
        return 0;
    }
    tok = strtok(cmd, white);
    if (tok == NULL || *tok == 0)
        return 0;
    for (tlen = strlen(tok), cp = commands; cp->name; cp++)
        if (strncmp(tok, cp->name, tlen) == 0)
            /* don't allow quit command to be abbreviated */
            if (cp->func != doquit || strcmp(tok, cp->name) == 0)
            {
                if (func == NULL)
                    func = cp->func;
                else
                {
                    func = NULL;	/* ambiguous */
                    break;
                }
            }
    if (func)
        return func(cmd);
    printf("%s ?\n", tok);
    return 0;
}

#ifdef DEBUG
void
sighand(int sig)
{
    stopsim = 1;
}
#endif

void
monitor(FASTWORK adr)
{
    static char *cmd = NULL;

    ttycook();
#ifdef DEBUG
    if (adr & 0x10000)
        printf("stopped at pc=0x%04x\n", adr & 0xffff);
    stopsim = 0;
    signal(SIGINT, sighand);
#endif
#ifdef USE_GNU_READLINE
    do
    {
        if (cmd)
        {
            free(cmd);
            cmd = NULL;
        }
        cmd = readline("$>");
        if (cmd == NULL)
        {
            if ((ttyflags & ISATTY) == 0)
                doquit(NULL);
            else
                putchar('\n');
        }
    }
    while (!docmd(cmd));
#else
    if (cmd == NULL)
        cmd = xmalloc(BUFSIZ);
    do
    {
        fputs("$>", stdout);
        fflush(stdout);
        if (fgets(cmd, BUFSIZ - 1, stdin) == NULL)
        {
            if ((ttyflags & ISATTY) == 0)
                doquit(NULL);
            else
            {
                putchar('\n');
                cmd[0] = 0;
            }
        }
    }
    while (!docmd(cmd));
#endif
    ttyraw();
}
