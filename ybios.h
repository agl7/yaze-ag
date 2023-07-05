/* Header file for the basic i/o system.
   Copyright (C) 1995  Frank D. Cringle.
   Modifications for CP/M 3.1 Copyright (C) 2000/2003 by Andreas Gerlich (agl)

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


/* TTY management */
extern struct termios cookedtio, rawtio;
#define ISATTY	1
#define ISRAW	2
extern int ttyflags;
extern int interrupt;
extern void ttyraw(void), ttycook(void);


/* Table of logical streams */
extern int chn[6];

#if 0
/* CHNxxxx constants moved to chan.h for separate inclusion */
#define CHNconin	0
#define CHNconout	1
#define CHNrdr		2
#define CHNpun		3
#define CHNlst		4
#define CHNaux		5
#else
#include "chan.h"
#endif

/* Table of physical streams */
#define	SIOno	-1
#define TTYin	0
#define TTYout	1
#define CRTin	2
#define CRTout	3
#define UC1in	4
#define UC1out	5
#define RDRin	6
#define UR1in	7
#define UR2in	8
#define PUNout	9
#define UP1out	10
#define UP2out	11
#define LPTout	12
#define UL1out	13
#define	AUX	14
#define	MAXPSTR	15

#define ST_IN	0
#define ST_OUT	1
#define ST_IN2	2
#define ST_OUT2	3
#define	ST_INOUT 4

#define IOBYTE 0x95	/* agl: standard IOBYTE */

/* --------------------------------------------------------------------- */
/* special definitions for comunication between cp/m 3 and yaze: ------- */

#define SCB_columns	(0x1A)	/* Colums address in SCB */
#define SCB_lines	(0x1C)	/* Lines address in SCB */

#define SCB_MEDIA (0x54)    /* position of the MEDIA Flag in SCB */
#define SCB_MXTPA (0x62)    /* position of MXTPA in SCB		*/

#define DPH_xlt     0       /* position of the translation table (xlt)
			       in DPH (Disk Parameter Header)          */
#define DPH_mf      11      /* position of the Media Flag (MF) in DPH */
#define DPH_dpb     12      /* position of the address of the DPB    */
			    /* in the DPH                           */

#define DPB_spt     0       /* position of SPT in Disk Parameter Block */
#define DPB_bsh     2       /* position of BSH in DPB		      */
#define DPB_dsm     5       /* position of DSM in DPB		     */
#define DPB_drm     7       /* position of DRM in DPB		    */
#define DPB_cks     11      /* position of CKS in DPB              */
#define DPB_off     13      /* position of OFFSET in DPB          */
#define DPB_psh     15      /* position of PSH in DPB            */

/* -------------------------------------------------------------------*/
/* -------------------------------------------------------------------*/
/* definition for time calculation -----------------------------------*/

/* Faktor for date calculation in CP/M 3.1 */
#define DayFaktor_CPMSTART 722449L /* Day Faktor of CP/M start. One day     */
				  /* before start of CP/M 3.0 (1.1.1978).  */
				 /* (I don't know if it's the start of    */
				/*   CP/M 3.0 )			         */
			       /* Calculated with dayfaktor(31.12.1977) */

/* -------------------------------------------------------------------*/

extern struct sio
{
    FILE *fp;
    char *filename;
    char *streamname;
    char tty;
    const char strtype;
} siotab[];


/* Disk management */

/* There are two kinds of simulated disk:
   a unix file containing an image of a cp/m disk and
   a unix directory which is made to look like a cp/m disk on the fly */
#define MNT_ACTIVE	1
#define MNT_RDONLY	2
#define MNT_UNIXDIR	4

extern int cpm3;	/* flag for CP/M 3.1	*/
extern WORD dtbl;	/* dtbl			*/
extern WORD maxdsm;	/* maximal DSM supported by the bios */
extern WORD maxdrm;	/* maximal DRM supported by the bios */

extern struct mnt {		/* mount table (simulated drives A..P) */
    WORD flags;
    WORD dph;			/* address of disk parameter header in
				   cp/m 2 ram or in cp/m 3 */
    WORD xlt;		/*agl*/ /* address(z80) of the translation table */

    WORD dpb;		/*agl*/ /* address of Disk Parameter Block in CP/M 2 */

    WORD bls;		/*agl*/	/* will be calculated by bsh an blm */

    WORD dsm;		/*agl*/	/* total storage capacity of the drive */
				/* DSM is one less than the total number of */
				/* blocks on the drive. */

    WORD drm;		/*agl*/	/* total number of directory entries minus */
				/* one that can be stored on this drive */

    BYTE psh;		/*agl*/ /* PSH from the dpb */

    BYTE buf[128];	/*agl*/ /* Buffer of the first 128-Byte-Sektor of
				   the diskfile - the discriptor page */

    BYTE *data;			/* disk image (if the mounted disk is a unix
				   directory this pointer points to the
				   simulated cp/m - directory) */

    char *filename;		/* filename of disk image or unix directory */
    union {
	struct {		/* details of disk image */
	    char *header;	/* header if it's a disk image */
	    size_t isize;	/* size of disk image */
	    int ifd;		/* file descriptor of disk image */
	} image;
	struct {		/* details of unix directory */
	    int nde;		/* number of entries in cp/m directory */
	    int nfds;		/* number of files */
	    struct fdesc {	/* descriptor for each file */
		char *fullname;
		unsigned long firstblock;
		unsigned long lastblock;
		unsigned long serial;	/* unique id to aid caching */
	    } *fds;
	} udir;
    } m;
} mnttab[], *curdisk;
#define	header	m.image.header
#define	isize	m.image.isize
#define	ifd	m.image.ifd
#define	nde	m.udir.nde
#define	nfds	m.udir.nfds
#define	fds	m.udir.fds

/* We always use a block size of 4k for simulated disks constructed from unix
   directories.  The maximum possible number of cp/m directory entries on such a
   disk is 2032. */
#define BLOCK_SIZE	4096
#define N_ENTRIES	2032
#define COVER(x,y)	(((x)-1)/(y)+1)

#ifdef DEBUG
void sighand(int sig);
#endif

/*-------------------------------------------- prototyping -------------*/
void timexprint();
void clearfc(struct mnt *dp);
void setup_cpm3_dph_dpb(int disk);
unsigned long dayfaktor(unsigned long day,
			unsigned long month,
			unsigned long year);

