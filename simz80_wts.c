/* Z80 instruction set simulator.
   Copyright (C) 1995  Frank D. Cringle.

This file is part of yaze - yet another Z80 emulator.

Yaze is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

/* This file was generated from simz80.pl
   with the following choice of options */
/*
char *perl_params =
    "combine=0,"
    "optab=0,"
    "cb_inline=1,"
    "dfd_inline=1,"
    "ed_inline=1";
*/

#include <stdio.h>
/* #include <stdbool.h> */
#include "mem_mmu.h"
#include "simz80.h"
#include "ytypes.h"

static const unsigned char partab[256] = {
	4,0,0,4,0,4,4,0,0,4,4,0,4,0,0,4,
	0,4,4,0,4,0,0,4,4,0,0,4,0,4,4,0,
	0,4,4,0,4,0,0,4,4,0,0,4,0,4,4,0,
	4,0,0,4,0,4,4,0,0,4,4,0,4,0,0,4,
	0,4,4,0,4,0,0,4,4,0,0,4,0,4,4,0,
	4,0,0,4,0,4,4,0,0,4,4,0,4,0,0,4,
	4,0,0,4,0,4,4,0,0,4,4,0,4,0,0,4,
	0,4,4,0,4,0,0,4,4,0,0,4,0,4,4,0,
	0,4,4,0,4,0,0,4,4,0,0,4,0,4,4,0,
	4,0,0,4,0,4,4,0,0,4,4,0,4,0,0,4,
	4,0,0,4,0,4,4,0,0,4,4,0,4,0,0,4,
	0,4,4,0,4,0,0,4,4,0,0,4,0,4,4,0,
	4,0,0,4,0,4,4,0,0,4,4,0,4,0,0,4,
	0,4,4,0,4,0,0,4,4,0,0,4,0,4,4,0,
	0,4,4,0,4,0,0,4,4,0,0,4,0,4,4,0,
	4,0,0,4,0,4,4,0,0,4,4,0,4,0,0,4,
};

extern bool rtc_avail;
extern ui32 sim_os_msec();
extern ui32 sim_os_ms_sleep(unsigned int milliseconds);

#define parity(x)	partab[(x)&0xff]

#ifdef DEBUG
volatile int stopsim;
#endif

#define POP(x)	do {							\
	FASTREG y = RAM_pp(SP);						\
	x = y + (RAM_pp(SP) << 8);					\
} while (0)

#define PUSH(x) do {							\
	mm_RAM(SP) = (x) >> 8;						\
	mm_RAM(SP) = x;							\
} while (0)

#define JPC(cond) { tStates += 10; PC = cond ? GetWORD(PC) : PC+2; }

#define CALLC(cond) {							\
    if (cond) {								\
	FASTREG adrr = GetWORD(PC);					\
	PUSH(PC+2);							\
	PC = adrr;							\
	tStates += 17;							\
    } else {								\
	PC += 2;							\
	tStates += 10;							\
    }									\
}

/* load Z80 registers into (we hope) host registers */
#define LOAD_STATE()							\
    PC = pc;								\
    AF = af[af_sel];							\
    BC = regs[regs_sel].bc;						\
    DE = regs[regs_sel].de;						\
    HL = regs[regs_sel].hl;						\
    IX = ix;								\
    IY = iy;								\
    SP = sp

/* load Z80 registers into (we hope) host registers */
#define DECLARE_STATE()							\
    FASTREG PC = pc;							\
    FASTREG AF = af[af_sel];						\
    FASTREG BC = regs[regs_sel].bc;					\
    FASTREG DE = regs[regs_sel].de;					\
    FASTREG HL = regs[regs_sel].hl;					\
    FASTREG IX = ix;							\
    FASTREG IY = iy;							\
    FASTREG SP = sp

/* save Z80 registers back into memory */
#define SAVE_STATE()							\
    pc = PC;								\
    af[af_sel] = AF;							\
    regs[regs_sel].bc = BC;						\
    regs[regs_sel].de = DE;						\
    regs[regs_sel].hl = HL;						\
    ix = IX;								\
    iy = IY;								\
    sp = SP;								\
    tStates_save = tStates;						\
    tStatesInSlice_save = tStatesInSlice;				\
    startTime_save = startTime;						\
    clockFrequency_save = clockFrequency;

FASTWORK
simz80_with_tStates(FASTREG PC)
{
    FASTREG AF = af[af_sel];
    FASTREG BC = regs[regs_sel].bc;
    FASTREG DE = regs[regs_sel].de;
    FASTREG HL = regs[regs_sel].hl;
    FASTREG SP = sp;
    FASTREG IX = ix;
    FASTREG IY = iy;
    FASTWORK temp, acu, sum, cbits;
    FASTWORK op, adr;
#ifdef MMU
    FASTREG tmp2;
#endif

    register FASTREG tStates = tStates_save;
    register FASTREG tStatesInSlice = tStatesInSlice_save;
    			/* number of t-states in 10 mSec time-slice */
    FASTREG startTime = startTime_save;
    FASTREG now;
    register FASTREG clockFrequency = clockFrequency_save;
	/* in kHz --> for example 4000 kHz */
    static const FASTREG sliceLength = 10;	/* 10 msec */
    bool tStateModifier = false;

    if ((sim_os_msec()) >= startTime) {
	tStates = 0;   
	/* putchar('m'); fflush(stdout); */
	startTime = sim_os_msec();
    }
    
    /* initialization of the brake */
    if (brakeini) {
	brakeini = false;
	tStates = 0;
	if (rtc_avail) {
	    tStatesInSlice = sliceLength * clockFrequency;
	    /* printf("In brackeIni! %d\r\n",tStatesInSlice); */
	    startTime = clockFrequency ? sim_os_msec() : 0;
	} else {/* make sure that sim_os_msec() is not called later */
	    clockFrequency = startTime = tStatesInSlice = 0;
	}
    }
    


#ifdef DEBUG
    while (!stopsim) {
#else
    while (1) {
#endif

    /* the brake */
    if (clockFrequency && (tStates >= tStatesInSlice)) {
	/* clockFrequency != 0 implies that real time clock is available */
	startTime += sliceLength;
	tStates -= tStatesInSlice;
	if (startTime > (now = sim_os_msec())) {
	    sim_os_ms_sleep(startTime - now);
	    /* putchar('.'); fflush(stdout); */
	}
    }


    switch(RAM_pp(PC)) {
	case 0x00:			/* NOP */
		tStates += 4;		/* 4 */
		break;
	case 0x01:			/* LD BC,nnnn */
		tStates += 10;
		BC = GetWORD(PC);
		PC += 2;
		break;
	case 0x02:			/* LD (BC),A */
		tStates += 7;
		PutBYTE(BC, hreg(AF));
		break;
	case 0x03:			/* INC BC */
		tStates += 6;
		++BC;
		break;
	case 0x04:			/* INC B */
		tStates += 4;
		BC += 0x100;
		temp = hreg(BC);
		AF = (AF & ~0xfe) | (temp & 0xa8) |
			(((temp & 0xff) == 0) << 6) |
			(((temp & 0xf) == 0) << 4) |
			((temp == 0x80) << 2);
		break;
	case 0x05:			/* DEC B */
		tStates += 4;
		BC -= 0x100;
		temp = hreg(BC);
		AF = (AF & ~0xfe) | (temp & 0xa8) |
			(((temp & 0xff) == 0) << 6) |
			(((temp & 0xf) == 0xf) << 4) |
			((temp == 0x7f) << 2) | 2;
		break;
	case 0x06:			/* LD B,nn */
		tStates += 7;
		Sethreg(BC, GetBYTE_pp(PC));
		break;
	case 0x07:			/* RLCA */
		tStates += 4;
		AF = ((AF >> 7) & 0x0128) | ((AF << 1) & ~0x1ff) |
			(AF & 0xc4) | ((AF >> 15) & 1);
		break;
	case 0x08:			/* EX AF,AF' */
		tStates += 4;
		af[af_sel] = AF;
		af_sel = 1 - af_sel;
		AF = af[af_sel];
		break;
	case 0x09:			/* ADD HL,BC */
		tStates += 11;
		HL &= 0xffff;
		BC &= 0xffff;
		sum = HL + BC;
		cbits = (HL ^ BC ^ sum) >> 8;
		HL = sum;
		AF = (AF & ~0x3b) | ((sum >> 8) & 0x28) |
			(cbits & 0x10) | ((cbits >> 8) & 1);
		break;
	case 0x0A:			/* LD A,(BC) */
		tStates += 7;
		Sethreg(AF, GetBYTE(BC));
		break;
	case 0x0B:			/* DEC BC */
		tStates += 6;
		--BC;
		break;
	case 0x0C:			/* INC C */
		tStates += 4;
		temp = lreg(BC)+1;
		Setlreg(BC, temp);
		AF = (AF & ~0xfe) | (temp & 0xa8) |
			(((temp & 0xff) == 0) << 6) |
			(((temp & 0xf) == 0) << 4) |
			((temp == 0x80) << 2);
		break;
	case 0x0D:			/* DEC C */
		tStates += 4;
		temp = lreg(BC)-1;
		Setlreg(BC, temp);
		AF = (AF & ~0xfe) | (temp & 0xa8) |
			(((temp & 0xff) == 0) << 6) |
			(((temp & 0xf) == 0xf) << 4) |
			((temp == 0x7f) << 2) | 2;
		break;
	case 0x0E:			/* LD C,nn */
		tStates += 7;
		Setlreg(BC, GetBYTE_pp(PC));
		break;
	case 0x0F:			/* RRCA */
		tStates += 4;
		temp = hreg(AF);
		sum = temp >> 1;
		AF = ((temp & 1) << 15) | (sum << 8) |
			(sum & 0x28) | (AF & 0xc4) | (temp & 1);
		break;
	case 0x10:			/* DJNZ dd */
		/* ursprüngliche Zeile:
		PC += ((BC -= 0x100) & 0xff00) ? (signed char) GetBYTE(PC) + 1 : 1;
		*/
		if ((BC -= 0x100) & 0xff00) {
		    PC += (signed char) GetBYTE(PC) + 1;
		    tStates += 13;
		} else {
		    PC++;
		    tStates += 8;
		}
		break;
	case 0x11:			/* LD DE,nnnn */
		tStates += 10;
		DE = GetWORD(PC);
		PC += 2;
		break;
	case 0x12:			/* LD (DE),A */
		tStates += 7;
		PutBYTE(DE, hreg(AF));
		break;
	case 0x13:			/* INC DE */
		tStates += 6;
		++DE;
		break;
	case 0x14:			/* INC D */
		tStates += 4;
		DE += 0x100;
		temp = hreg(DE);
		AF = (AF & ~0xfe) | (temp & 0xa8) |
			(((temp & 0xff) == 0) << 6) |
			(((temp & 0xf) == 0) << 4) |
			((temp == 0x80) << 2);
		break;
	case 0x15:			/* DEC D */
		tStates += 4;
		DE -= 0x100;
		temp = hreg(DE);
		AF = (AF & ~0xfe) | (temp & 0xa8) |
			(((temp & 0xff) == 0) << 6) |
			(((temp & 0xf) == 0xf) << 4) |
			((temp == 0x7f) << 2) | 2;
		break;
	case 0x16:			/* LD D,nn */
		tStates += 7;
		Sethreg(DE, GetBYTE_pp(PC));
		break;
	case 0x17:			/* RLA */
		tStates += 4;
		AF = ((AF << 8) & 0x0100) | ((AF >> 7) & 0x28) | ((AF << 1) & ~0x01ff) |
			(AF & 0xc4) | ((AF >> 15) & 1);
		break;
	case 0x18:			/* JR dd */
		tStates += 12;
		PC += (1) ? (signed char) GetBYTE(PC) + 1 : 1;
		break;
	case 0x19:			/* ADD HL,DE */
		tStates += 11;
		HL &= 0xffff;
		DE &= 0xffff;
		sum = HL + DE;
		cbits = (HL ^ DE ^ sum) >> 8;
		HL = sum;
		AF = (AF & ~0x3b) | ((sum >> 8) & 0x28) |
			(cbits & 0x10) | ((cbits >> 8) & 1);
		break;
	case 0x1A:			/* LD A,(DE) */
		tStates += 4;
		Sethreg(AF, GetBYTE(DE));
		break;
	case 0x1B:			/* DEC DE */
		tStates += 6;
		--DE;
		break;
	case 0x1C:			/* INC E */
		tStates += 4;
		temp = lreg(DE)+1;
		Setlreg(DE, temp);
		AF = (AF & ~0xfe) | (temp & 0xa8) |
			(((temp & 0xff) == 0) << 6) |
			(((temp & 0xf) == 0) << 4) |
			((temp == 0x80) << 2);
		break;
	case 0x1D:			/* DEC E */
		tStates += 4;
		temp = lreg(DE)-1;
		Setlreg(DE, temp);
		AF = (AF & ~0xfe) | (temp & 0xa8) |
			(((temp & 0xff) == 0) << 6) |
			(((temp & 0xf) == 0xf) << 4) |
			((temp == 0x7f) << 2) | 2;
		break;
	case 0x1E:			/* LD E,nn */
		tStates += 7;
		Setlreg(DE, GetBYTE_pp(PC));
		break;
	case 0x1F:			/* RRA */
		tStates += 4;
		temp = hreg(AF);
		sum = temp >> 1;
		AF = ((AF & 1) << 15) | (sum << 8) |
			(sum & 0x28) | (AF & 0xc4) | (temp & 1);
		break;
	case 0x20:			/* JR NZ,dd */
		/* PC += (!TSTFLAG(Z)) ? (signed char) GetBYTE(PC) + 1 : 1; */
		if (TSTFLAG(Z)) {
		    PC++;
		    tStates += 7;
		} else {
		    PC += (signed char) GetBYTE(PC) + 1;
		    tStates += 12;
		}
		break;
	case 0x21:			/* LD HL,nnnn */
		tStates += 10;
		HL = GetWORD(PC);
		PC += 2;
		break;
	case 0x22:			/* LD (nnnn),HL */
		tStates += 16;
		temp = GetWORD(PC);
		PutWORD(temp, HL);
		PC += 2;
		break;
	case 0x23:			/* INC HL */
		tStates += 6;
		++HL;
		break;
	case 0x24:			/* INC H */
		tStates += 4;
		HL += 0x100;
		temp = hreg(HL);
		AF = (AF & ~0xfe) | (temp & 0xa8) |
			(((temp & 0xff) == 0) << 6) |
			(((temp & 0xf) == 0) << 4) |
			((temp == 0x80) << 2);
		break;
	case 0x25:			/* DEC H */
		tStates += 4;
		HL -= 0x100;
		temp = hreg(HL);
		AF = (AF & ~0xfe) | (temp & 0xa8) |
			(((temp & 0xff) == 0) << 6) |
			(((temp & 0xf) == 0xf) << 4) |
			((temp == 0x7f) << 2) | 2;
		break;
	case 0x26:			/* LD H,nn */
		tStates += 7;
		Sethreg(HL, GetBYTE_pp(PC));
		break;
	case 0x27:			/* DAA */
		tStates += 4;
		acu = hreg(AF);
		temp = ldig(acu);
		cbits = TSTFLAG(C);
		if (TSTFLAG(N)) {	/* last operation was a subtract */
			int hd = cbits || acu > 0x99;
			if (TSTFLAG(H) || (temp > 9)) { /* adjust low digit */
				if (temp > 5)
					SETFLAG(H, 0);
				acu -= 6;
				acu &= 0xff;
			}
			if (hd)		/* adjust high digit */
				acu -= 0x160;
		}
		else {			/* last operation was an add */
			if (TSTFLAG(H) || (temp > 9)) { /* adjust low digit */
				SETFLAG(H, (temp > 9));
				acu += 6;
			}
			if (cbits || ((acu & 0x1f0) > 0x90)) /* adjust high digit */
				acu += 0x60;
		}
		cbits |= (acu >> 8) & 1;
		acu &= 0xff;
		AF = (acu << 8) | (acu & 0xa8) | ((acu == 0) << 6) |
			(AF & 0x12) | partab[acu] | cbits;
		break;
	case 0x28:			/* JR Z,dd */
		PC += (TSTFLAG(Z)) ? (tStates += 12,(signed char) GetBYTE(PC) + 1)
				   : (tStates += 7, 1);
		/*
		if (TSTFLAG(Z)) {
		    PC += (signed char) GetBYTE(PC) + 1;
		    tStates += 12;
		} else {
		    PC++;
		    tStates += 7;
		}
		*/
		break;
	case 0x29:			/* ADD HL,HL */
		tStates += 11;
		HL &= 0xffff;
		sum = HL + HL;
		cbits = (HL ^ HL ^ sum) >> 8;
		HL = sum;
		AF = (AF & ~0x3b) | ((sum >> 8) & 0x28) |
			(cbits & 0x10) | ((cbits >> 8) & 1);
		break;
	case 0x2A:			/* LD HL,(nnnn) */
		tStates += 16;
		temp = GetWORD(PC);
		HL = GetWORD(temp);
		PC += 2;
		break;
	case 0x2B:			/* DEC HL */
		tStates += 6;
		--HL;
		break;
	case 0x2C:			/* INC L */
		tStates += 4;
		temp = lreg(HL)+1;
		Setlreg(HL, temp);
		AF = (AF & ~0xfe) | (temp & 0xa8) |
			(((temp & 0xff) == 0) << 6) |
			(((temp & 0xf) == 0) << 4) |
			((temp == 0x80) << 2);
		break;
	case 0x2D:			/* DEC L */
		tStates += 4;
		temp = lreg(HL)-1;
		Setlreg(HL, temp);
		AF = (AF & ~0xfe) | (temp & 0xa8) |
			(((temp & 0xff) == 0) << 6) |
			(((temp & 0xf) == 0xf) << 4) |
			((temp == 0x7f) << 2) | 2;
		break;
	case 0x2E:			/* LD L,nn */
		tStates += 7;
		Setlreg(HL, GetBYTE_pp(PC));
		break;
	case 0x2F:			/* CPL */
		tStates += 4;
		AF = (~AF & ~0xff) | (AF & 0xc5) | ((~AF >> 8) & 0x28) | 0x12;
		break;
	case 0x30:			/* JR NC,dd */
		PC += (!TSTFLAG(C)) ? (tStates += 12,(signed char) GetBYTE(PC) + 1)
				    : (tStates += 7, 1);
		break;
	case 0x31:			/* LD SP,nnnn */
		tStates += 10;
		SP = GetWORD(PC);
		PC += 2;
		break;
	case 0x32:			/* LD (nnnn),A */
		tStates += 13;
		temp = GetWORD(PC);
		PutBYTE(temp, hreg(AF));
		PC += 2;
		break;
	case 0x33:			/* INC SP */
		tStates += 6;
		++SP;
		break;
	case 0x34:			/* INC (HL) */
		tStates += 11;
		temp = GetBYTE(HL)+1;
		PutBYTE(HL, temp);
		AF = (AF & ~0xfe) | (temp & 0xa8) |
			(((temp & 0xff) == 0) << 6) |
			(((temp & 0xf) == 0) << 4) |
			((temp == 0x80) << 2);
		break;
	case 0x35:			/* DEC (HL) */
		tStates += 11;
		temp = GetBYTE(HL)-1;
		PutBYTE(HL, temp);
		AF = (AF & ~0xfe) | (temp & 0xa8) |
			(((temp & 0xff) == 0) << 6) |
			(((temp & 0xf) == 0xf) << 4) |
			((temp == 0x7f) << 2) | 2;
		break;
	case 0x36:			/* LD (HL),nn */
		tStates += 10;
		PutBYTE(HL, GetBYTE_pp(PC));
		break;
	case 0x37:			/* SCF */
		tStates += 4;
		AF = (AF&~0x3b)|((AF>>8)&0x28)|1;
		break;
	case 0x38:			/* JR C,dd */
		PC += (TSTFLAG(C)) ? (tStates +=12, (signed char) GetBYTE(PC) + 1)
				   : (tStates += 7, 1);
		break;
	case 0x39:			/* ADD HL,SP */
		tStates += 11;
		HL &= 0xffff;
		SP &= 0xffff;
		sum = HL + SP;
		cbits = (HL ^ SP ^ sum) >> 8;
		HL = sum;
		AF = (AF & ~0x3b) | ((sum >> 8) & 0x28) |
			(cbits & 0x10) | ((cbits >> 8) & 1);
		break;
	case 0x3A:			/* LD A,(nnnn) */
		tStates += 13;
		temp = GetWORD(PC);
		Sethreg(AF, GetBYTE(temp));
		PC += 2;
		break;
	case 0x3B:			/* DEC SP */
		tStates += 6;
		--SP;
		break;
	case 0x3C:			/* INC A */
		tStates += 4;
		AF += 0x100;
		temp = hreg(AF);
		AF = (AF & ~0xfe) | (temp & 0xa8) |
			(((temp & 0xff) == 0) << 6) |
			(((temp & 0xf) == 0) << 4) |
			((temp == 0x80) << 2);
		break;
	case 0x3D:			/* DEC A */
		tStates += 4;
		AF -= 0x100;
		temp = hreg(AF);
		AF = (AF & ~0xfe) | (temp & 0xa8) |
			(((temp & 0xff) == 0) << 6) |
			(((temp & 0xf) == 0xf) << 4) |
			((temp == 0x7f) << 2) | 2;
		break;
	case 0x3E:			/* LD A,nn */
		tStates += 7;
		Sethreg(AF, GetBYTE_pp(PC));
		break;
	case 0x3F:			/* CCF */
		tStates += 4;
		AF = (AF&~0x3b)|((AF>>8)&0x28)|((AF&1)<<4)|(~AF&1);
		break;
	case 0x40:			/* LD B,B */
		/* nop */
		tStates += 4;
		break;
	case 0x41:			/* LD B,C */
		tStates += 4;
		BC = (BC & 255) | ((BC & 255) << 8);
		break;
	case 0x42:			/* LD B,D */
		tStates += 4;
		BC = (BC & 255) | (DE & ~255);
		break;
	case 0x43:			/* LD B,E */
		tStates += 4;
		BC = (BC & 255) | ((DE & 255) << 8);
		break;
	case 0x44:			/* LD B,H */
		tStates += 4;
		BC = (BC & 255) | (HL & ~255);
		break;
	case 0x45:			/* LD B,L */
		tStates += 4;
		BC = (BC & 255) | ((HL & 255) << 8);
		break;
	case 0x46:			/* LD B,(HL) */
		tStates += 7;
		Sethreg(BC, GetBYTE(HL));
		break;
	case 0x47:			/* LD B,A */
		tStates += 4;
		BC = (BC & 255) | (AF & ~255);
		break;
	case 0x48:			/* LD C,B */
		tStates += 4;
		BC = (BC & ~255) | ((BC >> 8) & 255);
		break;
	case 0x49:			/* LD C,C */
		tStates += 4;
		/* nop */
		break;
	case 0x4A:			/* LD C,D */
		tStates += 4;
		BC = (BC & ~255) | ((DE >> 8) & 255);
		break;
	case 0x4B:			/* LD C,E */
		tStates += 4;
		BC = (BC & ~255) | (DE & 255);
		break;
	case 0x4C:			/* LD C,H */
		tStates += 4;
		BC = (BC & ~255) | ((HL >> 8) & 255);
		break;
	case 0x4D:			/* LD C,L */
		tStates += 4;
		BC = (BC & ~255) | (HL & 255);
		break;
	case 0x4E:			/* LD C,(HL) */
		tStates += 7;
		Setlreg(BC, GetBYTE(HL));
		break;
	case 0x4F:			/* LD C,A */
		tStates += 4;
		BC = (BC & ~255) | ((AF >> 8) & 255);
		break;
	case 0x50:			/* LD D,B */
		tStates += 4;
		DE = (DE & 255) | (BC & ~255);
		break;
	case 0x51:			/* LD D,C */
		tStates += 4;
		DE = (DE & 255) | ((BC & 255) << 8);
		break;
	case 0x52:			/* LD D,D */
		tStates += 4;
		/* nop */
		break;
	case 0x53:			/* LD D,E */
		tStates += 4;
		DE = (DE & 255) | ((DE & 255) << 8);
		break;
	case 0x54:			/* LD D,H */
		tStates += 4;
		DE = (DE & 255) | (HL & ~255);
		break;
	case 0x55:			/* LD D,L */
		tStates += 4;
		DE = (DE & 255) | ((HL & 255) << 8);
		break;
	case 0x56:			/* LD D,(HL) */
		tStates += 7;
		Sethreg(DE, GetBYTE(HL));
		break;
	case 0x57:			/* LD D,A */
		tStates += 4;
		DE = (DE & 255) | (AF & ~255);
		break;
	case 0x58:			/* LD E,B */
		tStates += 4;
		DE = (DE & ~255) | ((BC >> 8) & 255);
		break;
	case 0x59:			/* LD E,C */
		tStates += 4;
		DE = (DE & ~255) | (BC & 255);
		break;
	case 0x5A:			/* LD E,D */
		tStates += 4;
		DE = (DE & ~255) | ((DE >> 8) & 255);
		break;
	case 0x5B:			/* LD E,E */
		tStates += 4;
		/* nop */
		break;
	case 0x5C:			/* LD E,H */
		tStates += 4;
		DE = (DE & ~255) | ((HL >> 8) & 255);
		break;
	case 0x5D:			/* LD E,L */
		tStates += 4;
		DE = (DE & ~255) | (HL & 255);
		break;
	case 0x5E:			/* LD E,(HL) */
		tStates += 7;
		Setlreg(DE, GetBYTE(HL));
		break;
	case 0x5F:			/* LD E,A */
		tStates += 4;
		DE = (DE & ~255) | ((AF >> 8) & 255);
		break;
	case 0x60:			/* LD H,B */
		tStates += 4;
		HL = (HL & 255) | (BC & ~255);
		break;
	case 0x61:			/* LD H,C */
		tStates += 4;
		HL = (HL & 255) | ((BC & 255) << 8);
		break;
	case 0x62:			/* LD H,D */
		tStates += 4;
		HL = (HL & 255) | (DE & ~255);
		break;
	case 0x63:			/* LD H,E */
		tStates += 4;
		HL = (HL & 255) | ((DE & 255) << 8);
		break;
	case 0x64:			/* LD H,H */
		tStates += 4;
		/* nop */
		break;
	case 0x65:			/* LD H,L */
		tStates += 4;
		HL = (HL & 255) | ((HL & 255) << 8);
		break;
	case 0x66:			/* LD H,(HL) */
		tStates += 7;
		Sethreg(HL, GetBYTE(HL));
		break;
	case 0x67:			/* LD H,A */
		tStates += 4;
		HL = (HL & 255) | (AF & ~255);
		break;
	case 0x68:			/* LD L,B */
		tStates += 4;
		HL = (HL & ~255) | ((BC >> 8) & 255);
		break;
	case 0x69:			/* LD L,C */
		tStates += 4;
		HL = (HL & ~255) | (BC & 255);
		break;
	case 0x6A:			/* LD L,D */
		tStates += 4;
		HL = (HL & ~255) | ((DE >> 8) & 255);
		break;
	case 0x6B:			/* LD L,E */
		tStates += 4;
		HL = (HL & ~255) | (DE & 255);
		break;
	case 0x6C:			/* LD L,H */
		tStates += 4;
		HL = (HL & ~255) | ((HL >> 8) & 255);
		break;
	case 0x6D:			/* LD L,L */
		tStates += 4;
		/* nop */
		break;
	case 0x6E:			/* LD L,(HL) */
		tStates += 7;
		Setlreg(HL, GetBYTE(HL));
		break;
	case 0x6F:			/* LD L,A */
		tStates += 4;
		HL = (HL & ~255) | ((AF >> 8) & 255);
		break;
	case 0x70:			/* LD (HL),B */
		tStates += 7;
		PutBYTE(HL, hreg(BC));
		break;
	case 0x71:			/* LD (HL),C */
		tStates += 7;
		PutBYTE(HL, lreg(BC));
		break;
	case 0x72:			/* LD (HL),D */
		tStates += 7;
		PutBYTE(HL, hreg(DE));
		break;
	case 0x73:			/* LD (HL),E */
		tStates += 7;
		PutBYTE(HL, lreg(DE));
		break;
	case 0x74:			/* LD (HL),H */
		tStates += 7;
		PutBYTE(HL, hreg(HL));
		break;
	case 0x75:			/* LD (HL),L */
		PutBYTE(HL, lreg(HL));
		break;
	case 0x76:			/* HALT */
		tStates += 4;
		SAVE_STATE();
		return PC&0xffff;
	case 0x77:			/* LD (HL),A */
		tStates += 7;
		PutBYTE(HL, hreg(AF));
		break;
	case 0x78:			/* LD A,B */
		tStates += 4;
		AF = (AF & 255) | (BC & ~255);
		break;
	case 0x79:			/* LD A,C */
		tStates += 4;
		AF = (AF & 255) | ((BC & 255) << 8);
		break;
	case 0x7A:			/* LD A,D */
		tStates += 4;
		AF = (AF & 255) | (DE & ~255);
		break;
	case 0x7B:			/* LD A,E */
		tStates += 4;
		AF = (AF & 255) | ((DE & 255) << 8);
		break;
	case 0x7C:			/* LD A,H */
		tStates += 4;
		AF = (AF & 255) | (HL & ~255);
		break;
	case 0x7D:			/* LD A,L */
		tStates += 4;
		AF = (AF & 255) | ((HL & 255) << 8);
		break;
	case 0x7E:			/* LD A,(HL) */
		tStates += 7;
		Sethreg(AF, GetBYTE(HL));
		break;
	case 0x7F:			/* LD A,A */
		tStates += 4;
		/* nop */
		break;
	case 0x80:			/* ADD A,B */
		tStates += 4;
		temp = hreg(BC);
		acu = hreg(AF);
		sum = acu + temp;
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) |
			((cbits >> 8) & 1);
		break;
	case 0x81:			/* ADD A,C */
		tStates += 4;
		temp = lreg(BC);
		acu = hreg(AF);
		sum = acu + temp;
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) |
			((cbits >> 8) & 1);
		break;
	case 0x82:			/* ADD A,D */
		tStates += 4;
		temp = hreg(DE);
		acu = hreg(AF);
		sum = acu + temp;
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) |
			((cbits >> 8) & 1);
		break;
	case 0x83:			/* ADD A,E */
		tStates += 4;
		temp = lreg(DE);
		acu = hreg(AF);
		sum = acu + temp;
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) |
			((cbits >> 8) & 1);
		break;
	case 0x84:			/* ADD A,H */
		tStates += 4;
		temp = hreg(HL);
		acu = hreg(AF);
		sum = acu + temp;
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) |
			((cbits >> 8) & 1);
		break;
	case 0x85:			/* ADD A,L */
		tStates += 4;
		temp = lreg(HL);
		acu = hreg(AF);
		sum = acu + temp;
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) |
			((cbits >> 8) & 1);
		break;
	case 0x86:			/* ADD A,(HL) */
		tStates += 7;
		temp = GetBYTE(HL);
		acu = hreg(AF);
		sum = acu + temp;
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) |
			((cbits >> 8) & 1);
		break;
	case 0x87:			/* ADD A,A */
		tStates += 4;
		temp = hreg(AF);
		acu = hreg(AF);
		sum = acu + temp;
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) |
			((cbits >> 8) & 1);
		break;
	case 0x88:			/* ADC A,B */
		tStates += 4;
		temp = hreg(BC);
		acu = hreg(AF);
		sum = acu + temp + TSTFLAG(C);
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) |
			((cbits >> 8) & 1);
		break;
	case 0x89:			/* ADC A,C */
		tStates += 4;
		temp = lreg(BC);
		acu = hreg(AF);
		sum = acu + temp + TSTFLAG(C);
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) |
			((cbits >> 8) & 1);
		break;
	case 0x8A:			/* ADC A,D */
		tStates += 4;
		temp = hreg(DE);
		acu = hreg(AF);
		sum = acu + temp + TSTFLAG(C);
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) |
			((cbits >> 8) & 1);
		break;
	case 0x8B:			/* ADC A,E */
		tStates += 4;
		temp = lreg(DE);
		acu = hreg(AF);
		sum = acu + temp + TSTFLAG(C);
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) |
			((cbits >> 8) & 1);
		break;
	case 0x8C:			/* ADC A,H */
		tStates += 4;
		temp = hreg(HL);
		acu = hreg(AF);
		sum = acu + temp + TSTFLAG(C);
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) |
			((cbits >> 8) & 1);
		break;
	case 0x8D:			/* ADC A,L */
		tStates += 4;
		temp = lreg(HL);
		acu = hreg(AF);
		sum = acu + temp + TSTFLAG(C);
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) |
			((cbits >> 8) & 1);
		break;
	case 0x8E:			/* ADC A,(HL) */
		tStates += 7;
		temp = GetBYTE(HL);
		acu = hreg(AF);
		sum = acu + temp + TSTFLAG(C);
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) |
			((cbits >> 8) & 1);
		break;
	case 0x8F:			/* ADC A,A */
		tStates += 4;
		temp = hreg(AF);
		acu = hreg(AF);
		sum = acu + temp + TSTFLAG(C);
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) |
			((cbits >> 8) & 1);
		break;
	case 0x90:			/* SUB B */
		tStates += 4;
		temp = hreg(BC);
		acu = hreg(AF);
		sum = acu - temp;
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			((cbits >> 8) & 1);
		break;
	case 0x91:			/* SUB C */
		tStates += 4;
		temp = lreg(BC);
		acu = hreg(AF);
		sum = acu - temp;
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			((cbits >> 8) & 1);
		break;
	case 0x92:			/* SUB D */
		tStates += 4;
		temp = hreg(DE);
		acu = hreg(AF);
		sum = acu - temp;
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			((cbits >> 8) & 1);
		break;
	case 0x93:			/* SUB E */
		tStates += 4;
		temp = lreg(DE);
		acu = hreg(AF);
		sum = acu - temp;
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			((cbits >> 8) & 1);
		break;
	case 0x94:			/* SUB H */
		tStates += 4;
		temp = hreg(HL);
		acu = hreg(AF);
		sum = acu - temp;
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			((cbits >> 8) & 1);
		break;
	case 0x95:			/* SUB L */
		tStates += 4;
		temp = lreg(HL);
		acu = hreg(AF);
		sum = acu - temp;
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			((cbits >> 8) & 1);
		break;
	case 0x96:			/* SUB (HL) */
		tStates += 7;
		temp = GetBYTE(HL);
		acu = hreg(AF);
		sum = acu - temp;
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			((cbits >> 8) & 1);
		break;
	case 0x97:			/* SUB A */
		tStates += 4;
		temp = hreg(AF);
		acu = hreg(AF);
		sum = acu - temp;
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			((cbits >> 8) & 1);
		break;
	case 0x98:			/* SBC A,B */
		tStates += 4;
		temp = hreg(BC);
		acu = hreg(AF);
		sum = acu - temp - TSTFLAG(C);
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			((cbits >> 8) & 1);
		break;
	case 0x99:			/* SBC A,C */
		tStates += 4;
		temp = lreg(BC);
		acu = hreg(AF);
		sum = acu - temp - TSTFLAG(C);
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			((cbits >> 8) & 1);
		break;
	case 0x9A:			/* SBC A,D */
		tStates += 4;
		temp = hreg(DE);
		acu = hreg(AF);
		sum = acu - temp - TSTFLAG(C);
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			((cbits >> 8) & 1);
		break;
	case 0x9B:			/* SBC A,E */
		tStates += 4;
		temp = lreg(DE);
		acu = hreg(AF);
		sum = acu - temp - TSTFLAG(C);
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			((cbits >> 8) & 1);
		break;
	case 0x9C:			/* SBC A,H */
		tStates += 4;
		temp = hreg(HL);
		acu = hreg(AF);
		sum = acu - temp - TSTFLAG(C);
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			((cbits >> 8) & 1);
		break;
	case 0x9D:			/* SBC A,L */
		tStates += 4;
		temp = lreg(HL);
		acu = hreg(AF);
		sum = acu - temp - TSTFLAG(C);
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			((cbits >> 8) & 1);
		break;
	case 0x9E:			/* SBC A,(HL) */
		tStates += 7;
		temp = GetBYTE(HL);
		acu = hreg(AF);
		sum = acu - temp - TSTFLAG(C);
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			((cbits >> 8) & 1);
		break;
	case 0x9F:			/* SBC A,A */
		tStates += 4;
		temp = hreg(AF);
		acu = hreg(AF);
		sum = acu - temp - TSTFLAG(C);
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			((cbits >> 8) & 1);
		break;
	case 0xA0:			/* AND B */
		tStates += 4;
		sum = ((AF & (BC)) >> 8) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) |
			((sum == 0) << 6) | 0x10 | partab[sum];
		break;
	case 0xA1:			/* AND C */
		tStates += 4;
		sum = ((AF >> 8) & BC) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | 0x10 |
			((sum == 0) << 6) | partab[sum];
		break;
	case 0xA2:			/* AND D */
		tStates += 4;
		sum = ((AF & (DE)) >> 8) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) |
			((sum == 0) << 6) | 0x10 | partab[sum];
		break;
	case 0xA3:			/* AND E */
		tStates += 4;
		sum = ((AF >> 8) & DE) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | 0x10 |
			((sum == 0) << 6) | partab[sum];
		break;
	case 0xA4:			/* AND H */
		tStates += 4;
		sum = ((AF & (HL)) >> 8) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) |
			((sum == 0) << 6) | 0x10 | partab[sum];
		break;
	case 0xA5:			/* AND L */
		tStates += 4;
		sum = ((AF >> 8) & HL) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | 0x10 |
			((sum == 0) << 6) | partab[sum];
		break;
	case 0xA6:			/* AND (HL) */
		tStates += 7;
		sum = ((AF >> 8) & GetBYTE(HL)) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | 0x10 |
			((sum == 0) << 6) | partab[sum];
		break;
	case 0xA7:			/* AND A */
		tStates += 4;
		sum = ((AF & (AF)) >> 8) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) |
			((sum == 0) << 6) | 0x10 | partab[sum];
		break;
	case 0xA8:			/* XOR B */
		tStates += 4;
		sum = ((AF ^ (BC)) >> 8) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
		break;
	case 0xA9:			/* XOR C */
		tStates += 4;
		sum = ((AF >> 8) ^ BC) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
		break;
	case 0xAA:			/* XOR D */
		tStates += 4;
		sum = ((AF ^ (DE)) >> 8) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
		break;
	case 0xAB:			/* XOR E */
		tStates += 4;
		sum = ((AF >> 8) ^ DE) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
		break;
	case 0xAC:			/* XOR H */
		tStates += 4;
		sum = ((AF ^ (HL)) >> 8) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
		break;
	case 0xAD:			/* XOR L */
		tStates += 4;
		sum = ((AF >> 8) ^ HL) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
		break;
	case 0xAE:			/* XOR (HL) */
		tStates += 7;
		sum = ((AF >> 8) ^ GetBYTE(HL)) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
		break;
	case 0xAF:			/* XOR A */
		tStates += 4;
		sum = ((AF ^ (AF)) >> 8) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
		break;
	case 0xB0:			/* OR B */
		tStates += 4;
		sum = ((AF | (BC)) >> 8) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
		break;
	case 0xB1:			/* OR C */
		tStates += 4;
		sum = ((AF >> 8) | BC) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
		break;
	case 0xB2:			/* OR D */
		tStates += 4;
		sum = ((AF | (DE)) >> 8) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
		break;
	case 0xB3:			/* OR E */
		tStates += 4;
		sum = ((AF >> 8) | DE) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
		break;
	case 0xB4:			/* OR H */
		tStates += 4;
		sum = ((AF | (HL)) >> 8) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
		break;
	case 0xB5:			/* OR L */
		tStates += 4;
		sum = ((AF >> 8) | HL) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
		break;
	case 0xB6:			/* OR (HL) */
		tStates += 7;
		sum = ((AF >> 8) | GetBYTE(HL)) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
		break;
	case 0xB7:			/* OR A */
		tStates += 4;
		sum = ((AF | (AF)) >> 8) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
		break;
	case 0xB8:			/* CP B */
		tStates += 4;
		temp = hreg(BC);
		AF = (AF & ~0x28) | (temp & 0x28);
		acu = hreg(AF);
		sum = acu - temp;
		cbits = acu ^ temp ^ sum;
		AF = (AF & ~0xff) | (sum & 0x80) |
			(((sum & 0xff) == 0) << 6) | (temp & 0x28) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			(cbits & 0x10) | ((cbits >> 8) & 1);
		break;
	case 0xB9:			/* CP C */
		tStates += 4;
		temp = lreg(BC);
		AF = (AF & ~0x28) | (temp & 0x28);
		acu = hreg(AF);
		sum = acu - temp;
		cbits = acu ^ temp ^ sum;
		AF = (AF & ~0xff) | (sum & 0x80) |
			(((sum & 0xff) == 0) << 6) | (temp & 0x28) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			(cbits & 0x10) | ((cbits >> 8) & 1);
		break;
	case 0xBA:			/* CP D */
		tStates += 4;
		temp = hreg(DE);
		AF = (AF & ~0x28) | (temp & 0x28);
		acu = hreg(AF);
		sum = acu - temp;
		cbits = acu ^ temp ^ sum;
		AF = (AF & ~0xff) | (sum & 0x80) |
			(((sum & 0xff) == 0) << 6) | (temp & 0x28) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			(cbits & 0x10) | ((cbits >> 8) & 1);
		break;
	case 0xBB:			/* CP E */
		tStates += 4;
		temp = lreg(DE);
		AF = (AF & ~0x28) | (temp & 0x28);
		acu = hreg(AF);
		sum = acu - temp;
		cbits = acu ^ temp ^ sum;
		AF = (AF & ~0xff) | (sum & 0x80) |
			(((sum & 0xff) == 0) << 6) | (temp & 0x28) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			(cbits & 0x10) | ((cbits >> 8) & 1);
		break;
	case 0xBC:			/* CP H */
		tStates += 4;
		temp = hreg(HL);
		AF = (AF & ~0x28) | (temp & 0x28);
		acu = hreg(AF);
		sum = acu - temp;
		cbits = acu ^ temp ^ sum;
		AF = (AF & ~0xff) | (sum & 0x80) |
			(((sum & 0xff) == 0) << 6) | (temp & 0x28) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			(cbits & 0x10) | ((cbits >> 8) & 1);
		break;
	case 0xBD:			/* CP L */
		tStates += 4;
		temp = lreg(HL);
		AF = (AF & ~0x28) | (temp & 0x28);
		acu = hreg(AF);
		sum = acu - temp;
		cbits = acu ^ temp ^ sum;
		AF = (AF & ~0xff) | (sum & 0x80) |
			(((sum & 0xff) == 0) << 6) | (temp & 0x28) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			(cbits & 0x10) | ((cbits >> 8) & 1);
		break;
	case 0xBE:			/* CP (HL) */
		tStates += 7;
		temp = GetBYTE(HL);
		AF = (AF & ~0x28) | (temp & 0x28);
		acu = hreg(AF);
		sum = acu - temp;
		cbits = acu ^ temp ^ sum;
		AF = (AF & ~0xff) | (sum & 0x80) |
			(((sum & 0xff) == 0) << 6) | (temp & 0x28) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			(cbits & 0x10) | ((cbits >> 8) & 1);
		break;
	case 0xBF:			/* CP A */
		tStates += 4;
		temp = hreg(AF);
		AF = (AF & ~0x28) | (temp & 0x28);
		acu = hreg(AF);
		sum = acu - temp;
		cbits = acu ^ temp ^ sum;
		AF = (AF & ~0xff) | (sum & 0x80) |
			(((sum & 0xff) == 0) << 6) | (temp & 0x28) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			(cbits & 0x10) | ((cbits >> 8) & 1);
		break;
	case 0xC0:			/* RET NZ */
		if (!TSTFLAG(Z)) {
		    POP(PC);
		    tStates += 11;
		} else {
		    tStates += 5;
		}
		break;
	case 0xC1:			/* POP BC */
		tStates += 10;
		POP(BC);
		break;
	case 0xC2:			/* JP NZ,nnnn */
		JPC(!TSTFLAG(Z));	/* also updates tStates */
		break;
	case 0xC3:			/* JP nnnn */
		JPC(1);			/* also updates tStates */
		break;
	case 0xC4:			/* CALL NZ,nnnn */
		CALLC(!TSTFLAG(Z));	/* also updates tStates */
		break;
	case 0xC5:			/* PUSH BC */
		tStates += 11;
		PUSH(BC);
		break;
	case 0xC6:			/* ADD A,nn */
		tStates += 7;
		temp = GetBYTE_pp(PC);
		acu = hreg(AF);
		sum = acu + temp;
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) |
			((cbits >> 8) & 1);
		break;
	case 0xC7:			/* RST 0 */
		tStates += 11;
		PUSH(PC); PC = 0;
		break;
	case 0xC8:			/* RET Z */
		if (TSTFLAG(Z)) {
		    tStates += 11;
		    POP(PC);
		} else {
		    tStates += 5;
		}
		break;
	case 0xC9:			/* RET */
		tStates += 10;
		POP(PC);
		break;
	case 0xCA:			/* JP Z,nnnn */
		JPC(TSTFLAG(Z));	/* also updates tStates */
		break;
	case 0xCB:			/* CB prefix */
		adr = HL;
		switch ((op = GetBYTE(PC)) & 7) {
		    /*
		     * Use default to supress compiler warning: "warning:
		     * 'acu' may be used uninitialized in this function"
		     */
		default:
		case 0: ++PC; acu = hreg(BC); tStates += 8;
			tStateModifier = false;
			break;
		case 1: ++PC; acu = lreg(BC); tStates += 8;
			tStateModifier = false;
			break;
		case 2: ++PC; acu = hreg(DE); tStates += 8;
			tStateModifier = false;
			break;
		case 3: ++PC; acu = lreg(DE); tStates += 8;
			tStateModifier = false;
			break;
		case 4: ++PC; acu = hreg(HL); tStates += 8;
			tStateModifier = false;
			break;
		case 5: ++PC; acu = lreg(HL); tStates += 8;
			tStateModifier = false;
			break;
		case 6: ++PC; acu = GetBYTE(adr); tStates += 15;
			tStateModifier = true;
			break;
		case 7: ++PC; acu = hreg(AF); tStates += 8;
			tStateModifier = false;
			break;
		}
		switch (op & 0xc0) {
		    /*
		     * Use default to supress compiler warning: "warning:
		     * 'temp' may be used uninitialized in this function"
		     */
		default:
		case 0x00:		/* shift/rotate */
			switch (op & 0x38) {
			/*
			 * Use default to supress compiler warning: "warning:
			 * 'temp' may be used uninitialized in this function"
			 */
			default:
			case 0x00:	/* RLC */
				temp = (acu << 1) | (acu >> 7);
				cbits = temp & 1;
				goto cbshflg1;
			case 0x08:	/* RRC */
				temp = (acu >> 1) | (acu << 7);
				cbits = temp & 0x80;
				goto cbshflg1;
			case 0x10:	/* RL */
				temp = (acu << 1) | TSTFLAG(C);
				cbits = acu & 0x80;
				goto cbshflg1;
			case 0x18:	/* RR */
				temp = (acu >> 1) | (TSTFLAG(C) << 7);
				cbits = acu & 1;
				goto cbshflg1;
			case 0x20:	/* SLA */
				temp = acu << 1;
				cbits = acu & 0x80;
				goto cbshflg1;
			case 0x28:	/* SRA */
				temp = (acu >> 1) | (acu & 0x80);
				cbits = acu & 1;
				goto cbshflg1;
			case 0x30:	/* SLIA */
				temp = (acu << 1) | 1;
				cbits = acu & 0x80;
				goto cbshflg1;
			case 0x38:	/* SRL */
				temp = acu >> 1;
				cbits = acu & 1;
			cbshflg1:
				AF = (AF & ~0xff) | (temp & 0xa8) |
					(((temp & 0xff) == 0) << 6) |
					parity(temp) | !!cbits;
			}
			break;
		case 0x40:		/* BIT */
			if (tStateModifier)
				tStates -= 3;
			if (acu & (1 << ((op >> 3) & 7)))
				AF = (AF & ~0xfe) | 0x10 |
				(((op & 0x38) == 0x38) << 7);
			else
				AF = (AF & ~0xfe) | 0x54;
			if ((op&7) != 6)
				AF |= (acu & 0x28);
			temp = acu;
			break;
		case 0x80:		/* RES */
			temp = acu & ~(1 << ((op >> 3) & 7));
			break;
		case 0xc0:		/* SET */
			temp = acu | (1 << ((op >> 3) & 7));
			break;
		}
		switch (op & 7) {
		case 0: Sethreg(BC, temp); break;
		case 1: Setlreg(BC, temp); break;
		case 2: Sethreg(DE, temp); break;
		case 3: Setlreg(DE, temp); break;
		case 4: Sethreg(HL, temp); break;
		case 5: Setlreg(HL, temp); break;
		case 6: PutBYTE(adr, temp);  break;
		case 7: Sethreg(AF, temp); break;
		}
		break;
	case 0xCC:			/* CALL Z,nnnn */
		CALLC(TSTFLAG(Z));	/* also updates tStates */
		break;
	case 0xCD:			/* CALL nnnn */
		CALLC(1);		/* also updates tStates */
		break;
	case 0xCE:			/* ADC A,nn */
		tStates += 7;
		temp = GetBYTE_pp(PC);
		acu = hreg(AF);
		sum = acu + temp + TSTFLAG(C);
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) |
			((cbits >> 8) & 1);
		break;
	case 0xCF:			/* RST 8 */
		tStates += 11;
		PUSH(PC);
		PC = 8;
		break;
	case 0xD0:			/* RET NC */
		if (!TSTFLAG(C)) {
		    POP(PC);
		    tStates += 11;
		} else {
		    tStates += 5;
		}
		break;
	case 0xD1:			/* POP DE */
		tStates += 10;
		POP(DE);
		break;
	case 0xD2:			/* JP NC,nnnn */
		JPC(!TSTFLAG(C));	/* also updates tStates */
		break;
	case 0xD3:			/* OUT (nn),A */
		tStates += 11;
		Output(GetBYTE_pp(PC), hreg(AF));
		break;
	case 0xD4:			/* CALL NC,nnnn */
		CALLC(!TSTFLAG(C));	/* also updates tStates */
		break;
	case 0xD5:			/* PUSH DE */
		tStates += 11;
		PUSH(DE);
		break;
	case 0xD6:			/* SUB nn */
		tStates += 7;
		temp = GetBYTE_pp(PC);
		acu = hreg(AF);
		sum = acu - temp;
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			((cbits >> 8) & 1);
		break;
	case 0xD7:			/* RST 10H */
		tStates += 11;
		PUSH(PC); PC = 0x10;
		break;
	case 0xD8:			/* RET C */
		if (TSTFLAG(C)) {
		    POP(PC);
		    tStates += 11;
		} else {
		    tStates += 5;
		}
		break;
	case 0xD9:			/* EXX */
		tStates += 4;
		regs[regs_sel].bc = BC;
		regs[regs_sel].de = DE;
		regs[regs_sel].hl = HL;
		regs_sel = 1 - regs_sel;
		BC = regs[regs_sel].bc;
		DE = regs[regs_sel].de;
		HL = regs[regs_sel].hl;
		break;
	case 0xDA:			/* JP C,nnnn */
		JPC(TSTFLAG(C));	/* also updates tStates */
		break;
	case 0xDB:			/* IN A,(nn) */
		tStates += 11;
		Sethreg(AF, Input(GetBYTE_pp(PC)));
		break;
	case 0xDC:			/* CALL C,nnnn */
		CALLC(TSTFLAG(C));	/* also updates tStates */
		break;
	case 0xDD:			/* DD prefix */
		switch (op = GetBYTE_pp(PC)) {
		case 0x09:			/* ADD IX,BC */
			tStates += 15;
			IX &= 0xffff;
			BC &= 0xffff;
			sum = IX + BC;
			cbits = (IX ^ BC ^ sum) >> 8;
			IX = sum;
			AF = (AF & ~0x3b) | ((sum >> 8) & 0x28) |
				(cbits & 0x10) | ((cbits >> 8) & 1);
			break;
		case 0x19:			/* ADD IX,DE */
			tStates += 15;
			IX &= 0xffff;
			DE &= 0xffff;
			sum = IX + DE;
			cbits = (IX ^ DE ^ sum) >> 8;
			IX = sum;
			AF = (AF & ~0x3b) | ((sum >> 8) & 0x28) |
				(cbits & 0x10) | ((cbits >> 8) & 1);
			break;
		case 0x21:			/* LD IX,nnnn */
			tStates += 14;
			IX = GetWORD(PC);
			PC += 2;
			break;
		case 0x22:			/* LD (nnnn),IX */
			tStates += 20;
			temp = GetWORD(PC);
			PutWORD(temp, IX);
			PC += 2;
			break;
		case 0x23:			/* INC IX */
			tStates += 10;
			++IX;
			break;
		case 0x24:			/* INC IXH */
			tStates += 9;
			IX += 0x100;
			temp = hreg(IX);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				(((temp & 0xf) == 0) << 4) |
				((temp == 0x80) << 2);
			break;
		case 0x25:			/* DEC IXH */
			tStates += 9;
			IX -= 0x100;
			temp = hreg(IX);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				(((temp & 0xf) == 0xf) << 4) |
				((temp == 0x7f) << 2) | 2;
			break;
		case 0x26:			/* LD IXH,nn */
			tStates += 9;
			Sethreg(IX, GetBYTE_pp(PC));
			break;
		case 0x29:			/* ADD IX,IX */
			tStates += 15;
			IX &= 0xffff;
			sum = IX + IX;
			cbits = (IX ^ IX ^ sum) >> 8;
			IX = sum;
			AF = (AF & ~0x3b) | ((sum >> 8) & 0x28) |
				(cbits & 0x10) | ((cbits >> 8) & 1);
			break;
		case 0x2A:			/* LD IX,(nnnn) */
			tStates += 20;
			temp = GetWORD(PC);
			IX = GetWORD(temp);
			PC += 2;
			break;
		case 0x2B:			/* DEC IX */
			tStates += 10;
			--IX;
			break;
		case 0x2C:			/* INC IXL */
			tStates += 9;
			temp = lreg(IX)+1;
			Setlreg(IX, temp);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				(((temp & 0xf) == 0) << 4) |
				((temp == 0x80) << 2);
			break;
		case 0x2D:			/* DEC IXL */
			tStates += 9;
			temp = lreg(IX)-1;
			Setlreg(IX, temp);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				(((temp & 0xf) == 0xf) << 4) |
				((temp == 0x7f) << 2) | 2;
			break;
		case 0x2E:			/* LD IXL,nn */
			tStates += 9;
			Setlreg(IX, GetBYTE_pp(PC));
			break;
		case 0x34:			/* INC (IX+dd) */
			tStates += 23;
			adr = IX + (signed char) GetBYTE_pp(PC);
			temp = GetBYTE(adr)+1;
			PutBYTE(adr, temp);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				(((temp & 0xf) == 0) << 4) |
				((temp == 0x80) << 2);
			break;
		case 0x35:			/* DEC (IX+dd) */
			tStates += 23;
			adr = IX + (signed char) GetBYTE_pp(PC);
			temp = GetBYTE(adr)-1;
			PutBYTE(adr, temp);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				(((temp & 0xf) == 0xf) << 4) |
				((temp == 0x7f) << 2) | 2;
			break;
		case 0x36:			/* LD (IX+dd),nn */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			PutBYTE(adr, GetBYTE_pp(PC));
			break;
		case 0x39:			/* ADD IX,SP */
			tStates += 15;
			IX &= 0xffff;
			SP &= 0xffff;
			sum = IX + SP;
			cbits = (IX ^ SP ^ sum) >> 8;
			IX = sum;
			AF = (AF & ~0x3b) | ((sum >> 8) & 0x28) |
				(cbits & 0x10) | ((cbits >> 8) & 1);
			break;
		case 0x44:			/* LD B,IXH */
			tStates += 9;
			Sethreg(BC, hreg(IX));
			break;
		case 0x45:			/* LD B,IXL */
			tStates += 9;
			Sethreg(BC, lreg(IX));
			break;
		case 0x46:			/* LD B,(IX+dd) */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			Sethreg(BC, GetBYTE(adr));
			break;
		case 0x4C:			/* LD C,IXH */
			tStates += 9;
			Setlreg(BC, hreg(IX));
			break;
		case 0x4D:			/* LD C,IXL */
			tStates += 9;
			Setlreg(BC, lreg(IX));
			break;
		case 0x4E:			/* LD C,(IX+dd) */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			Setlreg(BC, GetBYTE(adr));
			break;
		case 0x54:			/* LD D,IXH */
			tStates += 9;
			Sethreg(DE, hreg(IX));
			break;
		case 0x55:			/* LD D,IXL */
			tStates += 9;
			Sethreg(DE, lreg(IX));
			break;
		case 0x56:			/* LD D,(IX+dd) */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			Sethreg(DE, GetBYTE(adr));
			break;
		case 0x5C:			/* LD E,IXH */
			tStates += 9;
			Setlreg(DE, hreg(IX));
			break;
		case 0x5D:			/* LD E,IXL */
			tStates += 9;
			Setlreg(DE, lreg(IX));
			break;
		case 0x5E:			/* LD E,(IX+dd) */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			Setlreg(DE, GetBYTE(adr));
			break;
		case 0x60:			/* LD IXH,B */
			tStates += 9;
			Sethreg(IX, hreg(BC));
			break;
		case 0x61:			/* LD IXH,C */
			tStates += 9;
			Sethreg(IX, lreg(BC));
			break;
		case 0x62:			/* LD IXH,D */
			tStates += 9;
			Sethreg(IX, hreg(DE));
			break;
		case 0x63:			/* LD IXH,E */
			tStates += 9;
			Sethreg(IX, lreg(DE));
			break;
		case 0x64:			/* LD IXH,IXH */
			tStates += 9;
			/* nop */
			break;
		case 0x65:			/* LD IXH,IXL */
			tStates += 9;
			Sethreg(IX, lreg(IX));
			break;
		case 0x66:			/* LD H,(IX+dd) */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			Sethreg(HL, GetBYTE(adr));
			break;
		case 0x67:			/* LD IXH,A */
			tStates += 9;
			Sethreg(IX, hreg(AF));
			break;
		case 0x68:			/* LD IXL,B */
			tStates += 9;
			Setlreg(IX, hreg(BC));
			break;
		case 0x69:			/* LD IXL,C */
			tStates += 9;
			Setlreg(IX, lreg(BC));
			break;
		case 0x6A:			/* LD IXL,D */
			tStates += 9;
			Setlreg(IX, hreg(DE));
			break;
		case 0x6B:			/* LD IXL,E */
			tStates += 9;
			Setlreg(IX, lreg(DE));
			break;
		case 0x6C:			/* LD IXL,IXH */
			tStates += 9;
			Setlreg(IX, hreg(IX));
			break;
		case 0x6D:			/* LD IXL,IXL */
			tStates += 9;
			/* nop */
			break;
		case 0x6E:			/* LD L,(IX+dd) */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			Setlreg(HL, GetBYTE(adr));
			break;
		case 0x6F:			/* LD IXL,A */
			tStates += 9;
			Setlreg(IX, hreg(AF));
			break;
		case 0x70:			/* LD (IX+dd),B */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			PutBYTE(adr, hreg(BC));
			break;
		case 0x71:			/* LD (IX+dd),C */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			PutBYTE(adr, lreg(BC));
			break;
		case 0x72:			/* LD (IX+dd),D */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			PutBYTE(adr, hreg(DE));
			break;
		case 0x73:			/* LD (IX+dd),E */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			PutBYTE(adr, lreg(DE));
			break;
		case 0x74:			/* LD (IX+dd),H */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			PutBYTE(adr, hreg(HL));
			break;
		case 0x75:			/* LD (IX+dd),L */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			PutBYTE(adr, lreg(HL));
			break;
		case 0x77:			/* LD (IX+dd),A */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			PutBYTE(adr, hreg(AF));
			break;
		case 0x7C:			/* LD A,IXH */
			tStates += 9;
			Sethreg(AF, hreg(IX));
			break;
		case 0x7D:			/* LD A,IXL */
			tStates += 9;
			Sethreg(AF, lreg(IX));
			break;
		case 0x7E:			/* LD A,(IX+dd) */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			Sethreg(AF, GetBYTE(adr));
			break;
		case 0x84:			/* ADD A,IXH */
			tStates += 9;
			temp = hreg(IX);
			acu = hreg(AF);
			sum = acu + temp;
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				((cbits >> 8) & 1);
			break;
		case 0x85:			/* ADD A,IXL */
			tStates += 9;
			temp = lreg(IX);
			acu = hreg(AF);
			sum = acu + temp;
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				((cbits >> 8) & 1);
			break;
		case 0x86:			/* ADD A,(IX+dd) */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			temp = GetBYTE(adr);
			acu = hreg(AF);
			sum = acu + temp;
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				((cbits >> 8) & 1);
			break;
		case 0x8C:			/* ADC A,IXH */
			tStates += 9;
			temp = hreg(IX);
			acu = hreg(AF);
			sum = acu + temp + TSTFLAG(C);
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				((cbits >> 8) & 1);
			break;
		case 0x8D:			/* ADC A,IXL */
			tStates += 9;
			temp = lreg(IX);
			acu = hreg(AF);
			sum = acu + temp + TSTFLAG(C);
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				((cbits >> 8) & 1);
			break;
		case 0x8E:			/* ADC A,(IX+dd) */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			temp = GetBYTE(adr);
			acu = hreg(AF);
			sum = acu + temp + TSTFLAG(C);
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				((cbits >> 8) & 1);
			break;
		case 0x94:			/* SUB IXH */
			tStates += 9;
			temp = hreg(IX);
			acu = hreg(AF);
			sum = acu - temp;
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
				((cbits >> 8) & 1);
			break;
		case 0x95:			/* SUB IXL */
			tStates += 9;
			temp = lreg(IX);
			acu = hreg(AF);
			sum = acu - temp;
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
				((cbits >> 8) & 1);
			break;
		case 0x96:			/* SUB (IX+dd) */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			temp = GetBYTE(adr);
			acu = hreg(AF);
			sum = acu - temp;
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
				((cbits >> 8) & 1);
			break;
		case 0x9C:			/* SBC A,IXH */
			tStates += 9;
			temp = hreg(IX);
			acu = hreg(AF);
			sum = acu - temp - TSTFLAG(C);
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
				((cbits >> 8) & 1);
			break;
		case 0x9D:			/* SBC A,IXL */
			tStates += 9;
			temp = lreg(IX);
			acu = hreg(AF);
			sum = acu - temp - TSTFLAG(C);
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
				((cbits >> 8) & 1);
			break;
		case 0x9E:			/* SBC A,(IX+dd) */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			temp = GetBYTE(adr);
			acu = hreg(AF);
			sum = acu - temp - TSTFLAG(C);
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
				((cbits >> 8) & 1);
			break;
		case 0xA4:			/* AND IXH */
			tStates += 9;
			sum = ((AF & (IX)) >> 8) & 0xff;
			AF = (sum << 8) | (sum & 0xa8) |
				((sum == 0) << 6) | 0x10 | partab[sum];
			break;
		case 0xA5:			/* AND IXL */
			tStates += 9;
			sum = ((AF >> 8) & IX) & 0xff;
			AF = (sum << 8) | (sum & 0xa8) | 0x10 |
				((sum == 0) << 6) | partab[sum];
			break;
		case 0xA6:			/* AND (IX+dd) */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			sum = ((AF >> 8) & GetBYTE(adr)) & 0xff;
			AF = (sum << 8) | (sum & 0xa8) | 0x10 |
				((sum == 0) << 6) | partab[sum];
			break;
		case 0xAC:			/* XOR IXH */
			tStates += 9;
			sum = ((AF ^ (IX)) >> 8) & 0xff;
			AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
			break;
		case 0xAD:			/* XOR IXL */
			tStates += 9;
			sum = ((AF >> 8) ^ IX) & 0xff;
			AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
			break;
		case 0xAE:			/* XOR (IX+dd) */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			sum = ((AF >> 8) ^ GetBYTE(adr)) & 0xff;
			AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
			break;
		case 0xB4:			/* OR IXH */
			tStates += 9;
			sum = ((AF | (IX)) >> 8) & 0xff;
			AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
			break;
		case 0xB5:			/* OR IXL */
			tStates += 9;
			sum = ((AF >> 8) | IX) & 0xff;
			AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
			break;
		case 0xB6:			/* OR (IX+dd) */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			sum = ((AF >> 8) | GetBYTE(adr)) & 0xff;
			AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
			break;
		case 0xBC:			/* CP IXH */
			tStates += 9;
			temp = hreg(IX);
			AF = (AF & ~0x28) | (temp & 0x28);
			acu = hreg(AF);
			sum = acu - temp;
			cbits = acu ^ temp ^ sum;
			AF = (AF & ~0xff) | (sum & 0x80) |
				(((sum & 0xff) == 0) << 6) | (temp & 0x28) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
				(cbits & 0x10) | ((cbits >> 8) & 1);
			break;
		case 0xBD:			/* CP IXL */
			tStates += 9;
			temp = lreg(IX);
			AF = (AF & ~0x28) | (temp & 0x28);
			acu = hreg(AF);
			sum = acu - temp;
			cbits = acu ^ temp ^ sum;
			AF = (AF & ~0xff) | (sum & 0x80) |
				(((sum & 0xff) == 0) << 6) | (temp & 0x28) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
				(cbits & 0x10) | ((cbits >> 8) & 1);
			break;
		case 0xBE:			/* CP (IX+dd) */
			tStates += 19;
			adr = IX + (signed char) GetBYTE_pp(PC);
			temp = GetBYTE(adr);
			AF = (AF & ~0x28) | (temp & 0x28);
			acu = hreg(AF);
			sum = acu - temp;
			cbits = acu ^ temp ^ sum;
			AF = (AF & ~0xff) | (sum & 0x80) |
				(((sum & 0xff) == 0) << 6) | (temp & 0x28) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
				(cbits & 0x10) | ((cbits >> 8) & 1);
			break;
		case 0xCB:			/* CB prefix */
			adr = IX + (signed char) GetBYTE_pp(PC);
			switch ((op = GetBYTE(PC)) & 7) {
			    /*
			     * default: supresses compiler warning: "warning:
			     * 'acu' may be used uninitialized in this function"
			     */
			default:
			case 0: ++PC; acu = hreg(BC); break;
			case 1: ++PC; acu = lreg(BC); break;
			case 2: ++PC; acu = hreg(DE); break;
			case 3: ++PC; acu = lreg(DE); break;
			case 4: ++PC; acu = hreg(HL); break;
			case 5: ++PC; acu = lreg(HL); break;
			case 6: ++PC; acu = GetBYTE(adr);  break;
			case 7: ++PC; acu = hreg(AF); break;
			}
			switch (op & 0xc0) {
			/*
			 * Use default to supress compiler warning: "warning:
			 * 'temp' may be used uninitialized in this function"
			 */
			default:
			case 0x00:		/* shift/rotate */
				switch (op & 0x38) {
				/*
				 * Use default: to supress compiler warning
				 * about 'temp' being used uninitialized
				 */
				default:
				case 0x00:	/* RLC */
					temp = (acu << 1) | (acu >> 7);
					cbits = temp & 1;
					goto cbshflg2;
				case 0x08:	/* RRC */
					temp = (acu >> 1) | (acu << 7);
					cbits = temp & 0x80;
					goto cbshflg2;
				case 0x10:	/* RL */
					temp = (acu << 1) | TSTFLAG(C);
					cbits = acu & 0x80;
					goto cbshflg2;
				case 0x18:	/* RR */
					temp = (acu >> 1) | (TSTFLAG(C) << 7);
					cbits = acu & 1;
					goto cbshflg2;
				case 0x20:	/* SLA */
					temp = acu << 1;
					cbits = acu & 0x80;
					goto cbshflg2;
				case 0x28:	/* SRA */
					temp = (acu >> 1) | (acu & 0x80);
					cbits = acu & 1;
					goto cbshflg2;
				case 0x30:	/* SLIA */
					temp = (acu << 1) | 1;
					cbits = acu & 0x80;
					goto cbshflg2;
				case 0x38:	/* SRL */
					temp = acu >> 1;
					cbits = acu & 1;
				cbshflg2:
					AF = (AF & ~0xff) | (temp & 0xa8) |
						(((temp & 0xff) == 0) << 6) |
						parity(temp) | !!cbits;
				}
				break;
			case 0x40:		/* BIT */
				tStates += 20;
				if (acu & (1 << ((op >> 3) & 7)))
					AF = (AF & ~0xfe) | 0x10 |
					(((op & 0x38) == 0x38) << 7);
				else
					AF = (AF & ~0xfe) | 0x54;
				if ((op&7) != 6)
					AF |= (acu & 0x28);
				temp = acu;
				break;
			case 0x80:		/* RES */
				tStates += 23;
				temp = acu & ~(1 << ((op >> 3) & 7));
				break;
			case 0xc0:		/* SET */
				tStates += 23;
				temp = acu | (1 << ((op >> 3) & 7));
				break;
			}
			switch (op & 7) {
			case 0: Sethreg(BC, temp); break;
			case 1: Setlreg(BC, temp); break;
			case 2: Sethreg(DE, temp); break;
			case 3: Setlreg(DE, temp); break;
			case 4: Sethreg(HL, temp); break;
			case 5: Setlreg(HL, temp); break;
			case 6: PutBYTE(adr, temp);  break;
			case 7: Sethreg(AF, temp); break;
			}
			break;
		case 0xE1:			/* POP IX */
			tStates += 14;
			POP(IX);
			break;
		case 0xE3:			/* EX (SP),IX */
			tStates += 23;
			temp = IX; POP(IX); PUSH(temp);
			break;
		case 0xE5:			/* PUSH IX */
			tStates += 15;
			PUSH(IX);
			break;
		case 0xE9:			/* JP (IX) */
			tStates += 8;
			PC = IX;
			break;
		case 0xF9:			/* LD SP,IX */
			tStates += 10;
			SP = IX;
			break;
		default: PC--;		/* ignore DD */
		}
		break;
	case 0xDE:			/* SBC A,nn */
		tStates += 7;
		temp = GetBYTE_pp(PC);
		acu = hreg(AF);
		sum = acu - temp - TSTFLAG(C);
		cbits = acu ^ temp ^ sum;
		AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
			(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			((cbits >> 8) & 1);
		break;
	case 0xDF:			/* RST 18H */
		tStates += 11;
		PUSH(PC); PC = 0x18;
		break;
	case 0xE0:			/* RET PO */
		if (!TSTFLAG(P)) {
		    POP(PC);
		    tStates += 11;
		} else {
		    tStates += 5;
		}
		break;
	case 0xE1:			/* POP HL */
		tStates += 10;
		POP(HL);
		break;
	case 0xE2:			/* JP PO,nnnn */
		JPC(!TSTFLAG(P));	/* also updates tStates */
		break;
	case 0xE3:			/* EX (SP),HL */
		tStates += 19;
		temp = HL; POP(HL); PUSH(temp);
		break;
	case 0xE4:			/* CALL PO,nnnn */
		CALLC(!TSTFLAG(P));	/* also updates tStates */
		break;
	case 0xE5:			/* PUSH HL */
		tStates += 11;
		PUSH(HL);
		break;
	case 0xE6:			/* AND nn */
		tStates += 7;
		sum = ((AF >> 8) & GetBYTE_pp(PC)) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | 0x10 |
			((sum == 0) << 6) | partab[sum];
		break;
	case 0xE7:			/* RST 20H */
		tStates += 11;
		PUSH(PC); PC = 0x20;
		break;
	case 0xE8:			/* RET PE */
		if (TSTFLAG(P)) {
		    POP(PC);
		    tStates += 11;
		} else {
		    tStates += 5;
		}
		break;
	case 0xE9:			/* JP (HL) */
		tStates += 4;
		PC = HL;
		break;
	case 0xEA:			/* JP PE,nnnn */
		JPC(TSTFLAG(P));	/* also updates tStates */
		break;
	case 0xEB:			/* EX DE,HL */
		tStates += 4;
		temp = HL; HL = DE; DE = temp;
		break;
	case 0xEC:			/* CALL PE,nnnn */
		CALLC(TSTFLAG(P));	/* also updates tStates */
		break;
	case 0xED:			/* ED prefix */
		switch (op = GetBYTE_pp(PC)) {
		case 0x40:			/* IN B,(C) */
			tStates += 12;
			temp = Input(lreg(BC));
			Sethreg(BC, temp);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				parity(temp);
			break;
		case 0x41:			/* OUT (C),B */
			tStates += 12;
			Output(lreg(BC), hreg(BC));
			break;
		case 0x42:			/* SBC HL,BC */
			tStates += 15;
			HL &= 0xffff;
			BC &= 0xffff;
			sum = HL - BC - TSTFLAG(C);
			cbits = (HL ^ BC ^ sum) >> 8;
			HL = sum;
			AF = (AF & ~0xff) | ((sum >> 8) & 0xa8) |
				(((sum & 0xffff) == 0) << 6) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				(cbits & 0x10) | 2 | ((cbits >> 8) & 1);
			break;
		case 0x43:			/* LD (nnnn),BC */
			tStates += 20;
			temp = GetWORD(PC);
			PutWORD(temp, BC);
			PC += 2;
			break;
		case 0x44:			/* NEG */
			temp = hreg(AF);
			AF = (-(AF & 0xff00) & 0xff00);
			AF |= ((AF >> 8) & 0xa8) | (((AF & 0xff00) == 0) << 6) |
				(((temp & 0x0f) != 0) << 4) | ((temp == 0x80) << 2) |
				2 | (temp != 0);
			break;
		case 0x45:			/* RETN */
			tStates += 14;
			IFF |= IFF >> 1;
			POP(PC);
			break;
		case 0x46:			/* IM 0 */
			tStates += 8;
			/* interrupt mode 0 */
			break;
		case 0x47:			/* LD I,A */
			tStates += 9;
			ir = (ir & 255) | (AF & ~255);
			break;
		case 0x48:			/* IN C,(C) */
			tStates += 12;
			temp = Input(lreg(BC));
			Setlreg(BC, temp);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				parity(temp);
			break;
		case 0x49:			/* OUT (C),C */
			tStates += 12;
			Output(lreg(BC), BC);
			break;
		case 0x4A:			/* ADC HL,BC */
			tStates += 15;
			HL &= 0xffff;
			BC &= 0xffff;
			sum = HL + BC + TSTFLAG(C);
			cbits = (HL ^ BC ^ sum) >> 8;
			HL = sum;
			AF = (AF & ~0xff) | ((sum >> 8) & 0xa8) |
				(((sum & 0xffff) == 0) << 6) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				(cbits & 0x10) | ((cbits >> 8) & 1);
			break;
		case 0x4B:			/* LD BC,(nnnn) */
			tStates += 20;
			temp = GetWORD(PC);
			BC = GetWORD(temp);
			PC += 2;
			break;
		case 0x4D:			/* RETI */
			tStates += 14;
			IFF |= IFF >> 1;
			POP(PC);
			break;
		case 0x4F:			/* LD R,A */
			tStates += 9;
			ir = (ir & ~255) | ((AF >> 8) & 255);
			break;
		case 0x50:			/* IN D,(C) */
			tStates += 12;
			temp = Input(lreg(BC));
			Sethreg(DE, temp);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				parity(temp);
			break;
		case 0x51:			/* OUT (C),D */
			tStates += 12;
			Output(lreg(BC), hreg(DE));
			break;
		case 0x52:			/* SBC HL,DE */
			tStates += 15;
			HL &= 0xffff;
			DE &= 0xffff;
			sum = HL - DE - TSTFLAG(C);
			cbits = (HL ^ DE ^ sum) >> 8;
			HL = sum;
			AF = (AF & ~0xff) | ((sum >> 8) & 0xa8) |
				(((sum & 0xffff) == 0) << 6) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				(cbits & 0x10) | 2 | ((cbits >> 8) & 1);
			break;
		case 0x53:			/* LD (nnnn),DE */
			tStates += 20;
			temp = GetWORD(PC);
			PutWORD(temp, DE);
			PC += 2;
			break;
		case 0x56:			/* IM 1 */
			tStates += 8;
			/* interrupt mode 1 */
			break;
		case 0x57:			/* LD A,I */
			tStates += 9;
			AF = (AF & 0x29) | (ir & ~255) | ((ir >> 8) & 0x80) | (((ir & ~255) == 0) << 6) | ((IFF & 2) << 1);
			break;
		case 0x58:			/* IN E,(C) */
			tStates += 12;
			temp = Input(lreg(BC));
			Setlreg(DE, temp);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				parity(temp);
			break;
		case 0x59:			/* OUT (C),E */
			tStates += 12;
			Output(lreg(BC), DE);
			break;
		case 0x5A:			/* ADC HL,DE */
			tStates += 15;
			HL &= 0xffff;
			DE &= 0xffff;
			sum = HL + DE + TSTFLAG(C);
			cbits = (HL ^ DE ^ sum) >> 8;
			HL = sum;
			AF = (AF & ~0xff) | ((sum >> 8) & 0xa8) |
				(((sum & 0xffff) == 0) << 6) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				(cbits & 0x10) | ((cbits >> 8) & 1);
			break;
		case 0x5B:			/* LD DE,(nnnn) */
			tStates += 20;
			temp = GetWORD(PC);
			DE = GetWORD(temp);
			PC += 2;
			break;
		case 0x5E:			/* IM 2 */
			tStates += 8;
			/* interrupt mode 2 */
			break;
		case 0x5F:			/* LD A,R */
			tStates += 9;
			AF = (AF & 0x29) | ((ir & 255) << 8) | (ir & 0x80) | (((ir & 255) == 0) << 6) | ((IFF & 2) << 1);
			break;
		case 0x60:			/* IN H,(C) */
			tStates += 12;
			temp = Input(lreg(BC));
			Sethreg(HL, temp);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				parity(temp);
			break;
		case 0x61:			/* OUT (C),H */
			tStates += 12;
			Output(lreg(BC), hreg(HL));
			break;
		case 0x62:			/* SBC HL,HL */
			tStates += 15;
			HL &= 0xffff;
			sum = HL - HL - TSTFLAG(C);
			cbits = (HL ^ HL ^ sum) >> 8;
			HL = sum;
			AF = (AF & ~0xff) | ((sum >> 8) & 0xa8) |
				(((sum & 0xffff) == 0) << 6) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				(cbits & 0x10) | 2 | ((cbits >> 8) & 1);
			break;
		case 0x63:			/* LD (nnnn),HL */
			tStates += 20;
			temp = GetWORD(PC);
			PutWORD(temp, HL);
			PC += 2;
			break;
		case 0x67:			/* RRD */
			tStates += 18;
			temp = GetBYTE(HL);
			acu = hreg(AF);
			PutBYTE(HL, hdig(temp) | (ldig(acu) << 4));
			acu = (acu & 0xf0) | ldig(temp);
			AF = (acu << 8) | (acu & 0xa8) | (((acu & 0xff) == 0) << 6) |
				partab[acu] | (AF & 1);
			break;
		case 0x68:			/* IN L,(C) */
			tStates += 12;
			temp = Input(lreg(BC));
			Setlreg(HL, temp);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				parity(temp);
			break;
		case 0x69:			/* OUT (C),L */
			tStates += 12;
			Output(lreg(BC), lreg(HL));
			break;
		case 0x6A:			/* ADC HL,HL */
			tStates += 15;
			HL &= 0xffff;
			sum = HL + HL + TSTFLAG(C);
			cbits = (HL ^ HL ^ sum) >> 8;
			HL = sum;
			AF = (AF & ~0xff) | ((sum >> 8) & 0xa8) |
				(((sum & 0xffff) == 0) << 6) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				(cbits & 0x10) | ((cbits >> 8) & 1);
			break;
		case 0x6B:			/* LD HL,(nnnn) */
			tStates += 20;
			temp = GetWORD(PC);
			HL = GetWORD(temp);
			PC += 2;
			break;
		case 0x6F:			/* RLD */
			tStates += 18;
			temp = GetBYTE(HL);
			acu = hreg(AF);
			PutBYTE(HL, (ldig(temp) << 4) | ldig(acu));
			acu = (acu & 0xf0) | hdig(temp);
			AF = (acu << 8) | (acu & 0xa8) | (((acu & 0xff) == 0) << 6) |
				partab[acu] | (AF & 1);
			break;
		case 0x70:			/* IN (C) */
			tStates += 12;
			temp = Input(lreg(BC));
			Setlreg(temp, temp);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				parity(temp);
			break;
		case 0x71:			/* OUT (C),0 */
			tStates += 12;
			Output(lreg(BC), 0);
			break;
		case 0x72:			/* SBC HL,SP */
			tStates += 15;
			HL &= 0xffff;
			SP &= 0xffff;
			sum = HL - SP - TSTFLAG(C);
			cbits = (HL ^ SP ^ sum) >> 8;
			HL = sum;
			AF = (AF & ~0xff) | ((sum >> 8) & 0xa8) |
				(((sum & 0xffff) == 0) << 6) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				(cbits & 0x10) | 2 | ((cbits >> 8) & 1);
			break;
		case 0x73:			/* LD (nnnn),SP */
			tStates += 20;
			temp = GetWORD(PC);
			PutWORD(temp, SP);
			PC += 2;
			break;
		case 0x78:			/* IN A,(C) */
			tStates += 12;
			temp = Input(lreg(BC));
			Sethreg(AF, temp);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				parity(temp);
			break;
		case 0x79:			/* OUT (C),A */
			Output(lreg(BC), hreg(AF));
			break;
		case 0x7A:			/* ADC HL,SP */
			tStates += 15;
			HL &= 0xffff;
			SP &= 0xffff;
			sum = HL + SP + TSTFLAG(C);
			cbits = (HL ^ SP ^ sum) >> 8;
			HL = sum;
			AF = (AF & ~0xff) | ((sum >> 8) & 0xa8) |
				(((sum & 0xffff) == 0) << 6) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				(cbits & 0x10) | ((cbits >> 8) & 1);
			break;
		case 0x7B:			/* LD SP,(nnnn) */
			tStates += 20;
			temp = GetWORD(PC);
			SP = GetWORD(temp);
			PC += 2;
			break;
		case 0xA0:			/* LDI */
			tStates += 16;
			acu = GetBYTE_pp(HL);
			PutBYTE_pp(DE, acu);
			acu += hreg(AF);
			AF = (AF & ~0x3e) | (acu & 8) | ((acu & 2) << 4) |
				(((--BC & 0xffff) != 0) << 2);
			break;
		case 0xA1:			/* CPI */
			tStates += 16;
			acu = hreg(AF);
			temp = GetBYTE_pp(HL);
			sum = acu - temp;
			cbits = acu ^ temp ^ sum;
			AF = (AF & ~0xfe) | (sum & 0x80) | (!(sum & 0xff) << 6) |
				(((sum - ((cbits&16)>>4))&2) << 4) | (cbits & 16) |
				((sum - ((cbits >> 4) & 1)) & 8) |
				((--BC & 0xffff) != 0) << 2 | 2;
			if ((sum & 15) == 8 && (cbits & 16) != 0)
				AF &= ~8;
			break;
		case 0xA2:			/* INI */
			tStates += 16;
			PutBYTE(HL, Input(lreg(BC))); ++HL;
			SETFLAG(N, 1);
			/* SETFLAG(P, (--BC & 0xffff) != 0); deleted */
			Sethreg(BC, lreg(BC) - 1); /* added at 2019-03-23 */
			SETFLAG(Z, lreg(BC) == 0); /* added at 2019-03-23 */
			break;
		case 0xA3:			/* OUTI */
			tStates += 16;
			Output(lreg(BC), GetBYTE(HL)); ++HL;
			SETFLAG(N, 1);
			Sethreg(BC, hreg(BC) - 1);
			SETFLAG(Z, hreg(BC) == 0);
			break;
		case 0xA8:			/* LDD */
			tStates += 16;
			acu = GetBYTE_mm(HL);
			PutBYTE_mm(DE, acu);
			acu += hreg(AF);
			AF = (AF & ~0x3e) | (acu & 8) | ((acu & 2) << 4) |
				(((--BC & 0xffff) != 0) << 2);
			break;
		case 0xA9:			/* CPD */
			tStates += 16;
			acu = hreg(AF);
			temp = GetBYTE_mm(HL);
			sum = acu - temp;
			cbits = acu ^ temp ^ sum;
			AF = (AF & ~0xfe) | (sum & 0x80) | (!(sum & 0xff) << 6) |
				(((sum - ((cbits&16)>>4))&2) << 4) | (cbits & 16) |
				((sum - ((cbits >> 4) & 1)) & 8) |
				((--BC & 0xffff) != 0) << 2 | 2;
			if ((sum & 15) == 8 && (cbits & 16) != 0)
				AF &= ~8;
			break;
		case 0xAA:			/* IND */
			tStates += 16;
			PutBYTE(HL, Input(lreg(BC))); --HL;
			SETFLAG(N, 1);
			Sethreg(BC, lreg(BC) - 1);
			SETFLAG(Z, lreg(BC) == 0);
			break;
		case 0xAB:			/* OUTD */
			tStates += 16;
			Output(lreg(BC), GetBYTE(HL)); --HL;
			SETFLAG(N, 1);
			Sethreg(BC, hreg(BC) - 1);
			SETFLAG(Z, hreg(BC) == 0);
			break;
		case 0xB0:			/* LDIR */
			tStates -= 5;;
			acu = hreg(AF);
			BC &= 0xffff;
			if (BC == 0)
			    BC = 0x10000;
			do {
				tStates += 21;
				acu = GetBYTE_pp(HL);
				PutBYTE_pp(DE, acu);
			} while (--BC);
			acu += hreg(AF);
			AF = (AF & ~0x3e) | (acu & 8) | ((acu & 2) << 4);
			break;
		case 0xB1:			/* CPIR */
			tStates -= 5;
			acu = hreg(AF);
			BC &= 0xffff;
			if (BC == 0)
			    BC = 0x10000;
			do {
				tStates += 21;
				temp = GetBYTE_pp(HL);
				op = --BC != 0;
				sum = acu - temp;
			} while (op && sum != 0);
			cbits = acu ^ temp ^ sum;
			AF = (AF & ~0xfe) | (sum & 0x80) | (!(sum & 0xff) << 6) |
				(((sum - ((cbits&16)>>4))&2) << 4) |
				(cbits & 16) | ((sum - ((cbits >> 4) & 1)) & 8) |
				op << 2 | 2;
			if ((sum & 15) == 8 && (cbits & 16) != 0)
				AF &= ~8;
			break;
		case 0xB2:			/* INIR */
			tStates -= 5;
			temp = hreg(BC);
			do {
				tStates += 21;
				PutBYTE(HL, Input(lreg(BC))); ++HL;
			} while (--temp);
			Sethreg(BC, 0);
			SETFLAG(N, 1);
			SETFLAG(Z, 1);
			break;
		case 0xB3:			/* OTIR */
			tStates -= 5;
			temp = hreg(BC);
			do {
				tStates += 21;
				Output(lreg(BC), GetBYTE(HL)); ++HL;
			} while (--temp);
			Sethreg(BC, 0);
			SETFLAG(N, 1);
			SETFLAG(Z, 1);
			break;
		case 0xB8:			/* LDDR */
			tStates -= 5;
			BC &= 0xffff;
			if (BC == 0)
			    BC = 0x10000;
			do {
				tStates += 21;
				tStates += 21;
				acu = GetBYTE_mm(HL);
				PutBYTE_mm(DE, acu);
			} while (--BC);
			acu += hreg(AF);
			AF = (AF & ~0x3e) | (acu & 8) | ((acu & 2) << 4);
			break;
		case 0xB9:			/* CPDR */
			tStates -= 5;
			acu = hreg(AF);
			BC &= 0xffff;
			if (BC == 0)
			    BC = 0x10000;
			do {
				tStates += 21;
				temp = GetBYTE_mm(HL);
				op = --BC != 0;
				sum = acu - temp;
			} while (op && sum != 0);
			cbits = acu ^ temp ^ sum;
			AF = (AF & ~0xfe) | (sum & 0x80) | (!(sum & 0xff) << 6) |
				(((sum - ((cbits&16)>>4))&2) << 4) |
				(cbits & 16) | ((sum - ((cbits >> 4) & 1)) & 8) |
				op << 2 | 2;
			if ((sum & 15) == 8 && (cbits & 16) != 0)
				AF &= ~8;
			break;
		case 0xBA:			/* INDR */
			tStates -= 5;
			temp = hreg(BC);
			do {
				tStates += 21;
				PutBYTE(HL, Input(lreg(BC))); --HL;
			} while (--temp);
			Sethreg(BC, 0);
			SETFLAG(N, 1);
			SETFLAG(Z, 1);
			break;
		case 0xBB:			/* OTDR */
			tStates -= 5;
			temp = hreg(BC);
			do {
				tStates += 21;
				Output(lreg(BC), GetBYTE(HL));
				--HL;
			} while (--temp);
			Sethreg(BC, 0);
			SETFLAG(N, 1);
			SETFLAG(Z, 1);
			break;
		default: if (0x40 <= op && op <= 0x7f) PC--;		/* ignore ED */
		}
		break;
	case 0xEE:			/* XOR nn */
		tStates += 7;
		sum = ((AF >> 8) ^ GetBYTE_pp(PC)) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
		break;
	case 0xEF:			/* RST 28H */
		tStates += 11;
		PUSH(PC); PC = 0x28;
		break;
	case 0xF0:			/* RET P */
		if (!TSTFLAG(S)) {
		    POP(PC);
		    tStates += 11;
		} else {
		    tStates += 5;
		}
		break;
	case 0xF1:			/* POP AF */
		tStates += 10;
		POP(AF);
		break;
	case 0xF2:			/* JP P,nnnn */
		JPC(!TSTFLAG(S));	/* also updates tStates */
		break;
	case 0xF3:			/* DI */
		tStates += 4;
		IFF = 0;
		break;
	case 0xF4:			/* CALL P,nnnn */
		CALLC(!TSTFLAG(S));	/* also updates tStates */
		break;
	case 0xF5:			/* PUSH AF */
		tStates += 11;
		PUSH(AF);
		break;
	case 0xF6:			/* OR nn */
		tStates += 7;
		sum = ((AF >> 8) | GetBYTE_pp(PC)) & 0xff;
		AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
		break;
	case 0xF7:			/* RST 30H */
		tStates += 11;
		PUSH(PC); PC = 0x30;
		break;
	case 0xF8:			/* RET M */
		if (TSTFLAG(S)) {
		    POP(PC);
		    tStates += 11;
		} else {
		    tStates += 5;
		}
		break;
	case 0xF9:			/* LD SP,HL */
		tStates += 6;
		SP = HL;
		break;
	case 0xFA:			/* JP M,nnnn */
		JPC(TSTFLAG(S));	/* also updates tStates */
		break;
	case 0xFB:			/* EI */
		tStates += 4;
		IFF = 3;
		break;
	case 0xFC:			/* CALL M,nnnn */
		CALLC(TSTFLAG(S));	/* also updates tStates */
		break;
	case 0xFD:			/* FD prefix */
		switch (op = GetBYTE_pp(PC)) {
		case 0x09:			/* ADD IY,BC */
			tStates += 15;
			IY &= 0xffff;
			BC &= 0xffff;
			sum = IY + BC;
			cbits = (IY ^ BC ^ sum) >> 8;
			IY = sum;
			AF = (AF & ~0x3b) | ((sum >> 8) & 0x28) |
				(cbits & 0x10) | ((cbits >> 8) & 1);
			break;
		case 0x19:			/* ADD IY,DE */
			tStates += 15;
			IY &= 0xffff;
			DE &= 0xffff;
			sum = IY + DE;
			cbits = (IY ^ DE ^ sum) >> 8;
			IY = sum;
			AF = (AF & ~0x3b) | ((sum >> 8) & 0x28) |
				(cbits & 0x10) | ((cbits >> 8) & 1);
			break;
		case 0x21:			/* LD IY,nnnn */
			tStates += 14;
			IY = GetWORD(PC);
			PC += 2;
			break;
		case 0x22:			/* LD (nnnn),IY */
			tStates += 20;
			temp = GetWORD(PC);
			PutWORD(temp, IY);
			PC += 2;
			break;
		case 0x23:			/* INC IY */
			tStates += 10;
			++IY;
			break;
		case 0x24:			/* INC IYH */
			tStates += 9;
			IY += 0x100;
			temp = hreg(IY);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				(((temp & 0xf) == 0) << 4) |
				((temp == 0x80) << 2);
			break;
		case 0x25:			/* DEC IYH */
			tStates += 9;
			IY -= 0x100;
			temp = hreg(IY);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				(((temp & 0xf) == 0xf) << 4) |
				((temp == 0x7f) << 2) | 2;
			break;
		case 0x26:			/* LD IYH,nn */
			tStates += 9;
			Sethreg(IY, GetBYTE_pp(PC));
			break;
		case 0x29:			/* ADD IY,IY */
			tStates += 15;
			IY &= 0xffff;
			sum = IY + IY;
			cbits = (IY ^ IY ^ sum) >> 8;
			IY = sum;
			AF = (AF & ~0x3b) | ((sum >> 8) & 0x28) |
				(cbits & 0x10) | ((cbits >> 8) & 1);
			break;
		case 0x2A:			/* LD IY,(nnnn) */
			tStates += 20;
			temp = GetWORD(PC);
			IY = GetWORD(temp);
			PC += 2;
			break;
		case 0x2B:			/* DEC IY */
			tStates += 10;
			--IY;
			break;
		case 0x2C:			/* INC IYL */
			tStates += 9;
			temp = lreg(IY)+1;
			Setlreg(IY, temp);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				(((temp & 0xf) == 0) << 4) |
				((temp == 0x80) << 2);
			break;
		case 0x2D:			/* DEC IYL */
			tStates += 9;
			temp = lreg(IY)-1;
			Setlreg(IY, temp);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				(((temp & 0xf) == 0xf) << 4) |
				((temp == 0x7f) << 2) | 2;
			break;
		case 0x2E:			/* LD IYL,nn */
			tStates += 9;
			Setlreg(IY, GetBYTE_pp(PC));
			break;
		case 0x34:			/* INC (IY+dd) */
			tStates += 23;
			adr = IY + (signed char) GetBYTE_pp(PC);
			temp = GetBYTE(adr)+1;
			PutBYTE(adr, temp);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				(((temp & 0xf) == 0) << 4) |
				((temp == 0x80) << 2);
			break;
		case 0x35:			/* DEC (IY+dd) */
			tStates += 23;
			adr = IY + (signed char) GetBYTE_pp(PC);
			temp = GetBYTE(adr)-1;
			PutBYTE(adr, temp);
			AF = (AF & ~0xfe) | (temp & 0xa8) |
				(((temp & 0xff) == 0) << 6) |
				(((temp & 0xf) == 0xf) << 4) |
				((temp == 0x7f) << 2) | 2;
			break;
		case 0x36:			/* LD (IY+dd),nn */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			PutBYTE(adr, GetBYTE_pp(PC));
			break;
		case 0x39:			/* ADD IY,SP */
			tStates += 15;
			IY &= 0xffff;
			SP &= 0xffff;
			sum = IY + SP;
			cbits = (IY ^ SP ^ sum) >> 8;
			IY = sum;
			AF = (AF & ~0x3b) | ((sum >> 8) & 0x28) |
				(cbits & 0x10) | ((cbits >> 8) & 1);
			break;
		case 0x44:			/* LD B,IYH */
			tStates += 9;
			Sethreg(BC, hreg(IY));
			break;
		case 0x45:			/* LD B,IYL */
			tStates += 9;
			Sethreg(BC, lreg(IY));
			break;
		case 0x46:			/* LD B,(IY+dd) */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			Sethreg(BC, GetBYTE(adr));
			break;
		case 0x4C:			/* LD C,IYH */
			tStates += 9;
			Setlreg(BC, hreg(IY));
			break;
		case 0x4D:			/* LD C,IYL */
			tStates += 9;
			Setlreg(BC, lreg(IY));
			break;
		case 0x4E:			/* LD C,(IY+dd) */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			Setlreg(BC, GetBYTE(adr));
			break;
		case 0x54:			/* LD D,IYH */
			tStates += 9;
			Sethreg(DE, hreg(IY));
			break;
		case 0x55:			/* LD D,IYL */
			tStates += 9;
			Sethreg(DE, lreg(IY));
			break;
		case 0x56:			/* LD D,(IY+dd) */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			Sethreg(DE, GetBYTE(adr));
			break;
		case 0x5C:			/* LD E,IYH */
			tStates += 9;
			Setlreg(DE, hreg(IY));
			break;
		case 0x5D:			/* LD E,IYL */
			Setlreg(DE, lreg(IY));
			break;
		case 0x5E:			/* LD E,(IY+dd) */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			Setlreg(DE, GetBYTE(adr));
			break;
		case 0x60:			/* LD IYH,B */
			tStates += 9;
			Sethreg(IY, hreg(BC));
			break;
		case 0x61:			/* LD IYH,C */
			tStates += 9;
			Sethreg(IY, lreg(BC));
			break;
		case 0x62:			/* LD IYH,D */
			tStates += 9;
			Sethreg(IY, hreg(DE));
			break;
		case 0x63:			/* LD IYH,E */
			tStates += 9;
			Sethreg(IY, lreg(DE));
			break;
		case 0x64:			/* LD IYH,IYH */
			tStates += 9;
			/* nop */
			break;
		case 0x65:			/* LD IYH,IYL */
			tStates += 9;
			Sethreg(IY, lreg(IY));
			break;
		case 0x66:			/* LD H,(IY+dd) */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			Sethreg(HL, GetBYTE(adr));
			break;
		case 0x67:			/* LD IYH,A */
			tStates += 9;
			Sethreg(IY, hreg(AF));
			break;
		case 0x68:			/* LD IYL,B */
			tStates += 9;
			Setlreg(IY, hreg(BC));
			break;
		case 0x69:			/* LD IYL,C */
			tStates += 9;
			Setlreg(IY, lreg(BC));
			break;
		case 0x6A:			/* LD IYL,D */
			tStates += 9;
			Setlreg(IY, hreg(DE));
			break;
		case 0x6B:			/* LD IYL,E */
			tStates += 9;
			Setlreg(IY, lreg(DE));
			break;
		case 0x6C:			/* LD IYL,IYH */
			tStates += 9;
			Setlreg(IY, hreg(IY));
			break;
		case 0x6D:			/* LD IYL,IYL */
			tStates += 9;
			/* nop */
			break;
		case 0x6E:			/* LD L,(IY+dd) */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			Setlreg(HL, GetBYTE(adr));
			break;
		case 0x6F:			/* LD IYL,A */
			tStates += 9;
			Setlreg(IY, hreg(AF));
			break;
		case 0x70:			/* LD (IY+dd),B */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			PutBYTE(adr, hreg(BC));
			break;
		case 0x71:			/* LD (IY+dd),C */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			PutBYTE(adr, lreg(BC));
			break;
		case 0x72:			/* LD (IY+dd),D */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			PutBYTE(adr, hreg(DE));
			break;
		case 0x73:			/* LD (IY+dd),E */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			PutBYTE(adr, lreg(DE));
			break;
		case 0x74:			/* LD (IY+dd),H */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			PutBYTE(adr, hreg(HL));
			break;
		case 0x75:			/* LD (IY+dd),L */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			PutBYTE(adr, lreg(HL));
			break;
		case 0x77:			/* LD (IY+dd),A */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			PutBYTE(adr, hreg(AF));
			break;
		case 0x7C:			/* LD A,IYH */
			tStates += 9;
			Sethreg(AF, hreg(IY));
			break;
		case 0x7D:			/* LD A,IYL */
			tStates += 9;
			Sethreg(AF, lreg(IY));
			break;
		case 0x7E:			/* LD A,(IY+dd) */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			Sethreg(AF, GetBYTE(adr));
			break;
		case 0x84:			/* ADD A,IYH */
			tStates += 9;
			temp = hreg(IY);
			acu = hreg(AF);
			sum = acu + temp;
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				((cbits >> 8) & 1);
			break;
		case 0x85:			/* ADD A,IYL */
			tStates += 9;
			temp = lreg(IY);
			acu = hreg(AF);
			sum = acu + temp;
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				((cbits >> 8) & 1);
			break;
		case 0x86:			/* ADD A,(IY+dd) */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			temp = GetBYTE(adr);
			acu = hreg(AF);
			sum = acu + temp;
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				((cbits >> 8) & 1);
			break;
		case 0x8C:			/* ADC A,IYH */
			tStates += 9;
			temp = hreg(IY);
			acu = hreg(AF);
			sum = acu + temp + TSTFLAG(C);
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				((cbits >> 8) & 1);
			break;
		case 0x8D:			/* ADC A,IYL */
			tStates += 9;
			temp = lreg(IY);
			acu = hreg(AF);
			sum = acu + temp + TSTFLAG(C);
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				((cbits >> 8) & 1);
			break;
		case 0x8E:			/* ADC A,(IY+dd) */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			temp = GetBYTE(adr);
			acu = hreg(AF);
			sum = acu + temp + TSTFLAG(C);
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) |
				((cbits >> 8) & 1);
			break;
		case 0x94:			/* SUB IYH */
			tStates += 9;
			temp = hreg(IY);
			acu = hreg(AF);
			sum = acu - temp;
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
				((cbits >> 8) & 1);
			break;
		case 0x95:			/* SUB IYL */
			tStates += 9;
			temp = lreg(IY);
			acu = hreg(AF);
			sum = acu - temp;
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
				((cbits >> 8) & 1);
			break;
		case 0x96:			/* SUB (IY+dd) */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			temp = GetBYTE(adr);
			acu = hreg(AF);
			sum = acu - temp;
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
				((cbits >> 8) & 1);
			break;
		case 0x9C:			/* SBC A,IYH */
			tStates += 9;
			temp = hreg(IY);
			acu = hreg(AF);
			sum = acu - temp - TSTFLAG(C);
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
				((cbits >> 8) & 1);
			break;
		case 0x9D:			/* SBC A,IYL */
			tStates += 9;
			temp = lreg(IY);
			acu = hreg(AF);
			sum = acu - temp - TSTFLAG(C);
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
				((cbits >> 8) & 1);
			break;
		case 0x9E:			/* SBC A,(IY+dd) */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			temp = GetBYTE(adr);
			acu = hreg(AF);
			sum = acu - temp - TSTFLAG(C);
			cbits = acu ^ temp ^ sum;
			AF = ((sum & 0xff) << 8) | (sum & 0xa8) |
				(((sum & 0xff) == 0) << 6) | (cbits & 0x10) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
				((cbits >> 8) & 1);
			break;
		case 0xA4:			/* AND IYH */
			tStates += 9;
			sum = ((AF & (IY)) >> 8) & 0xff;
			AF = (sum << 8) | (sum & 0xa8) |
				((sum == 0) << 6) | 0x10 | partab[sum];
			break;
		case 0xA5:			/* AND IYL */
			tStates += 9;
			sum = ((AF >> 8) & IY) & 0xff;
			AF = (sum << 8) | (sum & 0xa8) | 0x10 |
				((sum == 0) << 6) | partab[sum];
			break;
		case 0xA6:			/* AND (IY+dd) */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			sum = ((AF >> 8) & GetBYTE(adr)) & 0xff;
			AF = (sum << 8) | (sum & 0xa8) | 0x10 |
				((sum == 0) << 6) | partab[sum];
			break;
		case 0xAC:			/* XOR IYH */
			tStates += 9;
			sum = ((AF ^ (IY)) >> 8) & 0xff;
			AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
			break;
		case 0xAD:			/* XOR IYL */
			tStates += 9;
			sum = ((AF >> 8) ^ IY) & 0xff;
			AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
			break;
		case 0xAE:			/* XOR (IY+dd) */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			sum = ((AF >> 8) ^ GetBYTE(adr)) & 0xff;
			AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
			break;
		case 0xB4:			/* OR IYH */
			tStates += 9;
			sum = ((AF | (IY)) >> 8) & 0xff;
			AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
			break;
		case 0xB5:			/* OR IYL */
			tStates += 9;
			sum = ((AF >> 8) | IY) & 0xff;
			AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
			break;
		case 0xB6:			/* OR (IY+dd) */
			tStates += 19;
			adr = IY + (signed char) GetBYTE_pp(PC);
			sum = ((AF >> 8) | GetBYTE(adr)) & 0xff;
			AF = (sum << 8) | (sum & 0xa8) | ((sum == 0) << 6) | partab[sum];
			break;
		case 0xBC:			/* CP IYH */
			tStates += 9;
			temp = hreg(IY);
			AF = (AF & ~0x28) | (temp & 0x28);
			acu = hreg(AF);
			sum = acu - temp;
			cbits = acu ^ temp ^ sum;
			AF = (AF & ~0xff) | (sum & 0x80) |
				(((sum & 0xff) == 0) << 6) | (temp & 0x28) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
				(cbits & 0x10) | ((cbits >> 8) & 1);
			break;
		case 0xBD:			/* CP IYL */
			tStates += 9;
			temp = lreg(IY);
			AF = (AF & ~0x28) | (temp & 0x28);
			acu = hreg(AF);
			sum = acu - temp;
			cbits = acu ^ temp ^ sum;
			AF = (AF & ~0xff) | (sum & 0x80) |
				(((sum & 0xff) == 0) << 6) | (temp & 0x28) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
				(cbits & 0x10) | ((cbits >> 8) & 1);
			break;
		case 0xBE:			/* CP (IY+dd) */
			tStates += 9;
			adr = IY + (signed char) GetBYTE_pp(PC);
			temp = GetBYTE(adr);
			AF = (AF & ~0x28) | (temp & 0x28);
			acu = hreg(AF);
			sum = acu - temp;
			cbits = acu ^ temp ^ sum;
			AF = (AF & ~0xff) | (sum & 0x80) |
				(((sum & 0xff) == 0) << 6) | (temp & 0x28) |
				(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
				(cbits & 0x10) | ((cbits >> 8) & 1);
			break;
		case 0xCB:			/* CB prefix */
			adr = IY + (signed char) GetBYTE_pp(PC);
			switch ((op = GetBYTE(PC)) & 7) {
			    /*
			     * default: supresses compiler warning: "warning:
			     * 'acu' may be used uninitialized in this function"
			     */
			default:
			case 0: ++PC; acu = hreg(BC); break;
			case 1: ++PC; acu = lreg(BC); break;
			case 2: ++PC; acu = hreg(DE); break;
			case 3: ++PC; acu = lreg(DE); break;
			case 4: ++PC; acu = hreg(HL); break;
			case 5: ++PC; acu = lreg(HL); break;
			case 6: ++PC; acu = GetBYTE(adr);  break;
			case 7: ++PC; acu = hreg(AF); break;
			}
			switch (op & 0xc0) {
			/*
			 * Use default to supress compiler warning: "warning:
			 * 'temp' may be used uninitialized in this function"
			 */
			default:
			case 0x00:		/* shift/rotate */
				switch (op & 0x38) {
				/*
				 * Use default to supress compiler warning:
				 * "warning: 'temp' may be used uninitialized
				 * in this function"
				 */
				default:
				case 0x00:	/* RLC */
					temp = (acu << 1) | (acu >> 7);
					cbits = temp & 1;
					goto cbshflg3;
				case 0x08:	/* RRC */
					temp = (acu >> 1) | (acu << 7);
					cbits = temp & 0x80;
					goto cbshflg3;
				case 0x10:	/* RL */
					temp = (acu << 1) | TSTFLAG(C);
					cbits = acu & 0x80;
					goto cbshflg3;
				case 0x18:	/* RR */
					temp = (acu >> 1) | (TSTFLAG(C) << 7);
					cbits = acu & 1;
					goto cbshflg3;
				case 0x20:	/* SLA */
					temp = acu << 1;
					cbits = acu & 0x80;
					goto cbshflg3;
				case 0x28:	/* SRA */
					temp = (acu >> 1) | (acu & 0x80);
					cbits = acu & 1;
					goto cbshflg3;
				case 0x30:	/* SLIA */
					temp = (acu << 1) | 1;
					cbits = acu & 0x80;
					goto cbshflg3;
				case 0x38:	/* SRL */
					temp = acu >> 1;
					cbits = acu & 1;
				cbshflg3:
					AF = (AF & ~0xff) | (temp & 0xa8) |
						(((temp & 0xff) == 0) << 6) |
						parity(temp) | !!cbits;
				}
				break;
			case 0x40:		/* BIT */
				tStates += 20;
				if (acu & (1 << ((op >> 3) & 7)))
					AF = (AF & ~0xfe) | 0x10 |
					(((op & 0x38) == 0x38) << 7);
				else
					AF = (AF & ~0xfe) | 0x54;
				if ((op&7) != 6)
					AF |= (acu & 0x28);
				temp = acu;
				break;
			case 0x80:		/* RES */
				tStates += 23;
				temp = acu & ~(1 << ((op >> 3) & 7));
				break;
			case 0xc0:		/* SET */
				tStates += 23;
				temp = acu | (1 << ((op >> 3) & 7));
				break;
			}
			switch (op & 7) {
			case 0: Sethreg(BC, temp); break;
			case 1: Setlreg(BC, temp); break;
			case 2: Sethreg(DE, temp); break;
			case 3: Setlreg(DE, temp); break;
			case 4: Sethreg(HL, temp); break;
			case 5: Setlreg(HL, temp); break;
			case 6: PutBYTE(adr, temp);  break;
			case 7: Sethreg(AF, temp); break;
			}
			break;
		case 0xE1:			/* POP IY */
			tStates += 14;
			POP(IY);
			break;
		case 0xE3:			/* EX (SP),IY */
			tStates += 23;
			temp = IY; POP(IY); PUSH(temp);
			break;
		case 0xE5:			/* PUSH IY */
			tStates += 15;
			PUSH(IY);
			break;
		case 0xE9:			/* JP (IY) */
			tStates += 8;
			PC = IY;
			break;
		case 0xF9:			/* LD SP,IY */
			tStates += 9;
			SP = IY;
			break;
		default: PC--;		/* ignore DD */
		}
		break;
	case 0xFE:			/* CP nn */
		tStates += 7;
		temp = GetBYTE_pp(PC);
		AF = (AF & ~0x28) | (temp & 0x28);
		acu = hreg(AF);
		sum = acu - temp;
		cbits = acu ^ temp ^ sum;
		AF = (AF & ~0xff) | (sum & 0x80) |
			(((sum & 0xff) == 0) << 6) | (temp & 0x28) |
			(((cbits >> 6) ^ (cbits >> 5)) & 4) | 2 |
			(cbits & 0x10) | ((cbits >> 8) & 1);
		break;
	case 0xFF:			/* RST 38H */
		tStates += 11;
		PUSH(PC); PC = 0x38;
    }
    }
/* make registers visible for debugging if interrupted */
    SAVE_STATE();
    return (PC&0xffff)|0x10000;	/* flag non-bios stop */
}
