/*
   Module Keytrans to handle keytranslation
   Copyright (c) 2015 by Jon Saxton (Australia)

   Parts are modified by Andreas Gerlich

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
#include <ctype.h>
#include <string.h>
#include "ytypes.h"	/* for WORD */
#include "mem_mmu.h"	/* by agl */
#include "yaze.h"	/* by agl nessesary for newstr() (extern) */
#include "ktt.h"

/*---------------------------------------------------------
//			trim()
//
//  Removes leading and trailing spaces from a string.
//
//-------------------------------------------------------*/

static char *trim(char *x)
{
    char
        *e;
    while (isspace(*x))
        ++x;
    /* x now points at first non-space character.  The next step
    // is to remove trailing spaces.  It is easy enough to do a
    // forward scan but we are looking at short strings so it is
    // even easier to do a reverse scan. */
    
    e = x + strlen(x);
    while (isspace(*(--e)));
    *(e+1) = 0;
    return x;
}

/*-----------------------------------------------------------
//		keyboard translate table layout
//
//  The translate table is a fixed size. In the short term
//  this was a lot less complex than managing a dynamically
//  allocated table.  Making it a fixed size adequate for
//  any reasonable number of entries only costs about 20 Kb
//  which should not be a problem for any modern host.
//---------------------------------------------------------*/

struct kt_entry_
{
    ui32	keycode;	/* Coded keystroke */
    int		length;		/* Length of translated sequence */
    char	string[128];	/* Translated character sequence */
};

struct kt_def
{
    ui32	keycode;
    char	*spec;
};

typedef struct kt_entry_ kt_entry;

struct key_table_
{
    int		elements;
    char	*name;
    kt_entry	keys[200];
};

typedef struct key_table_ key_table;

/*-----------------------------------------------------------
//		     key translate table
//---------------------------------------------------------*/

static int const
    ktt_capacity = 200;
static key_table
    ktt = {-5, 0};	/* Initial value < 0 forces loader
                        // invocation on first attempt to
                        // translate a keystroke.  */
                        
/*----------------------------------------------------------
//		Set/reset diagnostic ouptput
//--------------------------------------------------------*/

static int
    verbose = 0;	/* For diagnostics */
void diagnose(int d)
{
    verbose = d;
}

/*----------------------------------------------------------
//			kt_compare()
//
// The C equivalent of a functor.  Used in qsort() and
// bsearch to compare two values.
//---------------------------------------------------------*/

static int kt_compare(const void *l, const void *r)
{
    kt_entry
        *left = (kt_entry *)l,
        *right = (kt_entry *)r;
    if (left->keycode == right->keycode)
        return 0;
    if (left->keycode > right->keycode)
        return 1;
    return -1;
}

/*-----------------------------------------------------------
//			ktt_delete()
//----------------------------------------------------------*/

static void ktt_delete()
{
    ktt.elements = 0;
    if (ktt.name)
    {
        free(ktt.name);
        ktt.name = 0;
    }
}

/*-----------------------------------------------------------
//			  ktt_name()
//			ktt_elements()
//
//  Just a couple of accessors.
//----------------------------------------------------------*/

char *ktt_name()
{
    return ktt.name;
}

int ktt_elements()
{
    return ktt.elements;
}

/*-----------------------------------------------------------
//			ktt_load()
//
//  Reads a user-constructed key translate table and builds
//  a sorted array of key translation specifiers.
//
//----------------------------------------------------------*/

int ktt_load(char *keyfile)
{
    struct kt_def
        keys[] =
        {
            { F1,		"f1" },
            { F2,		"f2" },
            { F3,		"f3" },
            { F4,		"f4" },
            { F5,		"f5" },
            { F6,		"f6" },
            { F7,		"f7" },
            { F8,		"f8" },
            { F9,		"f9" },
            { F10,		"f10" },
            { F11,		"f11" },
            { F12,		"f12" },
            { Insert,		"insert" },
            { Insert,		"ins" },
            { Delete,		"delete" },
            { Delete,		"del" },
            { Home,		"home" },
            { End,		"end" },
            { PageUp,		"pageup" },
            { PageUp,		"pgup" },
            { PageDown, 	"pagedown" },
            { PageDown,		"pgdn" },
	    { PageDown,		"pagedn" },
            { Up,		"up" },
            { Down,		"down" },
            { Down,		"dn" },
            { Right,		"right" },
            { Left,		"left" },
            { NP5,		"np5" },
            { ReverseTab,	"reversetab" },
            { ReverseTab,	"backtab" },
            { KT_SHIFT,		"shift" },
            { KT_ALT,		"alt" },
            { KT_CONTROL,	"ctl" },
            { KT_CONTROL,	"ctrl" },
	    { KT_CONTROL,	"control" },
            { KT_LITERAL,	"literal" },
            { KT_LITERAL,	"lit" },
            { '=',		"eq" },
            { '=',		"equals" },
            { 0x1B,		"esc" },
            { 0x1B,		"escape" },
	    { SysRq,		"sysrq" },
            { 0,		0 }
        },
        codes[] =
        {
            { 0x00, "nul" }, { 0x00, "^@" },
            { 0x01, "soh" }, { 0x01, "^a" },
            { 0x02, "stx" }, { 0x02, "^b" },
            { 0x03, "etx" }, { 0x03, "^c" },
            { 0x04, "eot" }, { 0x04, "^d" },
            { 0x05, "enq" }, { 0x05, "^e" },
            { 0x06, "ack" }, { 0x06, "^f" },
            { 0x07, "bel" }, { 0x07, "^g" },
            { 0x08, "bs"  }, { 0x08, "^h" },
            { 0x09, "ht"  }, { 0x09, "tab" }, { 0x09, "^i" },
            { 0x0A, "lf"  }, { 0x0A, "nl" },  { 0x0a, "^j" },
            { 0x0B, "vt"  }, { 0x0b, "^k" },
            { 0x0C, "ff"  }, { 0x0C, "np" },  { 0x0c, "^l" },
            { 0x0D, "cr"  }, { 0x0d, "^m" },
            { 0x0E, "so"  }, { 0x0e, "^n" },
            { 0x0F, "si"  }, { 0x0f, "^o" },
            { 0x10, "dle" }, { 0x10, "^p" },
            { 0x11, "dc1" }, { 0x11, "^q" },
            { 0x12, "dc2" }, { 0x12, "^r" },
            { 0x13, "dc3" }, { 0x13, "^s" },
            { 0x14, "dc4" }, { 0x14, "^t" },
            { 0x15, "nak" }, { 0x15, "^u" },
            { 0x16, "syn" }, { 0x16, "^v" },
            { 0x17, "etb" }, { 0x17, "^w" },
            { 0x18, "can" }, { 0x18, "^x" },
            { 0x19, "em"  }, { 0x19, "^y" },
            { 0x1A, "sub" }, { 0x1a, "^z" },
            { 0x1B, "esc" }, { 0x1b, "^[" },
            { 0x1C, "fs"  }, { 0x1c, "^\\" },
            { 0x1D, "gs"  }, { 0x1d, "^]" },
            { 0x1E, "rs"  }, { 0x1e, "^^" },
            { 0x1F, "us"  }, { 0x1f, "^_" },
            { 0x22, "qt"  }, { 0x22, "quote" },
            { 0x7F, "del" },
            { 0xFF, "srq" }, { 0xFF, "sysrq" },
            { 0, 0 }
        };
    char
        tbuf[200],
        token[200],
        *line,
        *t;
    ui32
        kc = 0;
    int
        entry,
        valid,
        x;
    FILE
        *kf;

    /* Dispense with the old table */
    ktt_delete();

    /* If we have been asked to unload the table then we're finished */
    if (strcmp(keyfile, "-") == 0)
        return 0;

    /* Try to load the table specified.  On any error we just leave
    // the default (empty) table loaded */
    kf = fopen(keyfile, "r");
    if (!kf)
        return 0;
	
    /* Read the file and build the translate table. */
    entry = 0;
    for (;entry < ktt_capacity;)
    {
        valid = 0;
        line = fgets(tbuf, 199, kf);
        if (!line)
            break;
        line = trim(line);
        if (!isalpha(line[0]))
            continue;

        /* We have a line which may describe a translation.
        //
        // The general format is:
        //
        // <qualifier list> <key> = <string>
        //
        // where <qualifier list> comprises zero or more shift
        // specifiers (Ctl, Alt, Shift) and <key> is something
        // like Home, F6, Left, PageUp (or PgUp). */

	ktt.keys[entry].length = kc = 0;
        do
        {
            t = token;
            if (isspace(*(line+1)) || *(line+1) == '=')
            {
                /* Single-character token */
                kc |= *line++;
                while (isspace(*line) && *line != '=')
                    ++line;
            }
            else
            {
                /* Longer token - look for a match in the key names table */
                while (!isspace(*line) && *line != '=')
                    *t++ = tolower(*line++);
                *t = 0;
                while (isspace(*line) && *line != '=')
                    ++line;

                if (token[0] == 'u' && token[1] == '+')
                {
                    /* Unicode specifier U+XXXX */
                    sscanf(token+2, "%x", &kc);
                    /* That cleared any modifiers.  For a Unicode
                    // specifier that is correct behaviour.  Now
                    // skip anything else before the = symbol. */
                    if (kc >= 0xC0)
                        kc |= KT_UNICODE;
                    while (*line && *line != '=')
                        ++line;
                }
                else
                {
                    for (x = 0; keys[x].spec; ++x)
                    {
                        if (strcmp(token, keys[x].spec) == 0)
                        {
                            kc |= keys[x].keycode;
                            break;
                        }
                    }
                    if (!keys[x].spec)
                        break;
                }
            }
        }
        while (*line != '=');

        while (isspace(*(++line)));

        /* Handle mulitple things on RHS */
	while (*line)
	{
	    if (*line == '"')
	    {
	        /* Quoted string */
		for (t = ktt.keys[entry].string + ktt.keys[entry].length;
		     *(++line) && *line != '"';
		     *t++ = *line)
		    ++ktt.keys[entry].length;
		ktt.keys[entry].keycode = kc;
		++line;
		valid = 1;
	    }
            else if (isalnum(*line) || *line == '^')
            {
                int checkHex = 1;
                for (t = token; *line && !isspace(*line); ++line)
                    *t++ = tolower(*line);
		*t = 0;
                for (x = 0; codes[x].spec; ++x)
                {
		    if (strcmp(token, codes[x].spec) == 0)
		    {
			valid = 1;
			checkHex = 0;
			ktt.keys[entry].keycode = kc;
			if (ktt.keys[entry].length > 127)
			    valid = 0;
                        else
			    ktt.keys[entry].string[ktt.keys[entry].length++]
				= codes[x].keycode;
                        break;
		    }
		}
		/* Even if we didn't find a character name we may have
		// a 2-digit hexadecimal number.
		// Note that it is not possible to inject 0xFF this way
		// because the string "ff" has been used for form feed
		// (0x0C).  That shouldn't matter as you probably don't
		// need to send 0xFF to CP/M anyway.
                //
                // 2015.01.25 jrs ... 0xFF is now used as a CP/M "SysRq"  */
		if (checkHex && strlen(token) == 2
		             && isxdigit(token[0])
		             && isxdigit(token[1]))
                {
                    unsigned int b;
		    valid = 1;
                    sscanf(token, "%x", &b);
		    ktt.keys[entry].keycode = kc;
		    if (ktt.keys[entry].length > 127)
		        valid = 0;
                    else
                        ktt.keys[entry].string[ktt.keys[entry].length++] = b;
		}
	    }
	    else
	    {
		valid = 0;
		break;
	    }

	    while (isspace(*line))
		++line;
	}
        if (valid)
            ++entry;
    }
    fclose(kf);
    if (entry == 0)
        return 0;
    ktt.elements = entry;
    ktt.name = newstr(keyfile);
    qsort(ktt.keys, entry, sizeof(kt_entry), kt_compare);

    if (verbose)
        printf("Loaded %s, elements = %d\r\n",
                ktt_name() ? ktt_name() : "<none>",
                ktt_elements());

    return ktt.elements;
}

/*---------------------------------------------------------------------
//				keyTrans()
//
//	Translates a compound or primitive keystroke according to
//	a user-specified table.
//
//	Returns non-zero if the translation failed.  That will
//	happen for a compound keystroke which does not appear in
//	the translate table.
//
//-------------------------------------------------------------------*/

int keyTrans(ui32 key, struct _ci *ci)
{
    kt_entry
        *k = 0;

    /* Ensure that the keyboard translate table is loaded.  This is
    // only done once.
    //
    // If there is any sort of error then use an empty table so that
    // yaze will work in a default mode. */
    if (ktt.elements < 0)
        ktt_load("yaze.ktt");

    if (verbose)
        printf("\r\nkey = %08X\r\n", key);

    /* See if there is an entry in the translate table. */
    if (ktt.elements)
        k = bsearch(&key, ktt.keys, ktt.elements, sizeof(kt_entry),
                    kt_compare);
    if (k)
    {
        ci->size = k->length;
        memcpy(ci->queue, k->string, k->length);
        ci->queue[ci->size] = ci->index = 0;
        return 0;
    }
    /* There was no corresponding entry in the translate table but
    // the key may be something simple. */

    if (key & 0xFFFFFF00)
        return 1;
    else
    {
        ci->size = 1;
        ci->queue[0] = key;
        return 0;
    }
}
