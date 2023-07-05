/*==============================================================
//	A program to exercise and test the console input
//	routines and keyboard translation of yaze-ag
//	Copyright (c) by Jon Saxton (Australia) 2015
//============================================================*/

#include "chan.h"
#include "ytypes.h"
#include "ktt.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <termios.h>
#include <sys/time.h>

#define INTERKEY_TIMEOUT 50000
#define ISATTY 1
#define ISRAW  2

/*---------------------------------------------------------
//  Some things copied from ybios.c and monitor.c to
//  set the console input mode.
//-------------------------------------------------------*/

struct sio
{
    FILE *fp;
    char *filename;
    char *streamname;
    char tty;
    const char strtype;   
} s;

struct termios
    rawtio,
    cookedtio;
int
    ttyflags,
    interrupt = 0;

int
    ttyflags = ISATTY;

void ttyraw()
{
    if ((ttyflags & (ISATTY|ISRAW)) == ISATTY)
    {
	tcsetattr(fileno(stdin), TCSAFLUSH, &rawtio);
	ttyflags |= ISRAW;
    }
}

void ttycook()
{
    if (ttyflags & ISRAW)
    {
	tcsetattr(fileno(stdin), TCSAFLUSH, &cookedtio);
	putc('\n', stdout);
	ttyflags &= ~ISRAW;
    }
}

int serin(int chan)
{
    char c;
    int ch;

    if (s.fp == NULL)
	return 0x1A;
    if (s.tty)
    {	if (read(fileno(s.fp), &c, 1) == 0)
	    return 0x1A;
	else
	    return c;
    }
    if ((ch = getc(s.fp)) == EOF)
	return 0x1A;
    else
	return ch & 0xFF;
}

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

/* stash a string away on the heap */
char *
newstr(const char *str)
{
	char *p = xmalloc(strlen(str) + 1);
	(void) strcpy(p, str);
	return p;
}

/*-------------------------------------------------------
//			init_term()
//
//  init_term() puts the console (keyboard) into raw
//  mode.
//-----------------------------------------------------*/

void init_term()
{
    char *name = ttyname(fileno(stdin));

    s.fp = stdin;
    s.filename = name ? newstr(name) : "(stdin)";
    s.tty = isatty(fileno(stdin));
    if (s.tty)
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
	memset(rawtio.c_cc, 0, NCCS);
	rawtio.c_cc[VINTR] = interrupt;
	rawtio.c_cc[VMIN] = 1;
    }
    atexit(ttycook);
    ttyraw();
    puts("Press keys.  <esc> <esc> <esc> to end\r");
}

/*--------------------------------------------------------
//			contest()
//
//  Returns TRUE if a character is available from the
//  keyboard within 50 ms.  Used after an ESC character
//  is seen to decide whether the ESC is an isolated
//  character or part of a multi-byte sequence.
//------------------------------------------------------*/

int contest()
{
    static struct timeval
        t = { 0, INTERKEY_TIMEOUT };
    fd_set
        rdy;
    int
        fd = STDIN_FILENO;

    FD_ZERO(&rdy);
    FD_SET(fd, &rdy);
    (void) select(fd+1, &rdy, NULL, NULL, &t);
    return FD_ISSET(fd, &rdy);
}

/*--------------------------------------------------------
//			usage
//------------------------------------------------------*/

char banner1[] =
    "\nThis program is for testing keyboard input and translation.\n\n"
    "The first parameter determines the mode of operation:-\n\n"
    "   -\tdisplays characters as they are entered via the keyboard.  This\n"
    "\tis useful for seeing exactly what sequence of characters any key\n"
    "\tgenerates.\n",
    banner2[] =
    "   +\tshows what comes back from the translation module.  Use this mode\n"
    "\tto test translation tables.\n\n"
    "  fn\t(where fn is a file name) is like + except that fn.ktt is loaded\n"
    "\tinstead of yaze.ktt\n\n"
    "In translated mode, pressing ESC twice toggles a diagnostic feature in the\n"
    "translation module which prints the keycode that it sees.\n";

void usage()
{
    puts(banner1);
    puts(banner2);
}

/*--------------------------------------------------------
//		Various I/O functions
//------------------------------------------------------*/

int _putch(int c)
{
    int e = putchar(c);
    fflush(stdout);
    return e;
}

void putstr(char *v)
{
    while (*v)
	_putch(*v++);
}

int hex[16] = { '0', '1', '2', '3', '4', '5', '6', '7',
                '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };

void puthex(int k)
{
    _putch('<');
    _putch(hex[(k & 0xF0) >> 4]);
    _putch(hex[k & 0xF]);
    _putch('>');
}

extern BYTE conin();
static int verbose = 0;
extern void diagnose(int v);

enum mode_
{
    UNSPECIFIED,
    UNADORNED,
    TRANSLATED
};


/*----------------------------------------------------------
//		   It all happens here
//--------------------------------------------------------*/

int main(int argc, char *argv[])
{
    BYTE
        key;
    int
        mode = UNSPECIFIED,
        escapes=0;
 
    /* Need more standard argument parsing than this ... */
    if (argc > 1)
    {
        if (strcmp(argv[1], "-") == 0)
            mode = UNADORNED;
        else if (strcmp(argv[1], "+") == 0)
            mode = TRANSLATED;
        else
        {
            /* Try to load the named translate table */
            char
                fn[140];
            strcpy(fn, argv[1]);
            strcat(fn, ".ktt");
            ktt_load(fn);
            mode = TRANSLATED;
        }
    }
    if (mode == UNSPECIFIED)
    {
        usage();
        return 1;
    }

    init_term();
    while (escapes < 3)
    {
	key = mode == TRANSLATED ? conin() : serin(0);
	if (key == 0x1B)
	{
	    putstr("<esc>");
	    if (++escapes >= 3)
		break;
            if (escapes == 2 && mode == TRANSLATED)
                diagnose((verbose = 1 - verbose));
	}
	else
	{
	    escapes = 0;
	    switch (key)
	    {
	    case '\r':
	    case '\n':
		puthex(key);
		_putch('\n');
		_putch('\r');
		break;
	    default:
		if (key < 0x20 || key > 0x7E)
		    puthex(key);
		else
		    _putch(key);
	    }
	}
    }
    _putch('\n');
    _putch('\r');
    return 0;
}

