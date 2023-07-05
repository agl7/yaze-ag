# Makefile for yaze-ag

BUILD = " (build for AMD (barcelona, 64Bit))"

# CC must be an ANSI-C compiler
CC            =	clang

#where you want the binaries and manual page
BINDIR	      = /usr/local/bin
MANDIR	      = /usr/local/man/man1
LIBDIR	      = /usr/local/lib/yaze
DSKDIR        = $(LIBDIR)/disks
DOCDIR        = $(LIBDIR)/doc
DOCHTMLDIR    = $(LIBDIR)/doc_html

#OPTIMIZE	= -g -m486
#OPTIMIZE	= -g -save-temps -m486
# OPTIMIZE	=
# OPTIMIZE	= -O1
# OPTIMIZE	= -O2
# OPTIMIZE	= -O2 -save-temps
# OPTIMIZE	= -O2 -save-temps -m486

# optimizing with gcc 2.x.x
#OPTIMIZE	= -O3 -m486

# optimizing with gcc 3.x.x
# (please search for your processor in "man gcc")
# (tip: search first for "pentium3")
#
# -march=cpu-type
#	Tune to cpu-type everything applicable about the gen-
#	erated code, except for the ABI and the set of avail-
#	able instructions.  The choices for cpu-type are i386,
#	i486, i586, i686, pentium, pentium-mmx, pentiumpro,
#	pentium2, pentium3, pentium4, k6, k6-2, k6-3, athlon,
#	athlon-tbird, athlon-4, athlon-xp, athlon-mp,
#	winchip-c6, winchip2 and c3.
#
#OPTIMIZE	= -O3 -march=i386
#OPTIMIZE	= -O3 -march=i486
#OPTIMIZE	= -O3 -march=i586
#OPTIMIZE	= -O3 -march=i686
#OPTIMIZE	= -O3 -march=pentium
#OPTIMIZE	= -O3 -march=pentium-mmx
#OPTIMIZE	= -O3 -march=pentiumpro
#OPTIMIZE	= -O3 -march=pentium2
#OPTIMIZE	= -O3 -march=pentium3
#OPTIMIZE	= -O3 -march=pentium4
#OPTIMIZE	= -O3 -march=nocona	# 64 Bit Intel
#OPTIMIZE	= -O3 -march=k6
#OPTIMIZE	= -O3 -march=k6-2
#OPTIMIZE	= -O3 -march=k6-3
#OPTIMIZE	= -O3 -march=athlon
#OPTIMIZE	= -O3 -march=athlon-tbird
#OPTIMIZE	= -O3 -march=athlon-4
#OPTIMIZE	= -O3 -march=athlon-xp
#OPTIMIZE	= -O3 -march=athlon-mp
#OPTIMIZE	= -O3 -march=athlon64	# 64 Bit AMD
#OPTIMIZE	= -O3 -march=athlon64-sse3
#OPTIMIZE	= -O3 -march=barcelona
OPTIMIZE	= -O3 -march=x86-64

# -DUSE_GNU_READLINE for command recall/editing and filename completion
# -DBGii_BUG works around a problem in Backgrounder II
# -DBIOS to build a CP/M bios and monitor program into the emulator
#  (see also YAZE_OBJS, below)
#
# -- The following options are new:
#
# -DMMU compiles support for MMU-switched memory and CP/M 3.1 extensions
# -DMMUTABLES=<val> sets the no of MMU tables (default 16)
# -DYAZEPAGESIZE=<val> sets the page size for every page pointer (default 4 KB)
#		(The MMUPAGEPOINTERS are generated automatically)
# -DSHOWMMU shows at start the MMU tables (will be defined if MMUTEST is def.)
# -DMMUTEST setup pages for 3000h - BFFFh to pages above 64K
#	    (-DSHOWMMU will be set and the MMU-Table 7 will be selected)
#	    (Only used unter CP/M 2.2. See README-1.20 for details)
#	    (DO NOT USE IT IF YOU WANT TO RUN CP/M 3.1 - THIS IS ONLY FOR TEST!)
#
# -DMEMSIZE=<val> sets size of memory in KBytes (default 64KB, with MMU: 512KB)
#
# -DBOOTSYS loads yaze-cpm3.boot at 0100H  instead of  yaze.boot (CP/M 2.2).
#	    yaze-cpm3.boot is idendical with A:CPM3.COM. Inside there is
#	    the CP/M 3.1 loader BOOTSYS.COM + CPM3.SYS (see BOOTSYS.Z80
#	    on drive P:). You can generate CPM3.COM with
#		"pip cpm3.com=bootsys.com,cpm3.sys".
#	    If BOOTSYS is given you can boot CP/M 2.2 (SUPERDOS) with
#		"yaze -l -1 -b yaze.boot".
#
# -DSHOWDRV shows the entries in @dtbl, the DPHs and the DPBs when CP/M 3.1
#	    starts or "mount.com" or "sys mount" is running.
# -DMULTIO  Use Multi-I/O disk transfere
# -DRWDEBUG Set the debug mode for read/write operations. If MULTIO is defined
#	    and multi sector count is set by the bdos the multi sector read
#	    and write will be displayed with "r" and "w". The rest will be
#	    displayed with ".". Reads/writes without multi sector count will
#	    be displayed with "R" and "W".
#
# -DUCSD    Set mode if you want to use UCSD-pascal version for CP/M.
#	    It was neccessary because UCSD-Pascal-System does not use the
#	    register B when it gives Sektor or Track to the BIOS. With this
#	    Flag only the Register C of BC is used in SETTRK & SETSEC. It
#	    was neccessary because yaze-ag crashes.
#
# --
# solaris2, freebsd and cygwin needs -D__EXTENSIONS__
# linux needs -D_BSD_SOURCE
#
OPTIONS	= -DBIOS -D__EXTENSIONS__ -DMMU -DBOOTSYS -DMULTIO # -DSHOWDRV

# Link with CP/M BIOS support,
YAZE_OBJS     = yaze.o mem_mmu.o simz80.o simz80_wts.o ytimer.o ybios.o \
		monitor.o keytran.o conin.o
# or link as a naked Z80
#YAZE_OBJS    = yaze.o simz80.o io.o

# Keyboard translation test program
KT_OBJS       = keytest.o conin.o keytran.o nulrq.o

# -lreadline -lcurses -liberty if you defined USE_GNU_READLINE
#  (you don't need -liberty on linux or BSD systems)
#
#LIBS	      = -lreadline -lncurses
LIBS	      =

# a bsd-like install program (/usr/ucb/install on Solaris2)
INSTALL	      = install


###### you should not need to change anything below this line ######
# CWARN	      = -ansi -pedantic -Wall -Wshadow \
#		-Wpointer-arith -Wnested-externs -Winline
CWARN	      = -pedantic -Wall -Wshadow \
		-Wpointer-arith -Wnested-externs -Winline \
		-Wno-attributes -Wno-char-subscripts -Wno-unused-variable
# CWARN	      = -ansi -Wall 
CFLAGS        =	$(CWARN) $(OPTIMIZE) $(OPTIONS) -DLIBDIR=\"$(LIBDIR)/\" \
		-DBUILD=\"$(BUILD)\"

SRCS	      = yaze.c mem_mmu.c simz80.c simz80_wts.c ytimer.c io.c ybios.c monitor.c cdm.c \
		conin.c keytest.c keytran.c nulrq.c \
		chan.h ktt.h mem_mmu.h simz80.h yaze.h ybios.h ytypes.h \
		ktt.z80 keycodes.z80

DOC	      = COPYING yaze.1 cdm.1 cpm.1 z80.1 ChangeLog \
		doc docfiles.txt install_doc.sh \
		doc_html docfiles_html.txt doc_cpmhelp_html.txt \
		install_doc_html.sh

TEST	      =	test/prelim.z80 test/zexlax.pl test/zexall.z80 test/zexdoc.z80 \
		test/prelim.com test/zexall.com test/zexdoc.com \
		test/savage.pas test/savage.com test/sys.azm test/sys.com \
		test/timex.com \
		TODO test.c md5 md5_V1.10 di

DISTRIB       =	Makefile_linux_32_i586 \
		Makefile_linux_64_amd_athlon64 \
		Makefile_linux_64_intel_corei7 \
		Makefile_linux_64_intel_nocona \
		Makefile_linux_64_intel_skylake \
		Makefile_linux_64_intel_tigerlake \
		Makefile_solaris_cc-sparcv9 \
		Makefile_solaris_cc-x86_64 \
		Makefile_solaris_gcc-sparcv9 \
		Makefile_solaris_gcc-x86_64 \
		Makefile_freebsd \
		Makefile Makefile_first \
		Makefile_cygwin_32 \
		Makefile_cygwin_64_core2duo \
		Makefile_cygwin_64_corei7 \
		Makefile_cygwin_64_generic \
		Makefile_Orange_Pi_Plus2_ARMv7 \
		Makefile_radxa_Rock_RockPro_RockLite \
		Makefile_Raspberry_Pi_1 \
		Makefile_Raspberry_Pi_2_Model_B \
		Makefile_Raspberry_Pi_3_Model_B_ARMv7_32Bit \
		Makefile_Raspberry_Pi_3_Model_B_ARMv8_64Bit \
		Makefile_Raspberry_Pi_4_Model_B_ARMv7_32Bit \
		Makefile_Raspberry_Pi_4_Model_B_ARMv8_64Bit \
		Makefile_bananapim3_BPI-M3 \
		m2.ktt sample.ktt yaze.ktt \
		gen_windows yaze.bat .yazerc_winbinaries \
		.minttyrc md5summer.exe metapad.exe metapad.ini \
		simz80.pl .yazerc yaze.boot $(SRCS) $(DOC) \
		yaze-cpm3_80-columns_46-lines.boot \
		yaze.sh doc_ydskfiles.txt doc_ydskfiles_windows.txt \
		install_disks.sh \
		bios3.ydsk yaze-cpm3.boot \
		BOOT_UTILS.ydsk BOOT_UTILS_windows.ydsk testdsk.ydsk \
		CPM3_SYS.ydsk kermit_szrz.ydsk \
		zinc.ydsk spl.ydsk\
		test-utils-1.10.ydsk TODO \
		unixlike.ydsk turbo-modula-2.ydsk \
		hi-c-z280-compiler.ydsk mmu-utils.ydsk \
		bios3-zsm4.ydsk \
		zsm4-source.ydsk \
		disksort disksort.ydsk # $(TEST)

all:		yaze_bin cdm keytest

yaze_bin:	$(YAZE_OBJS)
		$(CC) $(CFLAGS) $(YAZE_OBJS) $(LIBS) -o $@
		ln -f -s yaze.sh yaze

keytest:	$(KT_OBJS)
		$(CC) $(CFLAGS) $(KT_OBJS) -o $@

simz80.c:	simz80.pl
		rm -f simz80.c
		perl -w simz80.pl >simz80.c
		chmod a-w simz80.c

cdm:		cdm.o
		$(CC) $(CFLAGS) cdm.o $(LIBS) -o $@

install:	all
		rm -rf $(LIBDIR)
		mkdir -p $(LIBDIR)
		mkdir -p $(MANDIR)
		$(INSTALL) -s -c -m 755 yaze_bin $(BINDIR)
		$(INSTALL) -s -c -m 755 cdm $(BINDIR)
		$(INSTALL) -c -m 755 yaze $(BINDIR)
		ln -f -s $(BINDIR)/yaze $(BINDIR)/cpm
		ln -f -s $(BINDIR)/yaze $(BINDIR)/z80
		$(INSTALL) -c -m 644 yaze-cpm3.boot $(LIBDIR)
		$(INSTALL) -c -m 644 yaze.boot $(LIBDIR)
		$(INSTALL) -c -m 644 m2.ktt $(LIBDIR)
		$(INSTALL) -c -m 644 sample.ktt $(LIBDIR)
		$(INSTALL) -c -m 644 yaze.ktt $(LIBDIR)
		$(INSTALL) -c -m 644 yaze.1 $(MANDIR)
		$(INSTALL) -c -m 644 cdm.1 $(MANDIR)
		$(INSTALL) -c -m 644 cpm.1 $(MANDIR)
		$(INSTALL) -c -m 644 z80.1 $(MANDIR)
		/bin/sh install_disks.sh $(DSKDIR)
		/bin/sh install_doc.sh $(DOCDIR)
		/bin/sh install_doc_html.sh $(DOCHTMLDIR)
		(cd $(LIBDIR); \
		rm -f md5 /tmp/md5.yaze-ag; \
		find . -type f -exec md5sum -b {} >>/tmp/md5.yaze-ag \; ; \
		mv /tmp/md5.yaze-ag md5)

md5:		$(DISTRIB)
		(P=`pwd`; D=`basename $$P`; \
			rm -f md5_$$D; \
			ls md5_y* >md5.list; \
			ls YAZE-AG-* >yaze-ag.list; \
			md5sum -b $(DISTRIB) `cat md5.list` \
						`cat yaze-ag.list` \
						`cat windows.txt` \
						`cat docfiles.txt` \
						`cat docfiles_html.txt` \
			| grep -v 'md5_$$D' >md5_$$D; )
		rm -f md5.list
		rm -f yaze-ag.list

tar:		$(DISTRIB) md5
		ls md5_y* >md5.list
		ls YAZE-AG-* >yaze-ag.list
		(P=`pwd`; D=`basename $$P`; cd ..; \
			for f in $(DISTRIB) `cat $$D/md5.list` `cat $$D/yaze-ag.list`; \
				do echo $$D/$$f; done \
			| tar -T - -cf - | gzip -9 >$$D/$$D.tar.gz)
		rm -f md5.list
		rm -f yaze-ag.list

clean:;		rm -f *.o *~ core
		rm -f *.i *.s
		rm -f yaze

yaze.o:		yaze.c simz80.h yaze.h mem_mmu.h ytypes.h
ybios.o:	ybios.c simz80.h yaze.h ybios.h mem_mmu.h ytypes.h ktt.h
mem_mmu.o:	mem_mmu.c mem_mmu.h simz80.h ytypes.h
simz80.o:	simz80.c simz80.h mem_mmu.h ytypes.h
simz80_wts.o:	simz80_wts.c simz80.h mem_mmu.h ytypes.h
ytimer.o:	ytimer.c ytypes.h
monitor.o:	monitor.c yaze.h ybios.h chan.h mem_mmu.h ytypes.h
keytran.o:	keytran.c ktt.h ytypes.h
conin.o:	conin.c ytypes.h ktt.h chan.h

keytest.o:	keytest.c ytypes.h ktt.h
nulrq.o:	nulrq.c

