License: GPLv2


Yet Another Z80 Emulator by AG (YAZE-AG)
========================================

yaze-ag-2.51.3.tar.gz is the stable release of version 2.51.3.1 with the
extension to run also with the cygwin environment under windows.

yaze-ag-2.51.3 runs with:

- Solaris (SparcV9 or X86_64Bit),
- Linux (LinuxMint, Ubuntu, Debian, ...),
- RaspberryPiOS (RaspberryPi 1, 2B, 3B, 3B+, 4B) (32 Bit or 64 Bit),
- Orange Pi Plus/Plus2,
- FreeBSD,
- MacOS,
- Cygwin (Windows) (since V2.30.2 also for 64Bit)

yaze-ag-2.51.3 is a further development of yaze-1.10.

How you install it is described in INSTALL-2.51.3 !!!


What is YAZE-AG?
================

YAZE-AG is an excellent Z80 emulator, written in ANSI C, that works on many
Unix architectures like Solaris, OSF, FreeBSD, Linux, Mac-OS-X and the cygwin
environment (Windows). It is fast, emulates undocumented opcodes, and passes
instruction regression tests against real Z80 hardware.

NEW in version 2.xx.x is the emulated Memory Management Unit (MMU) and the BIOS3
which supports CP/M 3.1 (or ZPM3 of Simeon Cran which is used in yaze-ag).

Yaze-ag-2.51.3 is a final release and I am thinking this version runs quite
stable. All known bugs are solved which are described under "Fixed Bugs" in
yaze-ag_doc.html.

There are new features (against version 1.10) which are described under
"FEATURES" in yaze-ag_doc.html too.

"Mounting directories" in yaze-ag.doc describes the special feature to import
directories into the emulator by connecting them to CP/M drives. That way
you can easily import files into the emulator. The time stamps in the Host
system (Unix/Linux/Windows/...) are converted on the fly into the simulated
CP/M directory. So you can use a make utility inside the emulator (see the
example "Disksort" in drive C: (connected to the Directory "./disksort") and
drive D: (connected to the YAZE-AG-Diskfile "disksort.ydsk"). Connected
Directories are always read only!

With the utilities W.COM and R.COM you can export and import files inside the
emulator. This feature is added since version 2.30.

The other way to export/import files out/into the emulator is the "cdm" utility
(CP/M disk manager) but you have to leave yaze-ag (see "man cdm").

If you find an error or you miss a feature so feel free and send an email
to me.

You will find yaze-ag-2.51.3 at

[http://yaze-ag.de](http://yaze-ag.de)

it's a redirect to

[https://www.mathematik.uni-ulm.de/users/ag/yaze-ag/](https://www.mathematik.uni-ulm.de/users/ag/yaze-ag/)


Look also to:

- INSTALL-2.51.3,
- yaze.doc (v1.10),
- yaze-ag.doc (only the new features since yaze-1.10 until yaze-ag-2.51.3),
- The README's,
- yaze.1 (man yaze),
- cdm.1  (man cdm).

March 4, 2022,
Dipl.-Ing. (FH) Andreas Gerlich
(University of Ulm, Germany)
