;234567890123456789012345678901234567890123456789012345678901234567890123456789
;        1         2         3         4         5         6         7
;
; This is the errormessage text for disksort.com
;
; ************************************************************************
; *                                                                      *
; *  (c) 10/1988 Michael Schewe      3119 Gollern Nr.17                  *
; *                                                                      *
; *  6/2004 translated to English by Andreas Gerlich                     *
; *            transduced  to common English by H.Jungkunz               *
; ************************************************************************
;
; Explanation of the structure:
;
; comment lines will start with a semi-colon 
; (only apart from the Output-Text)
; Error-type is located at the beginning of a line and refers to the
; C-Source-Code
; Text output hereto will have a SPACE (chr(32)) at the beginning of each line
;  and will stay like this until the next error type.
;
;usage
; Aufruf: disksort a: b: modus
; wobei a: das Laufwerk bezeichnet,welches physikalisch sortiert werden soll.
;       b: das Laufwerk bezeichnet, auf dem einige Zwischendateien angelegt
;          werden (Typ .SRT). Diese sind notwendig 
;	  nach dem Auftreten von best.Fehlern(z.b. Stromausfall waehrend des
;          Sortierens) fuer eine Rettung des Diskinhalts.
;          hier muss auch die Datei DISKSORT.MSG vorhanden sein.
;   modus: Ziffer zwischen 0 und 3 ,weitere Informationen mit disksort a: b: 4
;          anfordern !
;          modus kann entfallen (default=0)
; Die Laufwerke a: und b: muessen verschieden sein,sonst gibt es eine Fehler-
; meldung.
; Laufwerk b: sollte keine Ramdisk sein, damit die Korrekturdateien sicher
; erhalten bleiben (bei Stromausfall etc.).Es genuegt hierfuer eine normale
; Floppy.
; Das zu sortierende Laufwerk a: kann sowohl Floppy als auch Harddisk oder
; Ramdisk sein.
; Beim allerersten Programmlauf ist es sehr empfehlenswert,eine Diskette
; zu verwenden,die keine wichtigen Daten enthaelt ( bei Festplatte Backup
; machen ! )   sonst schlaegt Murphy wieder zu ...
;
usage
 
 Syntax:  disksort a: b: mode
 
 where a: means the drive to be physically sorted.
 where b: means the drive where temporary files (type .SRT) may be stored. 
            These are necessary after the occurance of some errors (for
            example power failure) to rescue disk contents.
            On this drive, the message file DISKSORT.MSG must be present!
 mode:  a number between 0 and 3 request more informations about
             modes with "disksort a: b: 4" .
             mode may be omitted (default=0)
 
 "a:" and "b:" must refer to different drives.
 Drive b: should not be a ramdisk. A floppy disk will be good enough.
 Drive a: can be a floppy disk, a hard disk or a ram disk.
 At the very first program run, it's recommended to use a floppy disk that has
 no important data. If you want to defrag a hard disk it's recommendable to
 make a backup of your data bevor you run disksort !!! 
 (Ever heard of Murphy's Law?)
;
;
;
;Ungueltige Laufwerke
; Eine oder beide Laufwerksangaben sind nicht richtig.
; Zulaessig sind 'a'...'p' und 'A'...'P' .
; Der Doppelpunkt nach dem Laufwerk kann entfallen.
;
Ungueltige Laufwerke
 One or both drive statements are bad.
 Usable ones are 'a'..'p' or 'A'..'P' .
 The colon may be omitted.
;
;
;
;Laufwerke sind gleich
; Beide Laufwerksangaben sind gleich.
; Dies ist zum Zwecke der Datenkorrektur im Fehlerfall nicht zulaessig
; Verschiedene Laufwerke angeben.
;
Laufwerke sind gleich
 Both drive statements refer to same disk!
 Use different drives.
;
;
;
;alles ok
; Disk - Sortieren erfolgreich abgeschlossen.
;
alles ok
 Disk sort completed successfully
;
;
;
;Speicherplatz
; Der Speicherplatz reicht leider nicht aus.
; Bei Ramdisks und Harddisks mit grossem Directory
; ( ueber 512 genutzte Eintraege) kann es eventuell zu Speicherplatzmangel
; kommen. Ausschlaggebend ist jedoch nur die Anzahl der genutzten Eintraege.
; In meinem CP/M 3 System mit 60k TPA ist es noch moeglich, eine Harddisk
; mit 768 Directory-Eintraegen zu sortieren. Die Kapazitaet des Laufwerks
; spielt hierbei kaum eine Rolle ( getestet mit 5 Mbyte,bis 20 Mbyte sollte
; es aber keine Probleme geben )
;
; Wenn moeglich mehr Freimachen ...
;
; Weitere Informationen ueber Speicherplatzbelegung mit disksort a: b: 1
; anfordern !
;
Speicherplatz
 The free space in the "Transient Programm Area (TPA)" is not big enough.
 On RAM disks and hard disks with large directories (more than 512 _used_ 
 entries)  it could occur that you run out of sorting space in memory.
 However, only the number of used entries is relevant.
 In the YAZE-AG system (Z80 emulator) with CP/M 3 and 62K TPA, it's still
 possible to sort a drive with 428 used directory entries. The capacity
 of a drive can be neglected  (tested with a 8 Mbyte disk file in YAZE-AG).
 
 If possible, free up some space ...
 
 You can get more information about space with "disksort a: b: 1"
;
;
;
;cpm 2.2
; Dieses Programm benoetigt die CP/M Version 2.2 oder hoeher.
;
cpm 2.2
 DISKSORT needs CP/M version 2.2 or higher.
;
;
;
;Blocknr
; Eine der Blocknummern,die im Directory-Eintrag die Lage der zur Datei
; gehoerenden Daten kennzeichnet ist ausserhalb des zulaessigen Bereichs !!!
; Wenn moeglich die zugehoerige Datei ausfindig machen und loeschen
; ( solche defekte Dateien koennen bei Programmfehlern oder -Abstuerzen
;   entstehen und die Dateien haben merkwuerdige Namen,die auch
;   Nicht-Buchstaben enthalten )
;
Blocknr
 One number of the data blocks is not within the acceptable range.
 ( such defect files can be created by program errors or system crashes.
   The name of those files are usually strange, often containing non-printing
   characters.
 )
;
;
;
;dir error
; Mehrere Directory-Eintraege verweisen auf den gleichen Datenblock.
; Dies ist zum sortieren nicht zulaessig.
; ( es gibt fuer das cp/m 2.2 ein Public-Domain Programm,welches diese
;   Eintraege erzeugt,damit Programme die von mehreren User-Bereichen
;   aus erreichbar sein sollen nur einmal Speicherplatz der Diskette
;   belegen.Falls dies der Fall ist, die ueberfluessigen Eintraege
;   mit ERA loeschen und danach einen Warmstart ausfuehren (^C)
; )
;
dir error
 More than one directory entry refers to the same data block!
 This is not valid for sorting.
 ( There is a public domain program for cp/m 2.2 which creates such
   directory entries. With such entries you can call a programm from
   different user areas but the program uses only one unique file-space.
   Please delete the superfluous entries with ERA and perform a warm boot (^C).
 )
;
;
;
;inv sort drive
; Ungueltiges Sortier - Laufwerk
; Systemfehler,sollte bereits vom BDOS frueher erkannt werden !!!
;
inv sort drive
 invalid drive to sort
 system error (this error should be detected by the BDOS earlier)
;
;
;
;Kein Platz auf Backuplaufwerk
; Auf dem Backuplaufwerk ist nicht genuegend Platz.
; Dort wird eine Kopie des Directory (OLDDIR.SRT) und eine CRC-Datei an-
; gelegt,wo fuer jede Datei die Pruefsumme ueber alle belegten Bloecke
; abgelegt wird (CRC.SRT, mit TYPE anschaubar).
; Etwa 80 Kbytes sollten auch fuer Festplatten ausreichen.
;
Kein Platz auf Backuplaufwerk
 Not enough space left on the backup drive!
 A copy of the directory (OLDDIR.SRT) and the CRC file
 (CRC.SRT) will be created. 
 CRC.SRT contains the CRCs of each allocated block of any file.
 80 kb space should be enough for hard disks.
;
;
;
;modus
; Der Modus darf nur zwischen 0 und 3 liegen.
; ( Aufruf disksort a: b: modus ) wobei modus eine Ziffer von 0 bis 3 ist.
; Fehlt modus beim Aufruf,so wird modus=0 angenommen.
; modus 0: Es werden Keine Erlauterungen ausgegeben.
; modus 1: Es wird der freie Speicherplatz waehrend des Programmlaufs und
;          einige andere Informationen ausgegeben.
; modus 2: Er dient nur Diagnosezwecken und gibt die Inhalte von Translate[i]
;          und back[i] aus.Zum Verstaendniss ist die Dokumentation DISKSORT.DOC
;          und die C-Quelle unerlaesslich
; modus 3: Er bildet fuer alle Files,die in der Datei CRC.SRT stehen die CRC's
;          und vergleicht sie mit den Aufgezeichneten. Bei Ungleichheit wird
;          fuer jeden Block " CRC-Error" ausgegeben.
;          Dieser Modus ist zum Feststellen der Brauckbarkeit einer Diskette
;          nach Programmfehlern gedacht.
;          Zum Wiederherstellen der alten Dateinhalte das Programm CORRECT.COM
;          aufrufen !!!
;
modus
 The mode is defined between 0 and 4
 ( call "disksort a: b: mode" )
 If mode is not given, mod=0 will be assumed.
 mode 0: There are no messages.
 mode 1: The space usage of the working memory (TPA) will be explained and
          some other information.
 mode 2: Is for debuging. Prints the contents of translate[i] and
          back[i]. You need the C source to understand it.
 mode 3: It builds the CRC of any block of all files in CRC.SRT and
          compares the CRCs. If the CRC of a block is mismatched, "CRC-error"
          will be printed.
          This mode is for testing a disk if disksort creates an error.
          To recall the former contents call the provided utility CORRECT.COM
 mode 4: Prints the usage about mode (this text).
;
;
;
;read
; BIOS Read Error !
; Programm abgebrochen. Zur Datenwiederherstellung CORRECT.COM aufrufen.
; Weitere Informationen auch mit DISKSORT a: b: 4 erhaeltlich.
;
read
 BIOS read error !
 Aborting program. For data recovery run CORRECT.COM.
 You can get also more information with DISKSORT a: b: 4
;
;
;
;write
; BIOS Write Error !
; Programm abgebrochen. Zur Datenwiederherstellung CORRECT.COM aufrufen.
; Weitere Informationen auch mit DISKSORT a: b: 4 erhaeltlich.
;
write
 BIOS write error !
 Aborting program. For data recovery run CORRECT.COM.
 You can get also more information with DISKSORT a: b: 4
;
;
;
;file crc.srt durcheinander
; Die Datei CRC.SRT passt nicht zum Disketteninhalt.
; Richtige Version besorgen.
;
file crc.srt durcheinander
 The file CRC.SRT doesn't match the contents of the disk.
 Please provide the correct version.
;
;
;
;Datenfile nicht gefunden
; Die Datei CRC.SRT passt nicht zum Disketteninhalt.
; Eine der in CRC.SRT genannten Dateien laesst sich nicht oeffnen.
; Richtige Version besorgen.
;
Datenfile nicht gefunden
 The file CRC.SRT doesn't match the contens of the disk.
 At least one file in CRC.SRT cannot be opened.
 Please provide the correct(ed) version. 
;
;
;
;crc error
; CRC-Error beim Vergleichen einer Datei.
; Weitere Informationen auch mit DISKSORT a: b: 4 erhaeltlich.
;
crc error
 CRC-Error in file comparison.
 get more information with DISKSORT a: b: 4
;
