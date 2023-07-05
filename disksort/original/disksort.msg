; Dies ist die Fehlermeldungdsdatei zu disksort.com
;
; ************************************************************************
; *                                                                      *
; *  (c) 10/1988 Michael Schewe      3119 Gollern Nr.17                  *
; *                                                                      *
; ************************************************************************
;
; Erlauterungen zum Aufbau:
;
; Kommentarzeilen beginnen mit einem Semikolon ; (nur ausserhalb des Ausgabe-
; Textes)
; Fehlertyp steht ganz am Anfang einer Zeile und bezieht sich auf die C-Quelle
; Text der hierzu ausgegeben wird,hat an jedem Zeilenanfang ein Leerzeichen
; (chr(32)) und geht bis zum naechsten Fehlertyp

usage
 Aufruf: disksort a: b: modus
 wobei a: das Laufwerk bezeichnet,welches physikalisch sortiert werden soll.
       b: Laufwerk,auf dem einige Zwischendateien angelegt werden (Typ .SRT),
          die
          nach dem Auftreten von best.Fehlern(z.b. Stromausfall waehrend des
          Sortierens) fuer eine Rettung des Diskinhalts notwendig sind.
          hier muss auch die Datei DISKSORT.MSG vorhanden sein.
   modus: Ziffer zwischen 0 und 3 ,weitere Informationen mit disksort a: b: 4
          anfordern !
          modus kann entfallen (default=0)
 Die Laufwerke a: und b: muessen verschieden sein,sonst gibt es eine Fehler-
 meldung.
 Laufwerk b: sollte keine Ramdisk sein, damit die Korrekturdateien sicher
 erhalten bleiben (bei Stromausfall etc.).Es genuegt hierfuer eine normale
 Floppy.
 Das zu sortierende Laufwerk a: kann sowohl Floppy als auch Harddisk oder
 Ramdisk sein.
 Beim allerersten Programmlauf ist es sehr empfehlenswert,eine Diskette
 zu verwenden,die keine wichtigen Daten enthaelt ( bei Festplatte Backup
 machen ! )   sonst schlaegt Murphy wieder zu ...
;
;
Ungueltige Laufwerke
 Eine oder beide Laufwerksangaben sind nicht richtig.
 Zulaessig sind 'a'...'p' und 'A'...'P' .
 Der Doppelpunkt nach dem Laufwerk kann entfallen.
;
;
Laufwerke sind gleich
 Beide Laufwerksangaben sind gleich.
 Dies ist zum Zwecke der Datenkorrektur im Fehlerfall nicht zulaessig
 Verschiedene Laufwerke angeben.
;
;
alles ok
 Disk - Sortieren erfolgreich abgeschlossen.
;
Speicherplatz
 Der Speicherplatz reicht leider nicht aus.
 Bei Ramdisks und Harddisks mit grossem Directory
 ( ueber 512 genutzte Eintraege) kann es eventuell zu Speicherplatzmangel
 kommen. Ausschlaggebend ist jedoch nur die Anzahl der genutzten Eintraege.
 In meinem CP/M 3 System mit 60k TPA ist es noch moeglich, eine Harddisk
 mit 768 Directory-Eintraegen zu sortieren. Die Kapazitaet des Laufwerks
 spielt hierbei kaum eine Rolle ( getestet mit 5 Mbyte,bis 20 Mbyte sollte
 es aber keine Probleme geben )

 Wenn moeglich mehr Freimachen ...

 Weitere Informationen ueber Speicherplatzbelegung mit disksort a: b: 1
 anfordern !
;
cpm 2.2
 Dieses Programm benoetigt die CP/M Version 2.2 oder hoeher.
;
Blocknr
 Eine der Blocknummern,die im Directory-Eintrag die Lage der zur Datei
 gehoerenden Daten kennzeichnet ist ausserhalb des zulaessigen Bereichs !!!
 Wenn moeglich die zugehoerige Datei ausfindig machen und loeschen
 ( solche defekte Dateien koennen bei Programmfehlern oder -Abstuerzen
   entstehen und die Dateien haben merkwuerdige Namen,die auch
   Nicht-Buchstaben enthalten )
;
;
dir error
 Mehrere Directory-Eintraege verweisen auf den gleichen Datenblock.
 Dies ist zum sortieren nicht zulaessig.
 ( es gibt fuer das cp/m 2.2 ein Public-Domain Programm,welches diese
   Eintraege erzeugt,damit Programme die von mehreren User-Bereichen
   aus erreichbar sein sollen nur einmal Speicherplatz der Diskette
   belegen.Falls dies der Fall ist, die ueberfluessigen Eintraege
   mit ERA loeschen und danach einen Warmstart ausfuehren (^C)
 )
;
;
inv sort drive
 Ungueltiges Sortier - Laufwerk
 Systemfehler,sollte bereits vom BDOS frueher erkannt werden !!!
;

Kein Platz auf Backuplaufwerk
 Auf dem Backuplaufwerk ist nicht genuegend Platz.
 Dort wird eine Kopie des Directory (OLDDIR.SRT) und eine CRC-Datei an-
 gelegt,wo fuer jede Datei die Pruefsumme ueber alle belegten Bloecke
 abgelegt wird (CRC.SRT, mit TYPE anschaubar).
 Etwa 80 Kbytes sollten auch fuer Festplatten reichen.
;
;
modus
 Der Modus darf nur zwischen 0 und 3 liegen.
 ( Aufruf disksort a: b: modus ) wobei modus eine Ziffer von 0 bis 3 ist.
 Fehlt modus beim Aufruf,so wird modus=0 angenommen.
 modus 0: Es werden Keine Erlauterungen ausgegeben.
 modus 1: Es wird der freie Speicherplatz waehrend des Programmlaufs und
          einige andere Informationen ausgegeben.
 modus 2: Er dient nur Diagnosezwecken und gibt die Inhalte von Translate[i]
          und back[i] aus.Zum Verstaendniss ist die Dokumentation DISKSORT.DOC
          und die C-Quelle unerlaesslich
 modus 3: Er bildet fuer alle Files,die in der Datei CRC.SRT stehen die CRC's
          und vergleicht sie mit den Aufgezeichneten. Bei Ungleichheit wird
          fuer jeden Block " CRC-Error" ausgegeben.
          Dieser Modus ist zum Feststellen der Brauckbarkeit einer Diskette
          nach Programmfehlern gedacht.
          Zum Wiederherstellen der alten Dateinhalte das Programm CORRECT.COM
          aufrufen !!!
;
read
 BIOS Read Error !
 Programm abgebrochen. Zur Datenwiederherstellung CORRECT.COM aufrufen.
 Weitere Informationen auch mit DISKSORT a: b: 4 erhaeltlich.
;
write
 BIOS Write Error !
 Programm abgebrochen. Zur Datenwiederherstellung CORRECT.COM aufrufen.
 Weitere Informationen auch mit DISKSORT a: b: 4 erhaeltlich.
;
file crc.srt durcheinander
 Die Datei CRC.SRT passt nicht zum Disketteninhalt.
 Richtige Version besorgen.
;
Datenfile nicht gefunden
 Die Datei CRC.SRT passt nicht zum Disketteninhalt.
 Eine der in CRC.SRT genannten Dateien laesst sich nicht oeffnen.
 Richtige Version besorgen.
;
crc error
 CRC-Error beim Vergleichen einer Datei.
 Weitere Informationen auch mit DISKSORT a: b: 4 erhaeltlich.
;