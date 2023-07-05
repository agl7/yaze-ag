#include <stdio.h>
#include <disk.h>

extern int old_user, blocksize, modus, filecount, drive;
extern unsigned old_crc;
extern char *buffer;
extern char backup_drive;
extern FILE datei;
extern struct dir_eintrag **array;

int anzahl;

crc(datei,user)
FILE *datei;
int user;
{
  int i;
  int tab=10;
  set_user(old_user);
  if (user>9) printf("1");
    else printf("0");
  printf("%d  ",user % 10);

  for(i=0;i<11;++i) printf("%c",(datei->NAME[i])&127);
  set_user(user);
  while(anzahl=b_read(datei,buffer,blocksize/128))
     {
       old_crc=0;
       set_user(old_user);
       if (tab==10)  {
                       printf("\n");
                       tab=0;
                    }
         else printf(" ");
       printf("%x",acrc());
       ++tab;
       set_user(user);
     }
  set_user(old_user);
  printf("\n\n");
}

acrc()                 /* crc generieren */
{
#asm
      ld    a,(anzahl)
      ld    c,a
      ld    hl,(old_crc)    ; crc
      ld    de,(buffer)
l1:   ld    b,128
l2:   add   hl,hl
      jr    c,l10
;
      ld    a,(de)
      add   a,l
      ld    l,a
      inc   de
      djnz  l2
;
      dec   c
      jr    nz,l1
;
      jr    l20
;
l10:  ld    a,(de)
      add   a,l
      xor   097h
      ld    l,a
      ld    a,h
      xor   0a0h
      ld    h,a
      inc   de
      djnz  l2
;
      dec   c
      jr    nz,l1
;
l20:  ld    (old_crc),hl  ;hl=crc
      ; ret wird durch '}' realisiert
#endasm
}

crc_check()
{
  FILE out;
  int i;
  if (modus) printf("Erstelle die CRC-Datei CRC.SRT (dauert etwas) ...\n");
  assign(out,"a:crc.srt");
  out.report=1;
  out.DRIVE=backup_drive;
  rewrite(out);
  red_out(out);
  buffer=my_alloc(blocksize);
  for (i=0;i<filecount;++i)
    {
       struct entry *ptr=&( array[i]->eintrag );
       if (ptr->user < 16)
         {
           set_user(ptr->user);
           assign(datei,"a:nn.bin");
           move(ptr,datei,12);
           datei.DRIVE=drive;
           datei.report=1;
           datei.direct_flag=1;
           fopen(datei);
           crc(datei,ptr->user);
           set_user(old_user);
         }
    }
  printf("\n");
  red_out(0);
  weof(out);
  fclose(out);
  cfree(buffer);
}

check_crc()            /* CRC's am Ende noch einmal vergleichen ... */
{
  if (modus) printf("Vergleiche die CRC's mit den Files (dauert etwas)...\n");
   reset();
   buffer=my_alloc(blocksize);
   assign(datei,"a:crc.srt");
   datei.report=1;
   datei.DRIVE=backup_drive;
   fopen(datei);
   red_input(datei);
    {
      FILE quelle;
      static char ch;
      static int user,crcwert,err;
      while(!datei.eol)
       {
          err=scanf("%d",&user);
          if (!err) error("file crc.srt durcheinander");
          scanf("%c",&ch);
          assign(quelle,"a:nn.bin");
          quelle.report=1;
          quelle.direct_flag=1;
          quelle.DRIVE=drive;
           { int i;
             for (i=0;i<11;++i)
               {
                  scanf("%c",&(quelle.NAME[i]));
                  if (modus>1) printf("%c",quelle.NAME[i]);
               }
           }
          scanf("%c",&ch);
          set_user(user);
          fopen(quelle);
          set_user(old_user);
          while(scanf("%x",&crcwert))
            {
               anzahl=blocksize/128;
               set_user(user);
               anzahl=b_read(quelle,buffer,anzahl);
               if (!anzahl) error("Datenfile nicht gefunden");
               set_user(old_user);
               old_crc=0;
               if (crcwert!=acrc())
                {
                  if (modus==3) printf(" CRC-Error");
                    else error("crc error");
                }
           }
         if (modus==2) printf("  o.k.");
         if (modus>1) printf("\n");
       }
    }
   red_input(0);
   cfree(buffer);
 if (modus&&(modus<3))
   printf("Alle Files haben exakt die gleichen Daten wie vorher.\n");
}
