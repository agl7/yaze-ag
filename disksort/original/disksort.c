#include <stdio.h>
#include <disk.h>

/*  disk-sortierprogramm v1.0 (c) 1988 M.Schewe  */
/* besteht aus disk1.c disk2.c disk3.c disk4.c disksort.c disk.h */

/* #define vorsicht         /* auch c: (harddisk) zulassen -> wenn undefined */

char *buffer, * baum_array, * baum_ptr;
int namen_ctr, array_runner, anfangsnr, version, blocksize, sectorsize;
int woerter;
int old_drive, old_user, dir_counter, timestamps;
int *translate, *back;
unsigned old_crc, max_bytes;
struct dpb disk;
struct dir_eintrag **array, *directory;
FILE datei;

struct knoten *baum=0;
char backup_drive=0;
int modus=0;
int drive=0;
int filecount=0;
/* int vorsicht=1;       /* c:auch erlaubt,wenn = 0 !!! */

extern unsigned int space();

/***************************  Funktionen  ******************************/

make_translate_table()
{
  static int index,i,maxzahl;
  index=0;
  maxzahl=woerter? 8:16;
  {
     unsigned int i=disk.al;             /* anfangs-blocknr.berechnen */
     while (i){  if (i MOD 2)  ++index;
                 i/=2;
              }
     anfangsnr=index;
  }
  for(i=0;i<filecount;++i)                            /* alle files   */
   if (array[i]->eintrag.user <16 )
     {
       struct dir_eintrag *ptr;
       for(ptr=array[i];ptr;ptr=ptr->next)            /* alle entries */
          {
             int j;
             for(j=0;j<maxzahl;++j)
                {  int nummer;
                   if(woerter) nummer=ptr->eintrag.block.wort[j];
                     else      nummer=ptr->eintrag.block.byte[j];
                   if (nummer)
                     {
                        translate[index]=nummer;
                        if(woerter) ptr->eintrag.block.wort[j]=index;
                           else     ptr->eintrag.block.byte[j]=index;
                        ++index;
                     }
                     else break;       /* verlaesst innere for-schleife */
                }
          }
     };
}

sort_back()
{
   int i;
   for(i=anfangsnr;i<disk.dsm+1;++i)
     {
       int wert=translate[i];
       if (wert)
         {
            if ((wert>disk.dsm)||(wert<anfangsnr)) error("Blocknr");
            if (back[wert])
               {
                  printf("\nBlocknummer=%d\n",wert);
                  error("dir error");
               }
            back[wert]=i;
            if (wert==i)           /* Block schon da,wohin er gehoert ? */
              {
                 translate[i]=0; /* dann ueberfuessige exchange-operationen */
                 back[i]=0;      /* vermeiden !      */
              }
         }
     }
}

int get_block()
{
   int i;
   for (i=1;i<(disk.dsm+1);++i)  if (translate[i]) return i;
   return 0;                /* nichts mehr da ... */
}

rep()
{
   int i;
   printf("\n");
   for (i=0;i<(disk.dsm+1);++i)
     printf("i=%d  translate[i]=%d  back[i]=%d\n",i,translate[i],back[i]);
   printf("\n");
}

exchange(buffer,zielnr,istnr,ex_buf)
char **buffer;
int zielnr,istnr;
char **ex_buf;
{
   if (zielnr!=istnr)
      {
         read(*ex_buf,zielnr);
         write(*buffer,zielnr);
         {
            char *hilfe=*buffer;
            *buffer=*ex_buf;
            *ex_buf=hilfe;
         }
      }
}

comm_operation(i,buffer,ex_buf)
int i;
char **buffer,**ex_buf;
{
   int contents=i;
   read(*buffer,i);
   while(back[i])
     {
        exchange(buffer,back[i],contents,ex_buf);
        contents=back[i]; /* austausch merken */
        { int hilfe=i;
          i=back[i];
          translate[i]=0;  /* erledigte Arbeit merken */
          back[hilfe]=0;   /* s.o.                    */
        }
     }
}

sort_daten()       /* alle Daten der disk sortieren (dauert etwas ... ) */
{
   char *buffer=my_alloc(blocksize);
   char *ex_buf=my_alloc(blocksize);
   int i;
   if (modus==2) rep();
   if (modus) printf("Sortiere die Datenbloecke (dauert etwas) ...\n");
   while(i=get_block())
      {
         comm_operation(i,&buffer,&ex_buf);
         while(translate[i]) i=translate[i];
         comm_operation(i,&buffer,&ex_buf);
      }
   cfree(buffer);
   cfree(ex_buf);
}

main(argc,argv)
int argc;
char *argv[];
{
printf("DISKSORT.C  (c) Copyright 1988 by Michael Schewe  3119 Gollern Nr.17\n");
   old_user=get_user();
   old_drive=(*( (char *)4 ) ) &15;
   if (argc>2)
                {  drive=upcase(argv[1][0])-'A'+1;
                   backup_drive=upcase(argv[2][0])-'A'+1;
                }
      else error("usage");
   if ((drive>16)||(drive<1)||(backup_drive<1)||(backup_drive>16))
         error("Ungueltige Laufwerke");
   if (!(drive-backup_drive)) error("Laufwerke sind gleich");
   if (argc==4) modus=argv[3][0]-'0';
   if ((modus<0)||(modus>3)) error("modus");
   if (modus==3) {
                   setdisk();
                   check_crc();
                   my_exit();
                 }
   if (!modus) printf("Bitte etwas gedulden und nicht Abbrechen !!!\n");
   setdisk();
   if (modus) printf ("freier Arbeitsspeicher zu Beginn:%u Bytes\n",space());
    {
       int anzahl=disk.dsm+1;
       translate=my_alloc(4 * anzahl);
       back=translate+anzahl;          /* beide gleich gross      */
                                       /* translate ist int *     */
    }
   read_dir();      /* baut auch sortierten baum auf + my_alloc fuer array */
   ordne();         /* baut array auf                */
   if (modus)  printf("Minimaler freier Speicher:%u bytes\n",space() );
   cfree( baum_array );
   make_translate_table();    /* translate-array aufbauen,erstellt auch  */
                              /* neue directory-blocknummern             */
   sort_back();               /* umkehrung von translate herstellen      */
                              /* testet auch dir auf korrektheit         */
   if (modus)  printf("Pufferspeicher fuer CRC: %u bytes\n",space() );
   crc_check();
   write_dir();               /* neues directory wegschreiben            */
   cfree(array);
   cfree(directory);            /* dirbuffer freigeben      */
   if (modus) printf ("freier Datenpuffer:%u Bytes\n",space());
   sort_daten();              /* greift auf translate[] und back[] zurueck */
   cfree(translate);     /* macht auch cfree(back) ; s.o. */
   if (modus) printf ("freier Arbeitsspeicher am Ende:%u Bytes\n",space());
   check_crc();
   error("alles ok");
}
