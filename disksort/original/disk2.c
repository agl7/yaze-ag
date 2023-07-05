#include <stdio.h>
#include <disk.h>

extern char backup_drive;
extern char * baum_array, * baum_ptr;
extern int modus, blocksize, sectorsize, filecount;
extern int /* vorsicht,*/ drive, version, dir_counter, timestamps;
extern unsigned max_bytes;
extern struct dpb disk;
extern struct knoten *baum;
extern struct dir_eintrag **array, *directory;
extern FILE datei;

struct entry erased={0xe5};
struct entry sfcb={0x21};
int sect_anzahl;   /* Anzahl der Directory-Sektoren von read_dir ermittelt */

int enthaltene_eintraege(buffer)
char *buffer;
{
   int count=0;
   int i=0;
   for (;i<sectorsize;i+=32)   if ( buffer[i] < 0x21 ) ++count;
   return count;
}

read_dir()
{
   static unsigned int dirspace,bytes;
   static int dirbloecke,sectornummer,counter,eintraege;
   static char *buffer;

   if (modus) printf("Lese Directory ...\n");
   dirspace=(disk.drm+1)*32;
   dirbloecke=dirspace DIV blocksize+((dirspace MOD blocksize)!=0);
   buffer=my_alloc(sectorsize);
   sectornummer=eintraege=0;
   sect_anzahl=counter=dirbloecke * ( blocksize DIV sectorsize );

   assign(datei,"a:olddir.srt"); /* directory fuer datenwiederherstellungs-*/
   datei.DRIVE=backup_drive;      /* zwecke sichern */
   datei.report=1;
   datei.direct_flag=1;
   rewrite(datei);

   while (counter--)    /* anzahl (ohne timestamps) der eintraege ermitteln */
     {
         int anzahl=sectorsize DIV 128;
         readsector(buffer,sectornummer++);
         if (sectornummer==1) timestamps=(buffer[96]==0x21);
         bios_hl(9,0,backup_drive-1,1);      /* select disk  */
         if ( anzahl!=b_write(datei,buffer,anzahl) )
                            error("Kein Platz auf Backuplaufwerk");
         eintraege+=enthaltene_eintraege(buffer);
     }
   fclose(datei);
   if (modus) printf("Timestamps %s vorhanden.\n",timestamps ? "":"nicht");
   directory=my_alloc( bytes=eintraege * sizeof (struct dir_eintrag) );
   dir_counter=sectornummer=0;
   counter=sect_anzahl;
   while (counter--)             /* Eintraege in directory[] kopieren */
    {
      int i=0;
      readsector (buffer,sectornummer++);
      for (;i<sectorsize;i+=32)
       if (buffer[i] < 0x21)
         {
           if (timestamps)
             {
               static unsigned int quelle,ziel;
               int nummer=(i MOD 128) DIV 32;
               int sfcb=i+ 96 - (i MOD 128);
               if ( buffer[sfcb]!=0x21 )
                  error ("Nur teilweise SFCB's vorhanden");
               quelle=&( buffer[sfcb+1+10*nummer] );
               ziel=&( directory[dir_counter].time );
               move(quelle,ziel,10);
             }
           move (&buffer[i],&directory[dir_counter++],32);
        }
   }
   bios_hl(9,0,backup_drive-1,1);      /* select disk  */
   cfree(buffer);
   array=my_alloc(2*dir_counter);      /* ist evtl. etwas zu gross */
   if (modus)
    {
      printf ("%d Dir-Bloecke belegen %u Bytes\n",dirbloecke,bytes);
      printf("Sortiere Eintraege in einem Binaeren Baum ...\n");
    }
   max_bytes=sizeof(struct knoten) * dir_counter;
   baum_array=my_alloc( max_bytes );
   baum_ptr=baum_array;
           {
              int i;
              struct dir_eintrag *ptr=directory;
              for (i=0; i<dir_counter; ++i)  einfuegen (&baum,ptr++);
          }
}

write_dir()
{
   static char *buffer,*walker;
   static int i,nr,dirsektoren;
   static BYTE a_reg;
   dirsektoren=sect_anzahl;
   a_reg=nr=0;
   walker=buffer=my_alloc(sectorsize);
   if (modus)
    {  printf("Schreibe neues Directory weg. Ab jetzt sind alle Abbrech-");
       printf("Versuche\nzu Unterlassen !!!\n");
    }
   for(i=0;i<filecount;++i)                    /* alle files durchgehen */
     {
        struct dir_eintrag *ptr;
        for(ptr=array[i];ptr;ptr=ptr->next)   /* alle extents durchgehen */
          {
             move( &(ptr->eintrag),walker,32);
             walker+=32;
             ++a_reg;
             if (timestamps)
               {
                 int sfcboffs=64 - ( (walker-32-buffer) MOD 128);
                 char *ziel=walker+sfcboffs+1+10 * (a_reg-1);
                 move( &(ptr->time),ziel,10);
                 if (a_reg==3 )
                   {
                     *walker=0x21;
                     *(walker+31)=0;
                     walker+=32;
                     a_reg=0;
                   }
               }
             if (walker>=(buffer+sectorsize))
               {
                  writesector(buffer,nr);
                  ++nr;
                  --dirsektoren;
                  walker=buffer;
               }
          }
     }
   while(dirsektoren--)       /* rest auffuellen mit erased */
     {
        while(walker<(buffer+sectorsize))
          {
             move(erased,walker,32);
             ++a_reg;
             if (timestamps && a_reg==4)
               {
                 a_reg=0;
                 move(sfcb,walker,32);
               }
             walker+=32;
          }
        writesector(buffer,nr);
        ++nr;
        walker=buffer;
     }
  cfree(buffer);
}

read(buffer,blocknr)
char *buffer;
unsigned int blocknr;
{
  common(buffer,blocknr,0);
}

write(buffer,blocknr)
char *buffer;
unsigned int blocknr;
{
#ifdef vorsicht
   error ("nicht auf Harddisk !!!");
#endif
   common(buffer,blocknr,1);
}

readsector(buffer,sectornummer)
char *buffer;
unsigned int sectornummer;
{
  common(buffer,sectornummer,2);
}

writesector(buffer,sectornummer)
char *buffer;
unsigned int sectornummer;
{
#ifdef vorsicht
   error ("nicht auf Harddisk !!!");
#endif
   common(buffer,sectornummer,3);
}

common(buffer,blocknr,operation)
char *buffer;
unsigned int blocknr;
int operation;
{
  static int erro,anzahl;
  static unsigned int track,sector,sectornr,faktor;

  anzahl= (operation < 2) ? (blocksize DIV sectorsize) : 1;
  if (version==3)
     {
       bios_a(23,0,anzahl,0);       /* multisectorcount setzen */
       bios_a(28,1,0,0);                /* dma-bank=1 */
     }
  sectornr=blocknr*anzahl;
  faktor=disk.spt/potenz2(disk.psh);
  while (anzahl--)                   /* ein block=mehrere Sektoren */
    {
       static int skew_table;
       static int dph;
       dph=bios_hl(9,0,drive-1,1);      /* select disk  */
       if (!dph) error("inv sort drive");
       skew_table=get_byte(dph)+256*get_byte(dph+1);
       bios_a(12,0,buffer,0);                         /* set dma adress */
       track=sectornr DIV faktor;
       sector=sectornr MOD faktor;
       sector=bios_hl(16,0,sector,skew_table);        /* translate sector */
       track+=disk.off;
       bios_a(10,0,track,0);                          /* set track */
       bios_a(11,0,sector,0);                         /* set sector */
       if (operation MOD 2)
         {
           if (bios_a(14,0,0,0)) error("write");
         }
          else if (bios_a(13,0,0,0)) error("read");
       ++sectornr;
       buffer+=sectorsize;
    }
   if (version==3) bios_a(23,0,1,0);           /* multisectorcount=1 */
}

int get_byte(adr)                           /* holt ein Byte aus Bank 0 */
char *adr;
{
   /*   Problem beim Bankswitched cpm 3 :
        von Bank 1 aus ist die Adresse der Sector-translate-tabelle
        nicht zu bekommen,weil der Disk parameter Header in Bank 0
        residiert.Durch einen Trick ist es aber moeglich,mit Hilfe
        der Routine SECTRN im BIOS einzelne Bytes aus Bank 0 zu lesen...
   */

  if (adr < 0xF000) error ("Disk Parameter Header nicht im Common");

  return *adr;

/*
  if (version==3) return bios_hl(16,0,0,adr);
                               * nutzt BIOS - SECTRN -Routine *
  else return *adr;        * fuer cpm2 *
*/

}
