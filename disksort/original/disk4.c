#include <stdio.h>
#include <disk.h>

extern char backup_drive;
extern char * baum_array, * baum_ptr;
extern int filecount, namen_ctr, array_runner, modus, timestamps;
extern unsigned max_bytes;
extern struct knoten *baum;
extern struct dir_eintrag **array;

char *filenamen[]={ "CCP     COM",
                    "CPM3????COM",
                    "????????SYS",
                    "??      SUB","????????COM","????????OV?",
/*
                    "????????LIB",
                    "????????MCD",
                    "????????MOD",
                    "????????C  ",
*/
                    "???????????"}; /* <- muss unbedingt zuletzt sein !!! */

char *get_mem(anzahl)
unsigned anzahl;
{
   if( (baum_ptr-baum_array + anzahl) <= max_bytes )
     {
        baum_ptr+=anzahl;
        return baum_ptr-anzahl;
     }
   else error("Speicherplatz");
}

push(ort,daten)          /* Dir-Eintrag in eine sortierte Liste haengen */
struct dir_eintrag **ort;
struct dir_eintrag *daten;
{
    while ( (*ort) && ( (**ort).eintrag.extent < daten->eintrag.extent) )
          ort=&(**ort).next;

    daten->next=*ort;
    *ort=daten;
}

einfuegen(tree,ptr)      /* Dir-eintrag in binaeren baum haengen */
struct dir_eintrag *ptr;       /* fuer jedes file ein knoten,die einzelnen */
struct knoten **tree;    /* extents sind in der Liste               */
{
    while (*tree) { int comp=compare(ptr, (**tree).dire_ptr );
                    if (comp==0) {
                                   push( &( (**tree).dire_ptr->next),ptr);
                                   return;
                                 }
                    tree=comp<0 ? &(**tree).links : &(**tree).rechts;
                  }
    *tree=get_mem (sizeof(struct knoten));
    (**tree).dire_ptr=ptr;
    ++filecount;
}

compare(eins,zwei)           /* vergleich von zwei Eintrag-Namen+usern */
char *eins,*zwei;            /* return-wert wie bei strcmp()           */
{
     int i;
     if (*eins==0x20) return -1;   /* dir-label ganz vorne hin */
     for (i=0;i<12;++i)
        {
          int hilfe=eins[i]-zwei[i];
          if (hilfe) return hilfe;
        }
     return 0;
}

baum_sort(ptr)             /* baum sortiert durchgehen */
struct knoten *ptr;
{
   if (ptr) { baum_sort(ptr->links);
              if (!ptr->erledigt)  ins_array(ptr);
              baum_sort(ptr->rechts);
            }
}

int passend(ptr)        /* feststellen,ob name auf den ptr im baum zeigt */
char *ptr;              /* auf die aktuelle Maske (z.b.????????.COM)     */
{   int i;              /* passt */
    for (i=0;i<11;++i)
      {  char ch;
         if((ch=filenamen[namen_ctr][i])!='?')
            if(ch!=(ptr[i] & 127)) return 0;          /* Flags loeschen !!! */
      }
    return 1;  /* passt */
}

ins_array(kptr)    /* wenn passend in Maske,ptr auf knoten in array ablegen */
struct knoten *kptr;
{
   if (passend(kptr->dire_ptr->eintrag.name))
       {
          (array)[array_runner++] =kptr->dire_ptr;
          kptr->erledigt=1;
       }
}

ordne()              /* alle Masken in <filenamen> durchgehen */
{
    if (modus)
       printf("Sortiere nach File-Typen ??.sub  *.com  *.ov?  uebrige ...\n");
    array_runner=0;
    for(namen_ctr=0;namen_ctr<DIM(filenamen);++namen_ctr)  baum_sort(baum);
}

error(text)            /* ausfuehrliche Meldungen aus DISKSORT.MSG ausg. */
char *text;
{
 FILE errdatei;
 char zeile[129];
  reset();                            /* selects a:       */
  assign(errdatei,"a:disksort.msg");
  errdatei.DRIVE=backup_drive;
  fopen(errdatei);
  if (errdatei.eof)
   {
      printf("\nDISKSORT.MSG - Fehlermeldungsdatei auf dem Backup-Laufwerk\n");
      printf("nicht gefunden. ERROR:\n%s\n",text);
      my_exit();
   }
  errdatei.report=1;
  while(!errdatei.eof)
    {
      fgets(errdatei,zeile);
      if(!strcmp(zeile,text))
         while(!errdatei.eof)
             {
               fgets(errdatei,zeile);
               if ((!*zeile)||(*zeile==' '))  printf("%s\n",zeile);
                 else goto ende;
             }
    }
  printf("ERROR: %s\n\nWeitere Erlauterungen nicht vorhanden.\n",text);
ende:;
  my_exit();
}
