#include <stdio.h>
#include <disk.h>

extern int blocksize, version, old_user, old_drive;
extern int modus, drive, woerter, sectorsize;
extern FILE datei;
extern struct dpb disk;

char *my_alloc(n)             /* wie malloc(),aber mit fehlerabfrage */
unsigned n;
{
  char * new=calloc(1,n);     /* setzt alle speicherstellen auf 0 (wichtig) */
  if (new) return new;
  error("Speicherplatz");
}

int bdos_hl(nummer,de)                   /* returnt HL-registerinhalt */
int nummer,de;
{
#asm
        pop  hl  ; ruecksprungadr
        pop  bc  ; c=nummer
        pop  de
        push de
        push bc
        push hl
        call 5
#endasm
}

int bdos_a(nummer,de)                 /* returnt A-registerinhalt */
int nummer,de;
{
#asm
        pop  hl  ; ruecksprungadr
        pop  bc  ; c=nummer
        pop  de
        push de
        push bc
        push hl
        call 5
        ld   h,0
        ld   l,a
#endasm
}

move(hl,de,bc)              /* im speicher BC bytes von HL nach DE kopieren */
int hl,de,bc;
{
#asm
        pop  ix   ; returnadr
        pop  hl
        pop  de
        pop  bc
        push bc
        push de
        push hl
        push ix
        ldir
#endasm
}

unsigned int space()       /* verbleibenden Speicher ermitteln */
{
  int * ptr=malloc(blocksize);
  if (!ptr) return 0;                        /* testet in vielfachen von */
  else {                                     /* blocksize       */
         unsigned int x=space()+blocksize;
         cfree(ptr);
         return x;
       }
}


potenz2(x)          /* zweierpotenz von x berechnen */
int x;
{ if (x) return 2*potenz2(x-1);
     else return 1;
}

int bios_a(nummer,a,bc,de)          /* BIOS -aufruf ueber BDOS-func.50 */
int nummer,a,bc,de;
{
#asm
       pop  ix
       ld   (rueck),ix
       call bios_hl
       ld   h,0
       ld   l,a                ; return a-register
       ld   ix,(rueck)
       jp   (ix)
       dseg
rueck: dw 0
       cseg
#endasm
}

int bios_hl(nummer,a,bc,de)
int nummer,a,bc,de;
{
   static struct biospb {   BYTE func;
                            BYTE areg;
                            WORT bcreg;
                            WORT dereg;
                            WORT hlreg;
                        } parameter;
  if (version==2) return bios2hl(3*(nummer-1),a,bc,de);
   else
    {
       parameter.func=nummer;
       parameter.areg=a;
       parameter.bcreg=bc;
       parameter.dereg=de;
       parameter.hlreg=0;
       return bdos_hl(50,parameter);         /* direct bios call    */
    }
}

bios2hl(offs,a,bc,de)
int offs,a,bc,de;
{
#asm
        ld   hl,(1)
        pop  ix      ; ruecksprungadr
        pop  bc      ; offs
        add  hl,bc
        pop  bc      ; a
        ld   a,c
        pop  bc      ;bc
        pop  de      ;de
        push de
        push bc
        push af
        push bc
        push ix
        jp   (hl)
#endasm
}

char upcase(ch)
char ch;
{   if ((ch>='a')&&(ch<='z')) ch-='a'-'A';
    return ch;
}

reset()
{
  bdos_hl(13,0);            /* reset disk system (wegen pufferung von cpm3 */
  bdos_hl(14,old_drive);                 /* select old drive as default */
  set_user(old_user);
  red_out(0);
  red_in(0);
}

my_exit()
{
  reset();
  exit();
}

set_user(nr)
int nr;
{
  bdos_a(32,nr);
}

int get_user()
{
  return bdos_a(32,0xff);
}

fgets(datei,zeile)
FILE *datei;
char *zeile;
{
   while(!(datei->eol))  *(zeile++)=getc(datei);
   *zeile=0;         /* ende setzen   */
   if(datei->eof) return;

   getc(datei);      /* CR ueberlesen */
   getc(datei);      /* LF ueberlesen */
}

setdisk()                      /* Systeminitialisierungen,setzt einige */
{                              /* variablen    */
  struct dpb *sys_dpb;
  int vers=bdos_hl(12,0);
  if (modus)
    printf("Untersuche Betriebssystem (cpm2.2 oder 3) und Disks . ");
  if (vers<0x30)    version=2;
     else           version=3;
  if (modus) printf("CP/M %d erkannt.\n",version);
  if (vers<0x22) error("cpm 2.2");
  bdos_hl(14,drive-1);                       /* select drive as default */
  sys_dpb=bdos_hl(31,0);                     /*  get addr diskparameter */
  move(sys_dpb,disk,sizeof(struct dpb));    /*  copy it to variable disk */
  if (version==2) disk.psh=disk.phm=0;
                                            /* variablen berechnen */
  woerter=(disk.dsm>=256);                  /* wort oder byte fuer blocknr ? */
  blocksize=128*potenz2(disk.bsh);
  sectorsize=128*potenz2(disk.psh);
  if (modus) {
                printf("Blockgroesse= %d Bytes\n",blocksize);
                printf("phys. Sectorgroesse=%d Bytes\n",sectorsize);
             }
}
