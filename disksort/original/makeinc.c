#include <stdio.h>

extern FILE in_file;
extern char zeile [128];
extern int test;

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

int get_wort(zeile)
char *zeile;
{
   static char ch;
   static int string;
   if (!in_file.eof)
     {
       while( ch=getc(in_file), ( (ch==13)||(ch==' ') ) && (!in_file.eof) )
        {
           if (ch==13) getc(in_file);
        }
     }                   /* bis zum ersten buchstaben blanks ueberlesen */
    if (!in_file.eof)
     {
       char abbr;
       string=(ch=='"');
       abbr=string ? '"' : ' ';
       if (string) ch=getc(in_file);        /* " ueberlesen */
       if (ch=='"') goto leerstring;
       *(zeile++)=upcase(ch);
       while( ch=getc(in_file),  (ch!=13) && (ch!=abbr) && (!in_file.eof) )
         {
           *(zeile++)=upcase(ch);
         }
       if (ch==13) getc(in_file);
     }
leerstring:;
   *zeile=0;
   return string;           /* 0:normales wort, 1:String ohne "string text" */
                                               /*       Anfuehrungsstriche */
}

upcase(ch)
char ch;
{
  if ( (ch>='a') && (ch<='z') )  return ch-('a'-'A');
  else return ch;
}

fgets(zeile)
char *zeile;
{
   while(!(in_file.eol))  *(zeile++)=upcase(getc(in_file));
   *zeile=0;                                              /* ende setzen   */
   if (in_file.eof) return;

   getc(in_file);      /* CR ueberlesen */
   getc(in_file);      /* LF ueberlesen */
}

char *get_projektname()
{
      do fgets(zeile);
      while ( strncmp(zeile,"PROJEKT",7) && !in_file.eof );
      if (in_file.eof) return 0;
        else return zeile+8;
}

execute(projekt)
char *projekt;
{
   while (!in_file.eof)
    {
       static char *ptr;
       ptr=get_projektname();
       if ( ptr && (!strcmp(projekt,ptr) ) ) break;
    }
   if (in_file.eof) error ("Projektname in Makefile nicht gefunden !",0);
   get_wort(zeile);
   if (strcmp(zeile,"BEGIN")) error("BEGIN erwartet !",zeile);
   block(1);
   printf("era a:make.sub");

}

error(text,gefunden)
char *text,*gefunden;
{
   red_out(0);
   printf("Make Error:%s\n",text);
   if (gefunden) printf("gefunden:%s\n",gefunden);
   exit();
}

usage()
{
   printf("Usage: MAKE PROJEKTNAME [TEST]\n");
   printf("In MAKEFILE verfuegbare Projektnamen:\n\n");
   while (!in_file.eof)
    {
       char *ptr=get_projektname();
       if (ptr) printf("%s\n",ptr);
    }
   exit();
}

get_time(fcb,name)
FILE *fcb;
char *name;
{
   if (bdos_a(102,fcb))                        /* read file date stamp */
    {
                   /* wenn datei nicht gefunden,datum=1.1.1978 */
     *( ( (int *) fcb ) +14 ) = *( ( (int *) fcb ) +15 ) =0;
    }
}

tcomp(time1,time2)
char *time1, *time2;
{
   int datecomp=( *( (int*) time1) - *( (int*) time2) );
   if (datecomp) return(datecomp);
   return strncmp(time1+2,time2+2,2);
}

