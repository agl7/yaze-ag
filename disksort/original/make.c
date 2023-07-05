#include <stdio.h>

FILE out_file;
FILE in_file;
char zeile [128];
char langzeile[255];
int test=0;

struct liste {
                 char *name;
                 char *wert;
                 struct liste *next;
             } *variablen=NULL;
                                   /* #varname (stringvariable) */
block(wahr)   /* der Parameter wahr dient fuer die bedingte ausfuehrung */
int wahr;     /* wahr=1 -> Anweisung ausfuehren, wahr=0 -> nur Makefile */
{             /* wortweise lesen und syntax testen,Anw.nicht ausfuehren */
  while ( get_wort(zeile), strcmp(zeile,"END"))  anweisung(wahr);
}

anweisung(wahr)
int wahr;
{
   if (!strcmp(zeile,"IF"))
     {
        if_anweisung(wahr);
        return;
     }
   if (!strcmp(zeile,"IF_BEDINGUNG"))
     {
        if_bedingung(wahr);
        return;
     }
   if (!strcmp(zeile,"LET"))
     {
        let_anweisung(wahr);
        return;
     }
   if (!strcmp(zeile,"DO"))
     {
        do_anweisung(wahr);
        return;
     }
   if (!strcmp(zeile,"BEGIN"))
     {
        block(wahr);
        return;
     }
   if (strcmp(zeile,"END"))  error("Unbekannte Anweisung !",zeile);
}

char *get_variable(v_name)
char *v_name;
{
  struct liste *ptr=variablen;
  while (ptr && strcmp(v_name,ptr->name) ) ptr=ptr->next;
  if (!ptr) error("Variable nicht gefunden !",v_name);
  return ptr->wert;
}

teilausdruck_anfuegen()
{
  if (get_wort(zeile)) strcat(langzeile,zeile);
    else
      {
        if (*zeile=='#') strcat(langzeile,get_variable(zeile));
         else error("Variable oder String erwartet",zeile);
      }
}

char *operation ()
{
    *langzeile=0;
    if ( *zeile!='(' ) error(" ( erwartet",zeile);
    teilausdruck_anfuegen();
    while(get_wort(zeile),*zeile!=')')
      {
        if (*zeile!='+') error("Unzulaessiger Stringoperator",zeile);
        teilausdruck_anfuegen();
      }
  return langzeile;           /* enthaelt zusammengefuegten String */
}


char *ausdruck()
{
    if (get_wort(zeile)) return zeile;                 /* ist string */
    if (*zeile=='#') return get_variable(zeile);
    return operation ();
}

let_anweisung(wahr)
int wahr;
{
    char *name,*wert;
    get_wort(zeile);
    if (*zeile!='#') error ("#VarName erwartet:",zeile);
    if (wahr) name=strsave(zeile);
    get_wort(zeile);
    if (*zeile!='=') error ("= erwartet:",zeile);
    wert=ausdruck();
    if (wahr) insert_var(name,strsave(wert));
}

insert_var(v_name,v_wert)
char *v_name,*v_wert;
{
  struct liste **ptr=&variablen;
  while( *ptr && strcmp(v_name,(**ptr).name) ) ptr= &(**ptr).next;
  if (*ptr) (**ptr).wert=v_wert;
  else
    {
       *ptr=malloc(sizeof(struct liste));
       (**ptr).name=v_name;
       (**ptr).wert=v_wert;
       (**ptr).next=NULL;
   }
}

if_anweisung(wahr)
int wahr;
{
    get_wort(langzeile);
    if (*langzeile=='#')
      {                        /* typ: if #varname <anweisung> */
        get_wort(zeile);
        if (*get_variable(langzeile)) anweisung(wahr);
         else anweisung(0);
        return;
      }
    error("#varname erwartet",langzeile);
}

if_bedingung(wahr)
int wahr;
{     /* if_bedingung ( file1 file2 file3 .. filen )     */
      /* file1 ist abhaengig von file2 .. filen          */
    char name1 [80] ,name2 [80];
    static int ergebnis;
    FILE fcb1,fcb2;
    get_wort(zeile);
    if (*zeile!='(') error("( erwartet",zeile);
    get_wort(name1);
    assign(fcb1,name1);
    get_time(fcb1,name1);
    ergebnis=0;
    while( get_wort(name2), *name2!=')' )
     {
       assign(fcb2,name2);
       get_time(fcb2,name2);
       ergebnis |= ( tcomp( ((char *) fcb1)+28 ,((char *) fcb2)+28 )  <= 0 );
     }
    get_wort(zeile);
    anweisung(wahr && ergebnis);
}

do_anweisung(wahr)
int wahr;
{
    char *ptr=ausdruck();
    while (*ptr)
     {
       if (*ptr=='\\')
         {
            if (*(++ptr)!='N') error ("Unzulaessiges Zeichen nach Backsl.",0);
            if (wahr) printf("\n");
         }
       else if (wahr) printf("%c",*ptr);
       ++ptr;
     }
}

main(argc,argv)
int argc;
char *argv[];
{
  printf("\nMAKE.C (c) Copyright 11/1988 by Michael Schewe  ");
  printf(" 3119 Gollern Nr.17\n\n");
  if (argc>2)
    {
      if( strcmp(argv[2],"TEST") ) error ("Falsche Option",argv[2]);
      test=1;
    }
  assign(out_file,"a:make.sub");
  out_file.report=1;
  rewrite(out_file);
  assign(in_file,"makefile");
  in_file.report=1;
  fopen(in_file);
  if (argc<2) usage();
  if (!test)  red_out(out_file);
  execute(argv[1]);
  red_out(0);
  weof(out_file);
  fclose(out_file);
  if (test) exit();
  bdos_a(26,0x80);                      /* set dma */
  strcpy(0x80,"SUBMIT A:MAKE.SUB");
  bdos_a(47,0);        /* chain to program-aufruf  ( command-line ab 80h ) */
  printf("Make-Error:Command line not executed. No CP/M Version 3 ?\n");
}
