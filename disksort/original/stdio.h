/* geaendert fuer mylib.rel m.schewe */
#DEFINE NULL    0
struct FCB
 {
   CHAR DRIVE,
        NAME[8],
        TYPE[3],
        EXT,
        RESV[2],
        RC,
        SYDX[16],
        CR;
   UNSIGNED RECORD;
   INT OVERFL;
   char  eof;               /* fcb+37   */
   char  eol;               /* fcb+38   */
   char  pointer;           /* fcb+39   */
   char  report;            /* fcb+40   */
   char  direct_flag;       /* fcb+41   */
   char  buffer[128];       /* fcb+42   */
  /* darf nicht geaendert werden (wegen diskf.z80,hilfslib.z80 ! ) */
 } ;
typedef struct FCB FILE;
