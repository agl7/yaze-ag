#define DIM(var) (sizeof(var)/sizeof(var[0]))
#define WORT unsigned int
#define BYTE char
#define DIV /
#define MOD %
              /* mit MI-C Option /s compilieren !!!!!!!!!!!!!!!!! */
struct dpb {
   WORT    spt;
   BYTE    bsh;
   BYTE    blm;
   BYTE    exm;
   WORT    dsm;
   WORT    drm;
   WORT    al;
   WORT    cks;
   WORT    off;
   BYTE    psh;
   BYTE    phm;
  } ;

struct entry {
  BYTE   user;
  char   name[8];
  char   typ[3];
  BYTE   extent;
  BYTE   s1;
  BYTE   s2;
  BYTE   rc;
  union  {
           BYTE byte[16];
           WORT wort[8];
         } block;
  };

struct dir_eintrag
 {  struct entry eintrag;
    BYTE time[10];
    struct dir_eintrag  *next;
 };
struct knoten
 {  struct dir_eintrag *dire_ptr;
    char   erledigt;
    struct knoten *links,*rechts;
 } ;
