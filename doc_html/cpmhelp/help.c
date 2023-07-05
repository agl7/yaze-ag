/*
 *   Program for displaying CP/M help files by Per Ola Ingvarsson
 *   Yes, it is messy.
 */
typedef unsigned char uint8;
typedef char int8;

#ifdef HI_TECH_C
#include<hitech.h>
#include<unixio.h>
#define O_RDONLY  0
#define SHORT_FILE_NAMES 1
/* Const does not seem to exist */
#define const
typedef UINT_16 uint16;
typedef UINT_32 uint32;
#define SEEK_SET 0
#define INLINE
#else
typedef uint8 BOOL;
typedef unsigned short uint16;
typedef int FAST;
typedef unsigned int uint32;
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#undef SHORT_FILE_NAMES
#define INLINE inline
#endif

#include <ctype.h>
#include <stdio.h>
#include <string.h>


/* How many entries can we use in CP/M? */
#define MAX_NBR_IDX     100
/* Defines the size of the string that the user can enter */
#define MAX_USER_STRING 100

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

#define USE_UNIX_FILES
#ifdef USE_UNIX_FILES
#define INVALID_FILE -1
typedef int file_t;
#endif

/**
 *   Struct representing one topic in the index part of the file.
 */
struct idx_entry_t {
   /** Topic, really */
   char   m_name[13];
   /** Offset in file * 0x80 ? */
   uint16 m_record;
   /** Offset from m_record*0x80 */
   uint8  m_rec_offset;
   /** Level where to show topic */
   uint8  m_level;
   /** Index of parent */
   FAST m_parent;
};


/**
 *  Type of function for print_entry. void is really pointer to
 *  help_data_t, but I had problems with Hitech C.
 */
typedef void(* print_entry_fp)(void* self_in, FAST entry_num);

/**
 *  Function to call before printing.
 *  @param  self_in   Pointer to the help_data_t
 *  @param  entry_num Entry to print, -1 for index.
 *  @return TRUE if everything is OK.
 */
typedef BOOL(* start_printing_fp)(void* self_in, FAST enntry_num);
                                  
typedef BOOL(* stop_printing_fp)(void* self_in);

/**
 *   Collection of data used by the help functions.
 */
struct help_data_t {
   /** The index entries */
   struct idx_entry_t m_subjects[MAX_NBR_IDX];
   /** Index of next free index entry */
   uint8 m_nextFreeSubject;
   /** The current file */
   file_t m_inFile;
   /** Last displayed topic */
   uint8 m_lastDispTopic;
   /** Level of last displayed topic */
   uint8 m_lastDispLevel;
   /** Function to use when printing an entry name */
   print_entry_fp print_entry_name;
   /** Function to call before printing */
   start_printing_fp start_printing;
   /** Function to call after printing is done */
   stop_printing_fp stop_printing;
   /** File to print to (stdio) */
   FILE* m_outFile;
};



void
help_print_str(void* self_in,
               const char* str)
{
   struct help_data_t* self = (struct help_data_t*)self_in;
#ifdef HI_TECH_C
   if ( self->m_outFile == stdout ) {
      FAST len = strlen(str);
      str[len] = '$';
      /* Just guessing here, but it seems to work */
      bdos(9, str);
      str[len] = '\0';
   } else {
      fputs(str, self->m_outFile);
   }
#else
   printf(str);
   fputs(str, self->m_outFile);
#endif
}

void
help_print_strn(void* self_in, char* str, FAST len)
{
   struct help_data_t* self = (struct help_data_t*)self_in;
#ifdef HI_TECH_C
   if ( self->m_outFile == stdout ) {
      char tmpCh = str[len];
      str[len] = '$';
      /* Just guessing here, but it seems to work */
      bdos(9, str);
      str[len] = tmpCh;
   } else {
      fwrite(str, len, 1, self->m_outFile);
   }
#else
   fwrite(str, 1, len, stdout);
   fwrite(str, 1, len, self->m_outFile);    
#endif
}


char tmpLinkName[80];

const char*
html_get_file_name( struct help_data_t* self,
                    FAST entry_num,
                    BOOL isLink )
{
   if ( entry_num < 0 ) {
      if ( isLink ) {
         return "./";
      } else {
         return "index.html";
      }
   } else {
      sprintf(tmpLinkName, "%03d.html", entry_num);
      return tmpLinkName;
   }
}


void
help_print_prompt( struct help_data_t* self )
{
   help_print_strn(self, "HELP> ", 6);
}

void
help_html_print_prompt( struct help_data_t* self )
{
   const char* tmpNam = html_get_file_name(self, -1, TRUE);
   help_print_str(self, "<a href=\"");
   help_print_str(self, tmpNam);
   help_print_str(self, "\">HELP&gt;</a>  ");
}


BOOL
help_start_printing( void* self_in,
                     FAST entry_num)
{
   return TRUE;
}

BOOL
help_stop_printing( void* self_in )
{
   return TRUE;
}

BOOL
html_help_start_printing( void* self_in,
                         FAST entry_num)
{
   struct help_data_t* self = (struct help_data_t*)self_in;
   const char* tmpNam = html_get_file_name( self,
                                            entry_num,
                                            FALSE);
   self->m_outFile = fopen(tmpNam, "w");
   if ( self->m_outFile == NULL ) {
#ifndef HI_TECH_C
      printf(strerror(errno));
#endif
   }
   if ( self->m_outFile ) {
      help_print_str(self,
                     "<html><head><title>");
      if ( entry_num < 0 ) {
         help_print_str(self, "Help Index");
      } else {
         help_print_str(self, self->m_subjects[entry_num].m_name);
      }
      help_print_str(self, "</title></head><body><pre>\n");

   }
   return self->m_outFile != NULL;
}

BOOL
html_help_stop_printing( void* self_in )
{
   struct help_data_t* self = (struct help_data_t*)self_in;
   help_print_str(self, "</pre></body></html>\n");
   fclose(self->m_outFile);
   return TRUE;
}

void
help_print_entry_name( void* self_in,
                       FAST entry_num )
{
   struct help_data_t* self = (struct help_data_t*)self_in;
   help_print_strn(self_in, self->m_subjects[entry_num].m_name, 12);
}

void
html_help_print_entry_name( void* self_in,
                            FAST entry_num )
{
   struct help_data_t* self = (struct help_data_t*)self_in;
   struct idx_entry_t* entry = &(self->m_subjects[entry_num]);
   char* space_ptr = strchr(entry->m_name, ' ');
   const char* tmpNam = html_get_file_name(self, entry_num, TRUE);
   
   help_print_str(self_in, "<a href=\"");
   help_print_str(self_in, tmpNam);
   help_print_str(self_in, "\">");
   if ( space_ptr == NULL ) {
      help_print_strn(self_in, entry->m_name, 12);
   } else {
      help_print_strn(self_in, entry->m_name, space_ptr - entry->m_name);
   }
   help_print_str(self_in, "</a>");
   if ( space_ptr ) {
      help_print_str(self_in, space_ptr);
   }

}

void
debug_print_entry( struct idx_entry_t* entry )
{
   fprintf(stderr, "Entry : name = \"%s\"\n"
           "        level = %d\n"
           "        record = 0x%x\n"
           "        rec_offset = 0x%x\n",
          entry->m_name,
          entry->m_level,
          entry->m_record,
          entry->m_rec_offset);
}

int8
help_read_idx_entry(struct idx_entry_t* entry,
                    int file)
{
   uint8 buffer[16];
   uint16 recordLo;
   uint16 recordHi;
   if ( read( file, buffer, 16 ) != 16 ) {
      return -1;
   }
   if ( (char)(buffer[0]) == '$' ) {
      /* Seems like $ means end of index */
      return 1; /* OK */
   }
   /* Copy the name to the entry and zero-terminate */
   memcpy(entry->m_name, buffer, 12);   
   entry->m_name[12] = 0;

   /* Should not depend on processor type */
   recordLo = buffer[12];
   recordHi = buffer[13];
   entry->m_record = recordHi << 8 | recordLo;
   entry->m_rec_offset = buffer[14];
   entry->m_level = buffer[15];
   entry->m_parent = -1;
   /* debug_print_entry(entry); */
   return 0;
}

void
help_print_idx(struct help_data_t* self)
{
   uint8 counter = 0;
   uint8 currLevel = 1;
   struct idx_entry_t* entry;
   int i;
   help_print_str(self, "Topics available:\n\r\n\r");
   for( i = 0; i < self->m_nextFreeSubject; ++i) {
      entry = &self->m_subjects[i];
      if ( entry->m_level == currLevel ) {
         (self->print_entry_name)(self, i);
         if ( ++counter == 5 ) {
            counter = 0;
            help_print_strn(self, "\n\r", 2);
         }
      }
   }
   help_print_str(self, "\r\n\r\n");
}

BOOL
help_init(struct help_data_t* self, BOOL use_html)
{
   /* Reset some variables */
   self->m_nextFreeSubject = 0;
   self->m_inFile = INVALID_FILE;
   self->m_lastDispTopic = -1;
   self->m_lastDispLevel = 0; /* Starts at 1 */
   
   if ( use_html ) {
      self->print_entry_name = html_help_print_entry_name;
      self->start_printing   = html_help_start_printing;
      self->stop_printing    = html_help_stop_printing;
      self->m_outFile        = NULL;
   } else {
      self->print_entry_name = help_print_entry_name;
      self->start_printing   = help_start_printing;
      self->stop_printing    = help_stop_printing;
      self->m_outFile        = stdout;
   }
   return TRUE;
}

BOOL
help_open_file(struct help_data_t* self,
               const char* filename)
{
   file_t file = INVALID_FILE;
   file = open(filename, 0);
   if ( file == INVALID_FILE ) {
      return FALSE;
   }
   
   self->m_inFile = file;
   return TRUE;
}

BOOL help_read_idx(struct help_data_t* self)
{
   FAST i;
   int res = 0;
   struct idx_entry_t* entry = NULL;

   while ( self->m_nextFreeSubject < MAX_NBR_IDX ) {
      entry = &(self->m_subjects[self->m_nextFreeSubject]);
      if ( (res = help_read_idx_entry( entry, self->m_inFile)) < 0 ) {
         return FALSE;
      } else if ( res == 1 ) {
         /* Stop */
         return TRUE;
      } else {
         /* Set parent of entry if were not at top level */
         if ( entry->m_level != 1 ) {
            i = self->m_nextFreeSubject;
            /* Search backwards until level changes to less */
            while ( (i > 0) &&
                    (self->m_subjects[i].m_level
                     >= entry->m_level )) {
               --i;
            }
            if ( i >= 0 ) {
               entry->m_parent = i;
            }
         }
         self->m_nextFreeSubject++; 
      }
   }
   /* If we got here without seeing the $ - error */
   return FALSE;
}

FAST
help_find_parent(struct help_data_t* self,
                 FAST topic_nbr,
                 FAST parent_level)
{
   do {
      struct idx_entry_t* entry = &self->m_subjects[topic_nbr];
      if ( entry->m_level == parent_level ) {
         return topic_nbr;
      } else {
         topic_nbr = entry->m_parent;
      }
   } while ( topic_nbr >= 0);
   return -1;
}

BOOL
help_print_text(struct help_data_t* self,
                FAST topic_nbr)
{
   if ( topic_nbr >= 0 && topic_nbr < self->m_nextFreeSubject ) {
      uint8 nextLevel;
      FAST i, j;
      char ch;
      /* Create buffer and room for slashes. */
      char printBuffer[MAX_USER_STRING+3];
      /* Position in printBuffer */
      FAST printPos = 0;
      struct idx_entry_t* entry = &self->m_subjects[topic_nbr];
      uint32 rec = entry->m_record;
      uint32 off = entry->m_rec_offset + 0; /* Hitech needs this + 0*/
      /* Offet is record * 128 + rec_offset */
      uint32 offset = (rec << 7) + off;
      long int res;
      FAST slash_cnt = 0; /* Counter for slashes in a row */
      /* Print parents and name of command */
      for( i=1; i <= entry->m_level; ++i ) {
         for( j = 0; j < i; ++j ) {
            help_print_str(self, "  ");
         }
         (self->print_entry_name)(self, help_find_parent(self,
                                                         topic_nbr,
                                                         i));
         help_print_str(self, "\n\r");
      }

      /* Seek to the record offset */
      res = lseek( self->m_inFile,
                   offset,
                   SEEK_SET);
/*        printf("rec = %ld, rec_off = %ld\n", rec, off); */
/*        printf("res = %lu, off = %lu\n", res, offset); */
      
      /* Seems like a help text ends with three slashes or end of file */
      while ( slash_cnt < 3 ) {
         int res = read( self->m_inFile, &ch, 1);
         if ( res <= 0 ) {
            break;
         }
         if ( ch == '/' ) {        
            ++slash_cnt;
         } else {
            /* Print remaining slashes */
            while( slash_cnt ) {
               printBuffer[printPos++] = '/';
               /* putchar('/'); */
               --slash_cnt;
            }
            /* putchar(ch); */
            printBuffer[printPos++] = ch;            
         }
         if ( printPos >= MAX_USER_STRING ||
              slash_cnt >= 3 ||
              ch == 0x1a ) { /* end of file */
            if ( ch == 0x1a ) {
               --printPos;
               slash_cnt = 3; /* Means that we should exit */
            }
            printBuffer[printPos] = '\0';
            help_print_str(self, printBuffer);
            printPos = 0;
         }
      }
      nextLevel = entry->m_level + 1;
      for ( i = topic_nbr + 1; i < self->m_nextFreeSubject; ++i ) {
         if ( self->m_subjects[i].m_level == nextLevel ) {
            if ( i == (topic_nbr + 1) ) {
               help_print_str(self,
                              "\r\nENTER .subtopic FOR INFORMATION ON THE"
                              " FOLLOWING SUBTOPICS:\r\n\n");
            }
            /* We know that there are 12 characters in an entry */
            (self->print_entry_name)(self, i);
         } else if ( self->m_subjects[i].m_level < nextLevel ) {
            /* We're back to an entry of the startlevel */
            break;
         }
      }
      help_print_str(self, "\r\n\n\n");
      self->m_lastDispLevel = nextLevel - 1;
      self->m_lastDispTopic = topic_nbr;
      return TRUE;
   } else {
      return FALSE;
   }
}

FAST
help_comp_str(const char* str1,
              const char* str2,
              FAST length)
{
#if 0
   return strncasecmp(str1, str2, length);
#else
   uint8 c1 = 0;
   uint8 c2 = 0;
   
   if (length) {
      do {
         c1 = *str1;
         c2 = *str2;
         ++str1; ++str2;
         if ( (!c1) || (!c2) ) {
            /* End of a string */
            break;
         }
         if ( ! isupper(c1) ) {
            c1 = toupper(c1);
         }
         if ( ! isupper(c2) ) {
            c2 = toupper(c2);
         }
         if (c1 == c2) {
            continue;
         }
         if (c1 != c2)
            break;
      } while (--length);
   }
   return (FAST)c1 - (FAST)c2;
#endif
}

FAST
help_find_topic_name(struct help_data_t* self,
                     const char* topic,
                     FAST length,
                     FAST start_pos,
                     FAST level)
{
   FAST i;
   for( i = start_pos; i < self->m_nextFreeSubject; ++i ) {
      /* Only check the supplied level */
      if ( self->m_subjects[i].m_level == level ) {
         if ( !help_comp_str( topic, self->m_subjects[i].m_name, length) ) {
               return i;
         }
      } else if ( self->m_subjects[i].m_level < level ) {
         /* Level must not decrease */
         return -1;
      }
   }
   /* Not found */
   return -1;
}

FAST
help_parse_and_print(struct help_data_t* self,
                     char* instring)
{
   FAST found_topic = 0;
   FAST currLevel = 1; /* Default - display level 0 */
   FAST start_pos = 0; /* Default - start at beginning */
   FAST length = strlen(instring);

   /* Remove spaces in the end */
   while( length > 0 && instring[length-1] == ' ' ) {
      instring[--length] = '\0';
   }
   
   /* ? means list */
   if ( instring[0] == '?' ) {     
      help_print_idx(self);
      return -1;
   }

   
   /* '.' - start with the last displayed topic and level */
   if ( instring[0] == '.' ) {
      /* Only look for subtopics to the last displayed one */
      currLevel = self->m_lastDispLevel + 1;
      start_pos = self->m_lastDispTopic + 1;
      /* Remove the dot */         
      ++instring;
      --length;
   }
   
   while ( instring[0] != '\0'  ) {
      /* Start looking for help-stuff */
      char* space_ptr = strchr(instring, ' ');
      if ( space_ptr != NULL ) {
         *space_ptr = '\0';
         /* FIXME: Can be optimized. (Maybe some other stuff too?) */
         length = strlen(instring);
      } else {
         space_ptr = &instring[length-1];
      }

      /* Now look up the topic */
      found_topic = help_find_topic_name(self, instring, length,
                                         start_pos, currLevel);
      if ( found_topic >= 0 ) {
         (self->print_entry_name)(self, found_topic);
         currLevel++;
         start_pos = found_topic + 1;
         instring = space_ptr + 1;
         length = strlen(instring);
      } else {
         help_print_str(self, instring);
         return -1;
      }
   }
   return found_topic;
}


void
help_run(struct help_data_t* self)
{
   char str[MAX_USER_STRING];      
   FAST pos = 0;
   help_print_str(self, "CHELP UTILITY v0.1\r\n\n");
   help_print_str(self, "At \"HELP>\" enter topic {,subtopic}...\r\n\n");
   help_print_str(self, "EXAMPLE:  HELP> DIR BUILT-IN\r\n\n");
   
   help_print_idx(self);

   while ( pos != 1 ) {
      int ch = -1;
      pos = 0;
      help_print_prompt(self);
      fflush(stdout);
      do {
         ch = getc(stdin);
         if ( ch == 10 ) {
            ch = 0;
         }
         if ( ch >= 0 && ch != 10 && ch != 13 ) {
            str[pos++] = ch;
         }
      } while ( ch > 0 );
      if ( ch == 0 && pos != 1 ) {
         help_print_text(self, help_parse_and_print(self, str));
      }
   }
}

BOOL
help_print_html(struct help_data_t* self)
{
   FAST i;
   if ( (self->start_printing)(self, -1) == FALSE ) {
      printf("Error when opening outfile\n");
      return FALSE;
   }
   help_print_str(self, "CHELP UTILITY v0.1\r\n\n");
   help_print_str(self, "At \"HELP>\" enter topic {,subtopic}...\r\n\n");
   help_print_str(self, "EXAMPLE:  HELP> DIR BUILT-IN\r\n\n");
   
   help_print_idx(self);
   help_html_print_prompt(self);
   (self->stop_printing)(self);
   for ( i = 0; i < self->m_nextFreeSubject; ++i ) {
      if ( (self->start_printing)(self, i) == FALSE ) {
         printf("Error when opening outfile\n");
         return FALSE;
      }
      help_print_text(self, i);
      help_html_print_prompt(self);
      (self->stop_printing)(self);
   }
   return TRUE;
}

int main(int argc, char** argv)
{
   BOOL use_html = TRUE;
   /* Default filename */
   const char* filename  = "HELP.HLP";
   /* The help data goes here */
   struct help_data_t help_data;
   if ( argc > 1 && (strcmp(argv[1], "-f" ) == 0) ) {
      filename = argv[2];
   }   
   if ( ! help_init( &help_data, use_html) ) {
      return 1;
   }
   if ( help_open_file( &help_data,
                        filename ) == FALSE ) {
      fprintf(stderr, "Cannot open file \"%s\"\n", filename);
      return 2;
   }
   if ( help_read_idx( &help_data ) == FALSE ) {
      fprintf( stderr, "Error reading index\n");
   }
   if ( use_html ) {
      help_print_html(&help_data);
   } else {
      help_run(&help_data);
   }
   return 0;
}
