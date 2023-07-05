/* UNIX routines */

#include <time.h>
#include <sys/time.h>
#include <unistd.h>
/* #include <stdbool.h> */
#include "ytypes.h"
#define NANOS_PER_MILLI     1000000
#define MILLIS_PER_SEC      1000

extern int nanosleep(const struct timespec *req, struct timespec *rem);

const bool rtc_avail = true;

ui32 sim_os_msec (void)
{
struct timeval cur;
struct timezone foo;
ui32 msec;

gettimeofday (&cur, &foo);
msec = (((ui32) cur.tv_sec) * 1000) + (((ui32) cur.tv_usec) / 1000);
return msec;
}

ui32 sim_os_ms_sleep (unsigned int milliseconds)
{
ui32 stime = sim_os_msec ();
struct timespec treq;

treq.tv_sec = milliseconds / MILLIS_PER_SEC;
treq.tv_nsec = (milliseconds % MILLIS_PER_SEC) * NANOS_PER_MILLI;
nanosleep (&treq, NULL);
return sim_os_msec () - stime;
}
