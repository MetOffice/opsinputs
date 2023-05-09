/*******************************************************************************
 * (C) Crown copyright Met Office. All rights reserved.
 *******************************************************************************
 * NAME
 *   gen_cputime
 *
 * SYNOPSIS
 *   Get current CPU time using POSIX function "times".
 *******************************************************************************/

#include <sys/times.h>
#include "ops_c_fort2c_2a.h"

void
#if defined(UPPERCASE) && defined(UNDERSCORE)
GEN_CPUTIME_
#elif defined(UPPERCASE)
GEN_CPUTIME
#elif defined(UNDERSCORE)
gen_cputime_
#else
gen_cputime
#endif
(real *pr_CPUTime, integer *pi_rc) {
  struct tms time;
  clock_t rc = times(&time);
  *pi_rc = (rc == -1 ? 1 : 0);
  *pr_CPUTime = time.tms_utime;
}
