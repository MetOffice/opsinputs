/*******************************************************************************
 * (C) Crown copyright Met Office. All rights reserved.
 *     Refer to COPYRIGHT.txt of this distribution for details.
 *******************************************************************************
 * NAME
 *   gen_clockticks
 *
 * SYNOPSIS
 *   Find the number of clock ticks per second using a POSIX function sysconf.
 ******************************************************************************/

#include <unistd.h>
#include "ops_c_fort2c_2a.h"

void
#if defined(UPPERCASE) && defined(UNDERSCORE)
GEN_CLOCKTICKS_
#elif defined(UPPERCASE)
GEN_CLOCKTICKS
#elif defined(UNDERSCORE)
gen_clockticks_
#else
gen_clockticks
#endif
(integer *pi_clockticks) {
  *pi_clockticks = sysconf(_SC_CLK_TCK);
}
