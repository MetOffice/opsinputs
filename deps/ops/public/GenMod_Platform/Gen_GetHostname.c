/*******************************************************************************
 * (C) Crown copyright Met Office. All rights reserved.
 *     Refer to COPYRIGHT.txt of this distribution for details.
 *******************************************************************************
 * NAME
 *   gen_gethostname
 *
 * SYNOPSIS
 *   Find out the current hostname.
 *******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "ops_c_fort2c_2a.h"

void
#if defined(UPPERCASE) && defined(UNDERSCORE)
gen_gethostname_
#elif defined(UPPERCASE)
GEN_GETHOSTNAME
#elif defined(UNDERSCORE)
gen_gethostname_
#else
gen_gethostname
#endif
(char *hostname, integer *hostname_len, integer *iret) {
  *iret = (integer) gethostname(hostname, (int) *hostname_len);
  int len = strlen(hostname);
  if (len > *hostname_len || *iret != 0) {
    len = 0;
  }
  int i;
  for (i = len; i < *hostname_len; i++){
    hostname[i] = ' ';
  }
}
