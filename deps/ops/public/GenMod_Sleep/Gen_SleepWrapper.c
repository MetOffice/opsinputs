/*******************************************************************************
 * (C) Crown copyright Met Office. All rights reserved.
 *     Refer to COPYRIGHT.txt of this distribution for details.
 *******************************************************************************
 * NAME
 *   gen_sleepwrapper
 *
 * SYNOPSIS
 *   Wrapper for POSIX sleep function.  Needed because Fortran interap cannot
*    interface with unsigned C entities.
 ******************************************************************************/

#include <unistd.h>

void Gen_SleepWrapper (int seconds, long *ret) {
  *ret = 0;
  if (seconds > 0) {
    *ret = sleep (seconds);
  }
}
