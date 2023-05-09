/*
 *****************************COPYRIGHT*******************************
 (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
 *****************************COPYRIGHT*******************************

      ******************************************************************
      * Purpose:
      *
      *  Aborts program execution on current CPU. Intended as a portable
      *  abort for the serial version of GCOM.
      *
      * Input:
      *
      * Output:
      *
      * NOTES:
      *
      * GCOM internal routine only! Do not call from user program.
      * 4 different versions provided. One should match...
      *
      ******************************************************************
*/

#include <stdlib.h>

/* Prototypes */

void gc__abort (void);
void GC__ABORT (void);
void gc__abort_(void);
void GC__ABORT_(void);

/* Procedures */

void gc__abort(void)
{
 abort();
}

void GC__ABORT(void)
{
 abort();
}

void gc__abort_(void)
{
 abort();
}

void GC__ABORT_(void)
{
 abort();
}
