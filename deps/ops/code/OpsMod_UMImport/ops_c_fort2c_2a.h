#ifndef CFORT2C_2A_H
#define CFORT2C_2A_H

#include <string.h>
#include <stdint.h>

#include "ops_c_fort2c_types.h"

/* Description:                                                     */
/*                                                                  */
/* Definitions needed for Fortran to C Interface                    */
/*                                                                  */
/* Information:                                                     */
/*                                                                  */
/* Provides definitions needed for Fortran to C interface.          */


/* Define the function that outputs the text string for          */
/* messages.  Note that the trailing newline character is        */
/* to be supplied by the print routine, and is no longer         */
/* in the string.  Similarly, leading new lines are now handled  */
/* by the print routine - typically a newline is inserted for    */
/* each change of unit.                                          */

#define MAX_OUTSTR 384

void ops_f_ereport(void *, void *);
#define ereport(a,c) ops_f_ereport((void *)a,(void *)c)

/* End C_FORT2C                                                    */
#endif
