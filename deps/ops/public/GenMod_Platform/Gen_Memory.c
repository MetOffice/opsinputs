/*******************************************************************************
 * (C) Crown copyright Met Office. All rights reserved.
 *******************************************************************************
 * NAME
 *   gen_memory
 *
 * SYNOPSIS
 *   Find out the current memory heap using mallinfo().uordblks.
 ******************************************************************************/

#include <malloc.h>
#include "ops_c_fort2c_2a.h"


void
#if defined(UPPERCASE) && defined(UNDERSCORE)
GEN_MEMORY_
#elif defined(UPPERCASE)
GEN_MEMORY
#elif defined(UNDERSCORE)
gen_memory_
#else
gen_memory
#endif
(integer *memory_used) {
  *memory_used = mallinfo().uordblks;
}
