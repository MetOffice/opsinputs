!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Flush an I/O unit.
!-------------------------------------------------------------------------------

#include "gen_fortran_version.h"

SUBROUTINE Gen_FlushUnit (unit)

#if FORTRAN_VERSION < 2003
#if defined(USE_FLUSH)
#if defined(NAG) || defined(NAG4)

USE F90_UNIX_IO, ONLY: &
  FLUSH

#endif

#if defined(NAG)

USE OpsMod_Kinds, ONLY: &
  Integer32

#endif
#endif
#endif

IMPLICIT NONE

INTEGER, INTENT(IN)     :: unit

#if FORTRAN_VERSION < 2003
#if defined(USE_FLUSH)
#if defined(NAG)

INTEGER(kind=integer32) :: local_unit

#else

INTEGER                 :: local_unit

#endif

local_unit = unit
CALL FLUSH (local_unit)

#endif

#else

FLUSH (unit)

#endif

END SUBROUTINE Gen_FlushUnit
