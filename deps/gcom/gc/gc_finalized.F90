! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_finalized (FINAL)
!     ******************************************************************
!     * Purpose:
!     *
!     * Returns a LOGICAL indicating if GCOM has been finalized
!     *
!     * Output:
!     *  final   - .TRUE. if GCOM has been finalized
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE gc_globals_mod, ONLY:                                                      &
    gc__finaled

USE gc_kinds_mod, ONLY: gc_log_kind

IMPLICIT NONE

LOGICAL (KIND=gc_log_kind), INTENT(OUT) :: FINAL

FINAL = gc__finaled

RETURN
END SUBROUTINE gc_finalized
