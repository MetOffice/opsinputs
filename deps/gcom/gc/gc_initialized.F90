! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_initialized (init)
!     ******************************************************************
!     * Purpose:
!     *
!     * Returns a LOGICAL indicating if GCOM has been initialized
!     *
!     * Output:
!     *  init    - .TRUE. if GCOM has been initialized
!     *
!     * NOTES:
!     *   gc_exit() does not effect the value of init returned
!     *
!     ******************************************************************

USE gc_globals_mod, ONLY:                                                      &
    gc__inited

USE gc_kinds_mod, ONLY: gc_log_kind

IMPLICIT NONE

LOGICAL (KIND=gc_log_kind), INTENT(OUT) :: init

init = gc__inited

RETURN
END SUBROUTINE gc_initialized
