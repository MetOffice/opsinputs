! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************
#include "gcg_prolog.h"

SUBROUTINE gcg_config (mxgrp, mxrot)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Return information about the GCG configuration.
!     *
!     * Output:
!     *  MXPROC    - maximum number of groups per processor (deprecated)
!     *  MXROT     - maximum number of elements in a rotate (shift)
!     *              operation
!     *
!     * NOTES:
!     *  Refer to GC_CONFIG() for core GC configuration settings.
!     *
!     ******************************************************************

USE gc_globals_mod, ONLY:                                                      &
    max_rotate

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind) :: mxgrp, mxrot


mxgrp = 0                ! Deprecated
mxrot = max_rotate
END SUBROUTINE gcg_config
