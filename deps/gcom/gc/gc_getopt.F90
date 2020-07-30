! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_getopt(var, val, istat)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Get a runtime GC configuration option
!     *
!     * Input:
!     *  VAR     - configuration variable to read
!     *
!     * Output:
!     *  VAL     - present value of VAR
!     *  ISTAT   - status of option read, 0 is OK.
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE gc_globals_mod, ONLY:                                                      &
    gc__max_opts,                                                              &
    gc__options

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

#include "gc_constants.h"
#include "gc_functions.h"

INTEGER (KIND=gc_int_kind) :: var, val, istat

IF (var > gc__max_opts .OR. var < 1) THEN
  ! VAR is out of range
  CALL gc_abort(gc_me(), gc_nproc(),                                           &
                'Cannot get option - out of range')
END IF

val   = gc__options(var)
istat = gc__ok

RETURN
END SUBROUTINE gc_getopt
