! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_setopt(var, val, istat)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Set a runtime GC configuration option
!     *
!     * Input:
!     *  VAR     - configuration variable to change
!     *  VAL   - new value of VAR
!     *
!     * Output:
!     *  ISTAT   - status of option setting, 0 is OK.
!     *
!     * NOTES:
!     *  This is a synchronizing call for valid VAR/VAL pairs, or a
!     *  NO-OP otherwise.
!     *
!     ******************************************************************

USE gc_globals_mod, ONLY:                                                      &
    gc__max_opts,                                                              &
    gc__options

USE gcom_mod, ONLY:                                                            &
    gc_force_bitrep,                                                           &
    gc_is_parallel,                                                            &
    gc_on,                                                                     &
    gc_off

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

#include "gc_constants.h"
#include "gc_functions.h"

INTEGER (KIND=gc_int_kind) :: var, val, istat

! Ensure Option is one we recognise
IF (var > gc__max_opts .OR. var < 1) THEN
  CALL gc_abort(gc_me(), gc_nproc(),                                           &
                'Cannot set option - unrecognised')
END IF

! Ensure Option values are recognised
IF (var == gc_force_bitrep .AND.                                               &
    (val /= gc_on .AND. val /= gc_off)) THEN
  CALL gc_abort(gc_me(), gc_nproc(),                                           &
      'Cannot set GC_FORCE_BITREP - value unrecognised')
END IF

gc__options(var) = val

istat = gc__ok

RETURN
END SUBROUTINE gc_setopt
