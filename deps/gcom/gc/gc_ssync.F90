! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_ssync (nproc, istat)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Obsolete - Shared memory synchronization of processors.
!     *
!     * Input:
!     *  NPROC   - number of nodes
!     *
!     * Output:
!     *  ISTAT   - Return status, GC_OK if success.
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

#include "gc_constants.h"

INTEGER (KIND=gc_int_kind) :: nproc, istat

istat = gc__ok

RETURN
END SUBROUTINE gc_ssync
