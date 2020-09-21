! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gcg_prolog.h"

SUBROUTINE gcg_ssync (gid, istat)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Group synchronisation.
!     *
!     * Input:
!     *  GID     - processor group ID
!     *
!     * Output:
!     *  ISTAT   - status of rsum. 0 is OK (MPI_SRC only),
!     *            refer to the header files for nonzero status codes
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind) :: gid, istat

INTEGER (KIND=gc_int_kind) :: l, me, nproc, igid, iloc, grank
#include "gc_functions.h"
INTEGER (KIND=gc_int_kind) :: i


istat = 0

IF (gid  ==  0) THEN
  nproc = gc_nproc()
  CALL gc_ssync(nproc, istat)
  RETURN
END IF

RETURN
END SUBROUTINE gcg_ssync
