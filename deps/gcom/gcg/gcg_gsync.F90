! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gcg_prolog.h"

SUBROUTINE gcg_gsync (gid, istat)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Synchronize the processors in a group. Mainly used in front of
!     *  (asynchronous) communication and in connection with timing.
!     *
!     * Input:
!     *  GID     - processor group ID
!     *
!     * Output:
!     *  ISTAT   - status of send 0 is OK (MPI_SRC only),
!     *            refer to the header files for nonzero status codes
!     *
!     * NOTES:
!     *
!     *  No PE in a group can continue execution before everyone in the
!     * group has reached this point.
!     ******************************************************************

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

#include "gc_constants.h"

INTEGER (KIND=gc_int_kind) :: gid, istat

#if defined(MPI_SRC)
CALL mpl_barrier(gid, istat)
#endif

#if defined(SERIAL_SRC)
istat = gc__ok
#endif

RETURN
END SUBROUTINE gcg_gsync
