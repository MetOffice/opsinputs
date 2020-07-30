! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_gsync (nproc, istat)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Synchronize the processors. Mainly used in front of
!     *  (asynchronous) communication and in connection with timing.
!     *
!     * Input:
!     *  NPROC   - number of nodes
!     *
!     * Output:
!     *  ISTAT   - status of send 0 is OK (MPI_SRC only),
!     *            refer to the header files for nonzero status codes
!     *
!     * NOTES:
!     *
!     *  No node can continue execution before everybody have reached
!     *  this point.
!     ******************************************************************

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    gc__my_mpi_comm_world
#endif

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

#include "gc_constants.h"

INTEGER (KIND=gc_int_kind) :: nproc, istat

#if defined(MPI_SRC)
CALL mpl_barrier(gc__my_mpi_comm_world, istat)
#endif

#if defined(SERIAL_SRC)
istat = gc__ok
#endif

RETURN
END SUBROUTINE gc_gsync
