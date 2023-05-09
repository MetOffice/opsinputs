! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_get_communicator(val, istat)
!     ******************************************************************
!     * Purpose:
!     *
!     *  For MPI_SRC, this routine returns the
!     *  the active communicator used by GCOM. For other
!     *  versions, returns GC_OK.
!     *
!     *
!     * Input:
!     *  none.
!     *
!     * Output:
!     *  VAL     - present value of communicator (or GC_OK)
!     *  ISTAT   - status of option read, 0 is OK.
!     *
!     * NOTES:
!     *
!     ******************************************************************

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    gc__my_mpi_comm_world
#endif

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

#include "gc_constants.h"

INTEGER (KIND=gc_int_kind), INTENT(OUT) :: val
INTEGER (KIND=gc_int_kind), INTENT(OUT) :: istat

#if defined(MPI_SRC)
val = gc__my_mpi_comm_world
#else
val=gc__ok
#endif

istat=gc__ok

RETURN
END SUBROUTINE gc_get_communicator
