! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_set_communicator(val, me, nproc, istat)
!     ******************************************************************
!     * Purpose:
!     *
!     *  For MPI_SRC, this routine sets the active communicator used
!     *  by GCOM and returns the number of processors and my process
!     *  id within this communicator.
!     *
!     *
!     *
!     * Input:
!     *  VAL   - value of communicator to use
!     *
!     * Output:
!     *  ME      - processor ID in this communicator
!     *  NPROC   - number of processors in this communicator
!     *  ISTAT   - status of call, 0 is OK.
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE gc_globals_mod, ONLY:                                                      &
    gc__me,                                                                    &
    gc__nproc

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    gc__my_mpi_comm_world
#endif

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

#include "gc_constants.h"
#include "gc_functions.h"

INTEGER (KIND=gc_int_kind), INTENT(IN) :: val
INTEGER (KIND=gc_int_kind), INTENT(OUT) :: me
INTEGER (KIND=gc_int_kind), INTENT(OUT) :: nproc
INTEGER (KIND=gc_int_kind), INTENT(OUT) :: istat

INTEGER (KIND=gc_int_kind)              :: info


#if defined(MPI_SRC)
gc__my_mpi_comm_world = val
CALL mpl_comm_rank(gc__my_mpi_comm_world, me, info)
CALL mpl_comm_size(gc__my_mpi_comm_world, nproc, info)
#elif defined(SERIAL_SRC)
me = 0
nproc = 1
#endif

gc__nproc = nproc
gc__me    = me
istat     = gc__ok

RETURN
END SUBROUTINE gc_set_communicator
