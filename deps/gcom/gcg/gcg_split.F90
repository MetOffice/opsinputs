! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gcg_prolog.h"

SUBROUTINE gcg_split(me, nproc, color, istat, gid)
!     ******************************************************************
!     * Purpose:
!     *
!     *  GC groups routine. Splits processors in disjoint groups based
!     *  on a color attribute.
!     *
!     * Input:
!     *  ME      - my node ID
!     *  NPROC   - number of nodes
!     *  COLOR   - color attribute for this node
!     *
!     * Output:
!     *  GID     - group identifier
!     *  ISTAT   - status of split 0 is OK
!     *
!     * NOTES:
!     *  GCG_SPLIT() is a synchronizing call.
!     *
!     ******************************************************************

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    gc__my_mpi_comm_world
#endif

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

#include "gc_constants.h"

INTEGER (KIND=gc_int_kind) :: me, nproc, color, gid, istat

istat = gc__ok

#if defined(MPI_SRC)
CALL mpl_comm_split(gc__my_mpi_comm_world, color,                              &
                    0_gc_int_kind, gid, istat)
#endif

RETURN
END SUBROUTINE gcg_split
