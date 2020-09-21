! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gcg_prolog.h"

FUNCTION gcg_me (gid)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Return the rank of my node in this group, in the
!     *  range 0...groupsize(GID)-1 or -1 of invalid GID / not a member.
!     *
!     * Input:
!     *  GID     - processor group ID
!     *
!     * Output:
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

#include "gcg_constants.h"

INTEGER (KIND=gc_int_kind) :: gcg_me
INTEGER (KIND=gc_int_kind) :: gid

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: istat, igid, grank
#endif

#if defined(MPI_SRC)
IF (gid  ==  gcg__allgroup) THEN
  igid = gc__my_mpi_comm_world
ELSE
  igid = gid
END IF
CALL mpl_comm_rank(igid, grank, istat)
gcg_me = grank
#endif

#if defined(SERIAL_SRC)
gcg_me = 0
#endif

RETURN
END FUNCTION gcg_me
