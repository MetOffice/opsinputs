! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gcg_prolog.h"

SUBROUTINE gcg_cbcast (msg, len1, send, gid, istat, sarr)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Broadcast a character array to every processor of a group.
!     *
!     * Input:
!     *  MSG     - message tag (deprecated)
!     *  LEN1    - number of characters in message
!     *  SEND    - sender of the message (global rank)
!     *  GID     - processor group ID
!     *  SARR    - array to be sent
!     *
!     * Output:
!     *  SARR    - array to be received (on nodes != SEND)
!     *  ISTAT   - status of bcast. 0 is OK (MPI_SRC only),
!     *            refer to the header files for nonzero status codes
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE mpl, ONLY:                                                                 &
    mpl_character

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    gc__my_mpi_comm_world
#endif

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

#include "gc_constants.h"
#include "gcg_constants.h"

INTEGER (KIND=gc_int_kind) :: msg, len1, send, gid, istat
CHARACTER(LEN=*) :: sarr

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: igid, RANK, gcg__mpi_rank
#endif

istat = gc__ok

#if defined(MPI_SRC)
IF (gid  ==  gcg__allgroup) THEN
  igid = gc__my_mpi_comm_world
ELSE
  igid = gid
END IF
istat = gcg__mpi_rank(send, igid)
IF (istat  ==  -1) RETURN
RANK = istat
CALL mpl_bcast(sarr, len1, mpl_character, RANK,                                &
               igid, istat)
#endif

RETURN
END SUBROUTINE gcg_cbcast
