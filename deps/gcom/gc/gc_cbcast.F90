! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_cbcast (msg, len1, send, nproc, istat, sarr)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Broadcast a character array to every processor.
!     *
!     * Input:
!     *  MSG     - message tag
!     *  LEN1    - number of characters in message
!     *  SEND    - sender of the message
!     *  NPROC   - Number of processors
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

INTEGER (KIND=gc_int_kind) :: msg, len1, send, nproc, istat
CHARACTER(LEN=*) :: sarr

#if defined (MPI_SRC)
CALL mpl_bcast(sarr, len1, mpl_character, send,                                &
               gc__my_mpi_comm_world,istat)
#endif

#if defined(SERIAL_SRC)
istat = gc__ok
#endif

RETURN
END SUBROUTINE gc_cbcast
