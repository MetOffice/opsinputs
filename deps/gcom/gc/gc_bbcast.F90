! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_bbcast (msg, len1, send, nproc, istat, sarr)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Broadcast a byte array to every processor.
!     *
!     * Input:
!     *  MSG     - message tag
!     *  LEN1    - number of BYTES in message
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
    mpl_byte

USE gc__buildconst, ONLY: gc__isize

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    gc__my_mpi_comm_world
#endif

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

#include "gc_constants.h"

INTEGER (KIND=gc_int_kind) :: msg, len1, send, nproc, istat,                   &
                              sarr(len1/gc__isize+1)

#if defined (MPI_SRC)
CALL mpl_bcast(sarr, len1, mpl_byte, send,                                     &
               gc__my_mpi_comm_world, istat)
#endif

#if defined(SERIAL_SRC)
istat = gc__ok
#endif

RETURN
END SUBROUTINE gc_bbcast
