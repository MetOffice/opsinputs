! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_rrecv (msg, len1, send, istat, rarr, sarr)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Receive a real array from processor SEND.
!     *
!     * Input:
!     *  MSG     - message tag
!     *  LEN1    - number of elements in message
!     *  SEND    - sender of the message (SEND = GC_ANY means any
!     *            processor)
!     *  SARR    - name of the array on the sending processor
!     *            (Obsolete)
!     *
!     * Output:
!     *  RARR    - array to be received
!     *  ISTAT   - status of send 0 is OK (MPI_SRC only),
!     *            refer to the header files for nonzero status codes
!     *
!     * NOTES:
!     *  The use of ISTAT as an input argument is obsoleted. Use
!     *  GC_SETOPT().
!     *
!     ******************************************************************

USE mpl, ONLY:                                                                 &
    mpl_status_size,                                                           &
    mpl_any_source,                                                            &
    mpl_byte

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    gc__my_mpi_comm_world

USE gc__buildconst, ONLY: gc__rsize
#endif

USE gc_kinds_mod, ONLY: gc_int_kind, gc_real_kind

IMPLICIT NONE

#include "gc_constants.h"

INTEGER (KIND=gc_int_kind) :: msg, len1, send, istat
REAL (KIND=gc_real_kind)   :: rarr(len1), sarr(len1)

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: STAT(mpl_status_size)
INTEGER (KIND=gc_int_kind) :: me,i
#include "gc_functions.h"
#endif

#if defined(MPI_SRC)
me=gc_me()
IF (send  ==  me) THEN  ! Receiving from myself
  DO i=1,len1
    rarr(i)=sarr(i)
  END DO
ELSE ! Receiving from another processor
  IF (send  ==  gc__any) THEN
    CALL mpl_recv(rarr, gc__rsize*len1, mpl_byte,                              &
         mpl_any_source,                                                       &
         msg, gc__my_mpi_comm_world, STAT, istat)
  ELSE
    CALL mpl_recv(rarr, gc__rsize*len1, mpl_byte,                              &
         send, msg,                                                            &
         gc__my_mpi_comm_world, STAT, istat)
  END IF
END IF ! IF (send  ==  me)
#endif

#if defined(SERIAL_SRC)
istat = gc__ok
#endif

RETURN
END SUBROUTINE gc_rrecv
