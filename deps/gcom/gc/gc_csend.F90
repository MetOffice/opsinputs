! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_csend (msg, len1, reci, istat, rarr, sarr)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Send a character array from this processor to processor RECI.
!     *
!     * Input:
!     *  MSG     - message tag
!     *  LEN1    - number of characters in message
!     *  RECI    - receiver of the message
!     *  RARR    - name of the array on recieving processor
!     *            (Obsolete)
!     *  SARR    - array to be sent
!     *
!     * Output:
!     *  ISTAT   - status of send. 0 is OK (MPI_SRC only),
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

INTEGER (KIND=gc_int_kind) :: msg, len1, reci, istat
CHARACTER(LEN=*)           :: rarr, sarr

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: me
#include "gc_functions.h"
#endif

#if defined(MPI_SRC)
me=gc_me()
IF (reci  /=  me) THEN  ! If I'm not sending to myself
  CALL mpl_bsend(sarr, len1, mpl_character, reci, msg,                         &
       gc__my_mpi_comm_world, istat)
END IF
#endif

#if defined(SERIAL_SRC)
rarr=sarr
istat = gc__ok
#endif

RETURN
END SUBROUTINE gc_csend
