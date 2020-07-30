! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_abort (me, nproc, mesg)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Aborts program execution on all processors and clean up
!     *  the parallel system.
!     *
!     * Input:
!     *  ME      - my node ID, 0..NPROC-1
!     *  NPROC   - number of nodes (unused)
!     *  MESG    - abort message to be printed on stdout
!     *
!     * Output:
!     *
!     * NOTES:
!     *
!     ******************************************************************

#if defined(MPI_SRC)
USE mpl, ONLY: mpl_comm_world
#endif

USE gc__buildconst, ONLY:                                                      &
#if defined(MPI_SRC)
  mpiabort_errno,                                                              &
#endif
  gc__forterrunit

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind) :: me, nproc, i, info
CHARACTER(LEN=*) :: mesg
CHARACTER(LEN=*), PARAMETER     :: seqf2       = '(a20,i5,a3,a)'

WRITE(gc__forterrunit,seqf2) 'gc_abort (Processor ',me,'): ', mesg
CALL gc__flush(gc__forterrunit)
CALL gc__flush(INT(OUTPUT_UNIT, KIND=gc_int_kind))

#if defined(IBM)
CALL xl__trbk()
#endif

#if defined(MPI_SRC)
CALL mpl_abort(mpl_comm_world, mpiabort_errno, info)
#endif

!---  Should only get through to here if no other abort facility
!---  has been found. Use the C stdlib abort via gc__abort.
CALL gc__abort()

RETURN
END SUBROUTINE gc_abort
