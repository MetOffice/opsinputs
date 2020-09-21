! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_zsumr (len1, nproc, istat, ssum)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Calculate in a reproducible way the complex sum across all
!     *  processors and distribute the result to all the processors.
!     *
!     * Input:
!     *  LEN1    - number of elements in message
!     *  NPROC   - number of processors
!     *  SSUM    - array with elements to be added up across the nodes
!     *
!     * Output:
!     *  SSUM    - array containing the sums across the nodes
!     *  ISTAT   - status of zsumr. 0 is OK (MPI_SRC only),
!     *            refer to the header files for nonzero status codes
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE mpl, ONLY:                                                                 &
    mpl_complex

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    gc__my_mpi_comm_world
#endif

USE gc_kinds_mod, ONLY: gc_int_kind, gc_real_kind

IMPLICIT NONE

#include "gc_mtags.h"
#include "gc_constants.h"

INTEGER (KIND=gc_int_kind) :: len1, nproc, istat
COMPLEX (KIND=gc_real_kind) :: ssum(len1)
COMPLEX (KIND=gc_real_kind) :: reduce_data_wrk(len1,nproc)

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: i, l, me
#include "gc_functions.h"
#endif

#if defined(MPI_SRC)
me = gc_me()

CALL mpl_gather(ssum, len1, mpl_complex, reduce_data_wrk, len1,                &
                mpl_complex, gc__ionode, gc__my_mpi_comm_world,                &
                istat)

IF (me  ==  gc__ionode) THEN
  ssum(:) = CMPLX(0.0,0.0)
  DO i = 1, nproc
    DO l = 1,len1
      ssum(l) = ssum(l) + reduce_data_wrk(l,i)
    END DO
  END DO
END IF

CALL mpl_bcast(ssum, len1, mpl_complex, gc__ionode,                            &
               gc__my_mpi_comm_world, istat)

#endif

#if defined(SERIAL_SRC)
istat = gc__ok
#endif

RETURN
END SUBROUTINE gc_zsumr
