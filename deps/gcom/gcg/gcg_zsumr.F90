! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gcg_prolog.h"

SUBROUTINE gcg_zsumr (len1, gid, istat, ssum)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Calculate in a reproducible way the complex sum across all
!     *  processors of a group and distribute the result to all members
!     *  of the group.
!     *
!     * Input:
!     *  LEN1    - number of elements in message
!     *  GID     - processor group ID
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

USE gc_kinds_mod, ONLY: gc_int_kind, gc_real_kind

IMPLICIT NONE

#include "gc_constants.h"
#include "gcg_constants.h"
#include "gcg_mtags.h"

INTEGER (KIND=gc_int_kind) :: len1, gid, istat
COMPLEX (KIND=gc_real_kind) :: ssum(len1)


#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: l, nproc, igid, grank, gsize

COMPLEX (KIND=gc_real_kind), ALLOCATABLE :: reduce_data_wrk(:,:)
#include "gc_functions.h"
#endif

INTEGER (KIND=gc_int_kind) :: i

istat = gc__ok

#if defined(MPI_SRC)
IF (gid  ==  gcg__allgroup) THEN
  nproc = gc_nproc()
  CALL gc_zsumr(len1, nproc, istat, ssum)
  RETURN
ELSE
  igid = gid
  CALL mpl_comm_rank(igid, grank, istat)
  CALL mpl_comm_size(igid, gsize, istat)

  ALLOCATE( reduce_data_wrk(len1, gsize) )

  CALL mpl_gather(ssum, len1, mpl_complex, reduce_data_wrk, len1,              &
                  mpl_complex, 0_gc_int_kind, igid, istat)

  IF (grank  ==  0) THEN
    ssum(:) = CMPLX(0.0,0.0)
    DO i = 1, gsize
      DO l = 1,len1
        ssum(l) = ssum(l) + reduce_data_wrk(l,i)
      END DO
    END DO
  END IF

  CALL mpl_bcast(ssum, len1, mpl_complex, 0_gc_int_kind, igid, istat)

  DEALLOCATE( reduce_data_wrk )
END IF
#endif

RETURN
END SUBROUTINE gcg_zsumr
