! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

#include "gcg_prolog.h"

SUBROUTINE gcg_rsumr_kind32 (len1, gid, istat, rsum)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Calculate in a reproducible way the real sum across all
!     *  processors of a group and distribute the result to all members
!     *  of the group.
!     *
!     * Input:
!     *  LEN1    - number of elements in message
!     *  GID     - processor group ID
!     *  RSUM    - array with elements to be added up across the nodes
!     *
!     * Output:
!     *  RSUM    - array containing the sums across the nodes
!     *  ISTAT   - status of rsum. 0 is OK (MPI_SRC only),
!     *            refer to the header files for nonzero status codes
!     *
!     * NOTES:   This is the same as GCG_RSUMR but is explicitly 32 bit
!     *          even when compiled with 64bit flag. Only the reals are
!     *          single precision.
!     *
!     ******************************************************************

USE mpl, ONLY:                                                                 &
    mpl_real4

USE gc_kinds_mod, ONLY: gc_int_kind, gc_real32

IMPLICIT NONE

#include "gc_constants.h"
#include "gcg_constants.h"
#include "gcg_mtags.h"

INTEGER (KIND=gc_int_kind) :: len1, gid, istat
REAL (KIND=gc_real32)   :: rsum(len1)


#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: l, nproc, igid, grank, gsize

REAL (KIND=gc_real32), ALLOCATABLE   :: reduce_data_wrk(:,:)
#include "gc_functions.h"
#endif

INTEGER (KIND=gc_int_kind) :: i

istat = gc__ok

#if defined(MPI_SRC)
IF (gid  ==  gcg__allgroup) THEN
  nproc = gc_nproc()
  CALL gc_rsumr_kind32(len1, nproc, istat, rsum)
  RETURN
ELSE
  igid = gid
  CALL mpl_comm_rank(igid, grank, istat)
  CALL mpl_comm_size(igid, gsize, istat)

  ALLOCATE( reduce_data_wrk(len1, gsize) )

  CALL mpl_gather(rsum, len1, mpl_real4, reduce_data_wrk, len1,                &
                  mpl_real4, 0_gc_int_kind, igid, istat)

  IF (grank  ==  0) THEN
    rsum(:) = 0.0
    DO i = 1, gsize
      DO l = 1,len1
        rsum(l) = rsum(l) + reduce_data_wrk(l,i)
      END DO
    END DO
  END IF

  CALL mpl_bcast(rsum, len1, mpl_real4, 0_gc_int_kind, igid, istat)

  DEALLOCATE( reduce_data_wrk )
END IF
#endif

RETURN
END SUBROUTINE gcg_rsumr_kind32
