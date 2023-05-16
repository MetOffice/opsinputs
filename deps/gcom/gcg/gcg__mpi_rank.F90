! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

!=======================================================================
!  This is an INTERNAL routine to be used within the GCG interface ONLY.
!=======================================================================

#include "gcg_prolog.h"

#if defined(MPI_SRC)
FUNCTION gcg__mpi_rank(RANK, gid)

!     MPI Global to local rank translation routine. Translates
!     RANK in communicator GC__MY_MPI_COMM_WORLD to that of communicator
!     GID if RANK is a member of GID.

USE gc_globals_mod, ONLY:                                                      &
    gc__my_mpi_comm_world

USE gc_kinds_mod, ONLY: gc_int_kind, gc_log_kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind) :: RANK, gid, gcg__mpi_rank
INTEGER (KIND=gc_int_kind) :: i, size1, igid, istat
LOGICAL (KIND=gc_log_kind), SAVE :: initmpi = .FALSE.
INTEGER (KIND=gc_int_kind), SAVE :: gidw

INTEGER (KIND=gc_int_kind), ALLOCATABLE, SAVE :: ranks(:,:)

IF (.NOT. initmpi) THEN
  initmpi = .TRUE.
  CALL mpl_comm_group(gc__my_mpi_comm_world, gidw, istat)

  CALL mpl_comm_size(gc__my_mpi_comm_world, size1, istat)
  ALLOCATE (ranks(0:size1-1, 2))
  DO i = 0, size1-1
    ranks(i,1) = i
  END DO
END IF

!---  Translate ranks from GID to GC__MY_MPI_COMM_WORLD GID
CALL mpl_comm_size(gid, size1, istat)
CALL mpl_comm_group(gid, igid, istat)
CALL mpl_group_translate_ranks(igid, size1, ranks(0,1), gidw,                  &
     ranks(0,2), istat)
CALL mpl_group_free(igid, istat)

!---  Search for specified global rank in GID group
DO i = 0, size1-1
  IF (ranks(i,2)  ==  RANK) THEN
    gcg__mpi_rank = i
    RETURN
  END IF
END DO

!---  Global rank RANK not found in GID:
gcg__mpi_rank = -1

RETURN
END FUNCTION gcg__mpi_rank

#else

FUNCTION gcg__mpi_rank(dummy1,dummy2)

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind) :: dummy1,dummy2,gcg__mpi_rank

#include "gc_functions.h"

CALL gc_abort(gc_me(),gc_nproc(),                                              &
              'GCG__MPI_RANK called for non-MPI')

gcg__mpi_rank = -1

RETURN
END FUNCTION gcg__mpi_rank
#endif
