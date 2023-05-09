! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_init_thread (me, nproc, requested)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Initialize all (machine dependent) variables used in the
!     *  communication and set the threading model used.
!     *
!     * Input:
!     *  REQUESTED  - level of threading requested
!     *
!     * Output:
!     *  NPROC   - number of nodes
!     *  ME      - my node ID, 0..NPROC-1
!     *
!     * NOTES:
!     *
!     * Implementation:
!     *  Split into GC_INIT_INTRO_THREAD(COMM, REQUESTED)
!     *         and GC_INIT_FINAL(ME,NPROC,COMM)
!     *  COMM (INTEGER) is output from INTRO and input into FINAL
!     *  and is the MPI global communicator (undefined for other
!     *  communications systems) - this allows the user to call
!     *  INTRO & FINAL seperately rather than the normal GC_INIT
!     *  and intercept the global communicator and replace with their
!     *  own MPI communicator if required.
!     *
!     ******************************************************************

USE mpl, ONLY:                                                                 &
    mpl_thread_multiple,mpl_thread_serialized,mpl_thread_funneled,             &
    mpl_thread_single

USE gc_kinds_mod, ONLY:                                                        &
    gc_int_kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind) :: me, nproc
INTEGER (KIND=gc_int_kind) :: requested ! mpi_threading level requested
INTEGER (KIND=gc_int_kind) :: actual ! mpi_threading level active
INTEGER (KIND=gc_int_kind) :: info

INTEGER (KIND=gc_int_kind) :: comm

CALL gc_init_intro_thread(comm,requested)
CALL gc_init_final(me,nproc,comm)
#if defined(MPI_SRC)
CALL mpl_query_thread(actual,info)
#else
actual=mpl_thread_multiple
#endif

IF (me==0) THEN
  IF (actual/=requested) THEN
    WRITE(6,'(A)')'WARNING - REQUESTED AND ACTUAL THREADING LEVEL DIFFERENT'
  END IF
  IF (requested == mpl_thread_multiple)                                        &
    WRITE(6,'(A)')'THREAD LEVEL REQUESTED is MPL_THREAD_MULTIPLE'
  IF (requested == mpl_thread_serialized)                                      &
    WRITE(6,'(A)')'THREAD LEVEL REQUESTED is MPL_THREAD_SERIALIZED'
  IF (requested == mpl_thread_funneled)                                        &
    WRITE(6,'(A)')'THREAD LEVEL REQUESTED is MPL_THREAD_FUNNELED'
  IF (requested == mpl_thread_single)                                          &
    WRITE(6,'(A)')'THREAD LEVEL REQUESTED is MPL_THREAD_SINGLE'
  IF (actual==mpl_thread_multiple) WRITE(6,'(A)')'THREAD LEVEL SET is MPL_THREAD_MULTIPLE'
  IF (actual==mpl_thread_serialized) WRITE(6,'(A)')'THREAD LEVEL SET is MPL_THREAD_SERIALIZED'
  IF (actual==mpl_thread_funneled) WRITE(6,'(A)')'THREAD LEVEL SET is MPL_THREAD_FUNNELED'
  IF (actual==mpl_thread_single) WRITE(6,'(A)')'THREAD LEVEL SET is MPL_THREAD_SINGLE'
END IF

RETURN
END SUBROUTINE gc_init_thread

SUBROUTINE gc_init_intro_thread (comm, requested)

USE mpl, ONLY:                                                                 &
    mpl_comm_world

USE gc_kinds_mod, ONLY:                                                        &
#if defined(MPI_SRC)
    gc_log_kind,                                                               &
#endif
    gc_int_kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind) :: requested ! mpi_threading level requested
INTEGER (KIND=gc_int_kind) :: actual ! mpi_threading level allowed
INTEGER (KIND=gc_int_kind) :: comm ! OUT : Global communicator from MPI (otherwise 0)

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: info
LOGICAL (KIND=gc_log_kind) :: flag
#endif

comm=0

#if defined(MPI_SRC)
CALL mpl_initialized(flag,info)
! Only initialise MPI if is isn't already done
IF (.NOT. flag) THEN
  CALL mpl_init_thread(requested,actual,info)
END IF

comm=mpl_comm_world
#endif

RETURN
END SUBROUTINE gc_init_intro_thread
