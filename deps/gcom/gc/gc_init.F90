! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"

SUBROUTINE gc_init (path, me, nproc)
!     ******************************************************************
!     * Purpose:
!     *
!     *  Initialize all (machine dependent) variables used in the
!     *  communication.
!     *
!     * Input:
!     *  PATH    - OBSOLETE
!     *
!     * Output:
!     *  NPROC   - number of nodes
!     *  ME      - my node ID, 0..NPROC-1
!     *
!     * NOTES:
!     *
!     * Implementation:
!     *  Split into GC_INIT_INTRO(COMM)
!     *         and GC_INIT_FINAL(ME,NPROC,COMM)
!     *  COMM (INTEGER) is output from INTRO and input into FINAL
!     *  and is the MPI global communicator (undefined for other
!     *  communications systems) - this allows the user to call
!     *  INTRO & FINAL seperately rather than the normal GC_INIT
!     *  and intercept the global communicator and replace with their
!     *  own MPI communicator if required.
!     *
!     *
!     ******************************************************************

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

CHARACTER(LEN=*) :: path
INTEGER (KIND=gc_int_kind) :: me, nproc

INTEGER (KIND=gc_int_kind) :: comm

CALL gc_init_intro(comm)
CALL gc_init_final(me,nproc,comm)

RETURN
END SUBROUTINE gc_init

SUBROUTINE gc_init_intro (comm)

USE mpl, ONLY:                                                                 &
    mpl_comm_world

USE gc_kinds_mod, ONLY:                                                        &
#if defined(MPI_SRC)
    gc_log_kind,                                                               &
#endif
    gc_int_kind

IMPLICIT NONE

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
  CALL mpl_init(info)
END IF

comm=mpl_comm_world
#endif

RETURN
END SUBROUTINE gc_init_intro

SUBROUTINE gc_init_final (me,nproc,comm)

USE mpl, ONLY:                                                                 &
    mpl_tag_ub,                                                                &
    mpl_comm_world

USE gc_globals_mod, ONLY:                                                      &
    gc__me,                                                                    &
    gc__nproc,                                                                 &
    gc__inited

USE gcom_mod ,ONLY:                                                            &
    gc_force_bitrep,                                                           &
    gc_alltoall_version,                                                       &
    gc_off,                                                                    &
    gc_alltoall_orig,                                                          &
    gc_is_parallel,                                                            &
    gc_parallel,                                                               &
    gc_serial

#if defined(MPI_SRC)
USE gc_globals_mod, ONLY:                                                      &
    gc__my_mpi_comm_world,                                                     &
    gc__mpi_maxtag

USE gc__buildconst, ONLY: gc__isize, mpi_bsend_buffer_size
#endif

USE gc_kinds_mod, ONLY:                                                        &
#if defined(MPI_SRC)
    gc_log_kind,                                                               &
#endif
    gc_int_kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind) :: me, nproc
INTEGER (KIND=gc_int_kind) :: comm ! IN : Global communicator to
                                   !       use for MPI

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind)       :: info
LOGICAL (KIND=gc_log_kind)       :: mpiflag
INTEGER (KIND=gc_int_kind), SAVE ::                                            &
                            bsend_buffer(mpi_bsend_buffer_size)
#endif

INTEGER (KIND=gc_int_kind)       :: i
INTEGER (KIND=gc_int_kind)       :: ierr

IF (gc__inited) THEN
  RETURN
END IF

#if defined(MPI_SRC)
gc__my_mpi_comm_world=comm

CALL mpl_comm_rank(gc__my_mpi_comm_world, me, info)
CALL mpl_comm_size(gc__my_mpi_comm_world, nproc, info)

! Warning!! If using the mpi_64bit_fixes then this routine will
! only return the value of MPI_TAG_UB, no matter what is requested.
! This is hardcoded into the the C function in mpi_c_fix.c

CALL mpl_comm_get_attr(mpl_comm_world,mpl_tag_ub,                              &
                       gc__mpi_maxtag,mpiflag,info)

CALL mpl_buffer_attach(bsend_buffer,                                           &
                      mpi_bsend_buffer_size*gc__isize, ierr)

#endif

#if defined(SERIAL_SRC)
me = 0
nproc = 1
#endif

gc__nproc = nproc
gc__me = me

! Set default options
! Non-bit reproducible options by default
CALL gc_setopt(gc_force_bitrep, gc_off, ierr)
! Original RALLTOALLE
CALL gc_setopt(gc_alltoall_version, gc_alltoall_orig, ierr)
#if defined(MPI_SRC)
CALL gc_setopt(gc_is_parallel, gc_parallel, ierr)
#else
CALL gc_setopt(gc_is_parallel, gc_serial, ierr)
#endif

gc__inited = .TRUE.

IF (gc__me  ==  0) CALL gc__stamp()

RETURN
END SUBROUTINE gc_init_final
