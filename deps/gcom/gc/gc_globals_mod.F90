! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

MODULE gc_globals_mod
!     *****************************************************************
!     * Purpose:
!     * Global variables - replacing many common blocks
!     *****************************************************************
#if defined(MPI_SRC)
USE mpl, ONLY:                                                                 &
    mpl_address_kind
#endif

USE gc_kinds_mod, ONLY:                                                        &
    gc_int_kind,                                                               &
    gc_log_kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind), SAVE :: gc__nproc = -1 ! Number of processors
INTEGER (KIND=gc_int_kind), SAVE :: gc__me = -1    ! My processor

! Has GCOM been initialized?
LOGICAL (KIND=gc_log_kind), SAVE :: gc__inited = .FALSE.

! Has GCOM been finalized?
LOGICAL (KIND=gc_log_kind), SAVE :: gc__finaled = .FALSE.

#if defined(MPI_SRC)
INTEGER (KIND=mpl_address_kind):: gc__mpi_maxtag        ! Maximum tag
INTEGER (KIND=gc_int_kind)     :: gc__my_mpi_comm_world ! Communicator

INTEGER, PARAMETER             :: max_mpi_types = 1024  ! Hard-wired
INTEGER (KIND=gc_int_kind)     :: mpi_types_array(5, max_mpi_types)
INTEGER (KIND=gc_int_kind), SAVE :: n_mpi_types_defined = 0 ! How many defined
INTEGER (KIND=gc_int_kind), SAVE :: mpi_type_seq = 0
#endif

! Maximum number of rotations
INTEGER (KIND=gc_int_kind), PARAMETER :: max_rotate = 8192

! Maximum number of options
INTEGER (KIND=gc_int_kind), PARAMETER :: gc__max_opts = 3

! Options array
INTEGER (KIND=gc_int_kind)            :: gc__options(gc__max_opts)

END MODULE gc_globals_mod
