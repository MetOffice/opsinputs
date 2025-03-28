!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Basic initialisation of MPI for OPS.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_InitMPI

USE GenMod_Control, ONLY: &
  mype,                   &
  nproc

USE OpsMod_Gcom, ONLY: &
  gcg_split

IMPLICIT NONE

! Local declarations:
INTEGER :: istat

!---------------------------------
! 0. Initialise and Setup control
!---------------------------------

CALL gcg_split (mype,      &
                nproc,     &
                0,         &
                istat,     &
                mpi_group)

END SUBROUTINE Ops_InitMPI
