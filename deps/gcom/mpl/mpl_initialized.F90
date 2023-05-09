! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Initialized ()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Check if MPI is initialized
!     *
!     *  Output:  flag, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Initialized (flag, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind, gc_log_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Log_Kind

IMPLICIT NONE

LOGICAL (KIND=gc_log_kind) :: flag      ! Return status of init
INTEGER (KIND=gc_int_kind) :: ERROR     ! Error code to return

LOGICAL (KIND=mpl_log_kind) ::                                                 &
  l_flag                        ! Flag returned by MPI_INIT
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_error                       ! Error code returned by MPI_INIT

!=======================================================================

CALL MPI_Initialized(l_flag, l_error)

flag  = l_flag
ERROR = l_error

RETURN
END SUBROUTINE MPL_Initialized
