! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Finalized ()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Check if MPI has been finalized
!     *
!     *  Output:  flag, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Finalized (flag, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind, gc_log_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Log_Kind

IMPLICIT NONE

LOGICAL (KIND=gc_log_kind) :: flag      ! Whether MPI has been finalized
INTEGER (KIND=gc_int_kind) :: ERROR     ! Error code to return

LOGICAL (KIND=mpl_log_kind) ::                                                 &
  l_flag                        ! Flag returned by MPI_FINALIZED
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_error                       ! Error code returned by MPI_FINALIZED

!=======================================================================

CALL MPI_Finalized(l_flag, l_error)

flag  = l_flag
ERROR = l_error

RETURN
END SUBROUTINE MPL_Finalized
