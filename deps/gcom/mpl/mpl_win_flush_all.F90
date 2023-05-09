! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Win_Flush_All()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Completes all outstanding RMA operations to all targets
!     *
!     *  Output: error
!     *
!     ******************************************************************

SUBROUTINE MPL_Win_Flush_All (win, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  win,                                                                         &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_win,                                                                       &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_win    = win

CALL MPI_Win_Flush_All(l_win, l_error)

ERROR = l_error
#endif

RETURN
END SUBROUTINE MPL_Win_Flush_All
