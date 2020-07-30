! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Win_Flush()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Completes all outstanding RMA operations initiated by the
!     *  calling process to the target rank on the specified window
!     *
!     *  Output: error
!     *
!     ******************************************************************

SUBROUTINE MPL_Win_Flush (RANK, win, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  RANK,                                                                        &
  win,                                                                         &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_rank,                                                                      &
  l_win,                                                                       &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_rank   = RANK
l_win    = win

CALL MPI_Win_Flush(l_rank, l_win, l_error)

ERROR = l_error
#endif

RETURN
END SUBROUTINE MPL_Win_Flush
