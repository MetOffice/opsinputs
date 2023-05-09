! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Win_Lock()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Starts an RMA access epoch.
!     *
!     *  Output: error
!     *
!     ******************************************************************

SUBROUTINE MPL_Win_Lock (LOCK, RANK, assert, win, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  LOCK,                                                                        &
  RANK,                                                                        &
  assert,                                                                      &
  win,                                                                         &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_lock,                                                                      &
  l_rank,                                                                      &
  l_assert,                                                                    &
  l_win,                                                                       &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_lock   = LOCK
l_rank   = RANK
l_assert = assert
l_win    = win

CALL MPI_Win_Lock(l_lock, l_rank, l_assert, l_win, l_error)

ERROR = l_error
#endif

RETURN
END SUBROUTINE MPL_Win_Lock
