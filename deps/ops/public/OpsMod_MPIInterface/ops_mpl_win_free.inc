!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for mpl_win_free for integer arguments.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_win_free (win, &
                             error)

USE mpl, ONLY: &
  gc_int_kind

! Subroutine arguments:
INTEGER(kind=gc_int_kind) :: win
INTEGER(kind=gc_int_kind) :: error

! Local declarations:
EXTERNAL mpl_win_free

CALL mpl_win_free (win, &
                   error)

END SUBROUTINE ops_mpl_win_free
