!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for mpl_win_shared_query for integer arguments.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_win_shared_query (win,          &
                                     rank,         &
                                     segment_size, &
                                     disp_unit,    &
                                     baseptr,      &
                                     error)

USE mpl, ONLY: &
  gc_int_kind

USE, INTRINSIC :: ISO_C_BINDING, ONLY: &
  C_PTR

! Subroutine arguments:
INTEGER(kind=gc_int_kind) :: win
INTEGER(kind=gc_int_kind) :: rank
INTEGER(kind=gc_int_kind) :: segment_size
INTEGER(kind=gc_int_kind) :: disp_unit
TYPE(C_PTR)               :: baseptr
INTEGER(kind=gc_int_kind) :: error

! Local declarations:
EXTERNAL mpl_win_shared_query

CALL mpl_win_shared_query (win,          &
                           rank,         &
                           segment_size, &
                           disp_unit,    &
                           baseptr,      &
                           error)

END SUBROUTINE ops_mpl_win_shared_query
