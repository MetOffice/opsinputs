!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for mpl_win_allocate_shared for integer arguments.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_win_allocate_shared (win_size,  &
                                        disp_unit, &
                                        info,      &
                                        comm,      &
                                        baseptr,   &
                                        win,       &
                                        error)

USE mpl, ONLY: &
  gc_int_kind

USE, INTRINSIC :: ISO_C_BINDING, ONLY: &
  C_PTR

! Subroutine arguments:
INTEGER(kind=gc_int_kind) :: win_size
INTEGER(kind=gc_int_kind) :: disp_unit
INTEGER(kind=gc_int_kind) :: info
INTEGER(kind=gc_int_kind) :: comm
TYPE (C_PTR)              :: baseptr
INTEGER(kind=gc_int_kind) :: win
INTEGER(kind=gc_int_kind) :: error

! Local declarations:
EXTERNAL mpl_win_allocate_shared

CALL mpl_win_allocate_shared (win_size,  &
                              disp_unit, &
                              info,      &
                              comm,      &
                              baseptr,   &
                              win,       &
                              error)

END SUBROUTINE ops_mpl_win_allocate_shared
