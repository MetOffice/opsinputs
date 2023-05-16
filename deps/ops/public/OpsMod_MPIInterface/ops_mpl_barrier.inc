!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for mpl_barrier.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_barrier (comm,  &
                            error)
USE mpl, ONLY: &
  gc_int_kind

! Subroutine arguments:
INTEGER(kind=gc_int_kind) :: comm
INTEGER(kind=gc_int_kind) :: error

! Local declarations:
EXTERNAL mpl_barrier

CALL mpl_barrier (comm,  &
                  error)

END SUBROUTINE ops_mpl_barrier
