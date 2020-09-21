!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for mpl_comm_size.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_comm_size (comm,  &
                              size,  &
                              error)
USE mpl, ONLY: &
  gc_int_kind

! Subroutine arguments:
INTEGER(kind=gc_int_kind) :: comm
INTEGER(kind=gc_int_kind) :: size
INTEGER(kind=gc_int_kind) :: error

! Local declarations:
EXTERNAL mpl_comm_size

CALL mpl_comm_size (comm,  &
                    size,  &
                    error)

END SUBROUTINE ops_mpl_comm_size
