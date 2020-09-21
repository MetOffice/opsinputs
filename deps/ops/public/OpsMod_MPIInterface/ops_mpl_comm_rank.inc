!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for mpl_comm_rank.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_comm_rank (comm,  &
                              rank,  &
                              error)
USE mpl, ONLY: &
  gc_int_kind

! Subroutine arguments:
INTEGER(kind=gc_int_kind) :: comm
INTEGER(kind=gc_int_kind) :: rank
INTEGER(kind=gc_int_kind) :: error

! Local declarations:
EXTERNAL mpl_comm_rank

CALL mpl_comm_rank (comm,  &
                    rank,  &
                    error)

END SUBROUTINE ops_mpl_comm_rank