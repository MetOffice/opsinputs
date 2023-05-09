!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for mpl_comm_split_type for integer arguments.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_comm_split_type (comm,       &
                                    split_type, &
                                    key,        &
                                    info,       &
                                    newcomm,    &
                                    error)
USE mpl, ONLY: &
  gc_int_kind

! Subroutine arguments:
INTEGER(kind=gc_int_kind) :: comm
INTEGER(kind=gc_int_kind) :: split_type
INTEGER(kind=gc_int_kind) :: key
INTEGER(kind=gc_int_kind) :: info
INTEGER(kind=gc_int_kind) :: newcomm
INTEGER(kind=gc_int_kind) :: error

! Local declarations:
EXTERNAL mpl_comm_split_type

CALL mpl_comm_split_type (comm,       &
                          split_type, &
                          key,        &
                          info,       &
                          newcomm,    &
                          error)

END SUBROUTINE ops_mpl_comm_split_type
