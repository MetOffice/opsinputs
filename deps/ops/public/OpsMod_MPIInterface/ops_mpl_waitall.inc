!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for mpl_waitall.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_waitall (count,    &
                            requests, &
                            statuses, &
                            error)
USE mpl, ONLY:     &
  gc_int_kind,     &
  mpl_status_size

! Subroutine arguments:
INTEGER(kind=gc_int_kind) :: count
INTEGER(kind=gc_int_kind) :: requests(count)
INTEGER(kind=gc_int_kind) :: statuses(mpl_status_size,count)
INTEGER(kind=gc_int_kind) :: error

! Local declarations:
EXTERNAL mpl_waitall

CALL mpl_waitall (count,    &
                  requests, &
                  statuses, &
                  error)

END SUBROUTINE ops_mpl_waitall
