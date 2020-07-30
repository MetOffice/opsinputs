!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for mpl_recv for real arguments.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_recv_real (buf,      &
                              count,    &
                              datatype, &
                              source,   &
                              tag,      &
                              comm,     &
                              status,   &
                              error)
USE gc_kinds_mod, ONLY: &
  gc_int_kind,          &
  gc_real_kind

USE mpl, ONLY:     &
  mpl_status_size

! Subroutine arguments:
REAL(kind=gc_real_kind)   :: buf(:)
INTEGER(kind=gc_int_kind) :: count
INTEGER(kind=gc_int_kind) :: datatype
INTEGER(kind=gc_int_kind) :: source
INTEGER(kind=gc_int_kind) :: tag
INTEGER(kind=gc_int_kind) :: comm
INTEGER(kind=gc_int_kind) :: status(mpl_status_size)
INTEGER(kind=gc_int_kind) :: error

! Local declarations:
EXTERNAL mpl_recv

CALL mpl_recv (buf,      &
               count,    &
               datatype, &
               source,   &
               tag,      &
               comm,     &
               status,   &
               error)

END SUBROUTINE ops_mpl_recv_real
