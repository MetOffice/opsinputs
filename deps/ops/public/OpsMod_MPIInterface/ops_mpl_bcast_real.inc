!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for mpl_send for integer arguments.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_bcast_real (buf,      &
                               count,    &
                               datatype, &
                               root,     &
                               comm,     &
                               error)
USE mpl, ONLY: &
  gc_int_kind

! Subroutine arguments:
Real(kind=gc_int_kind)    :: buf(:)
INTEGER(kind=gc_int_kind) :: count
INTEGER(kind=gc_int_kind) :: datatype
INTEGER(kind=gc_int_kind) :: root
INTEGER(kind=gc_int_kind) :: comm
INTEGER(kind=gc_int_kind) :: error

! Local declarations:
EXTERNAL mpl_bcast

CALL mpl_bcast (buf,     &
               count,    &
               datatype, &
               root,     &
               comm,     &
               error)

END SUBROUTINE ops_mpl_bcast_real