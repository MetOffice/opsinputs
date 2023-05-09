!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for mpl_isend for real arguments.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_isend_real (buf,      &
                               count,    &
                               datatype, &
                               dest,     &
                               tag,      &
                               comm,     &
                               request,  &
                               error)
USE gc_kinds_mod, ONLY: &
  gc_int_kind,          &
  gc_real_kind

! Subroutine arguments:
REAL(kind=gc_real_kind)   :: buf(:)
INTEGER(kind=gc_int_kind) :: count
INTEGER(kind=gc_int_kind) :: datatype
INTEGER(kind=gc_int_kind) :: dest
INTEGER(kind=gc_int_kind) :: tag
INTEGER(kind=gc_int_kind) :: comm
INTEGER(kind=gc_int_kind) :: request
INTEGER(kind=gc_int_kind) :: error

! Local declarations:
EXTERNAL mpl_isend

CALL mpl_isend (buf,      &
                count,    &
                datatype, &
                dest,     &
                tag,      &
                comm,     &
                request,  &
                error)

END SUBROUTINE ops_mpl_isend_real
