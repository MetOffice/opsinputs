!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for mpl_isend for 2d character arguments.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_isend_char2d (buf,      &
                                 count,    &
                                 datatype, &
                                 dest,     &
                                 tag,      &
                                 comm,     &
                                 request,  &
                                 error)
USE mpl, ONLY: &
  gc_int_kind

! Subroutine arguments:
CHARACTER(len=*)          :: buf(:,:)
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

END SUBROUTINE ops_mpl_isend_char2d