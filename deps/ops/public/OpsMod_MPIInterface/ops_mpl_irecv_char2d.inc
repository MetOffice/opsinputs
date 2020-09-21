!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for mpl_irecv for 2d character arguments.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_irecv_char2d (buf,      &
                                 count,    &
                                 datatype, &
                                 source,   &
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
INTEGER(kind=gc_int_kind) :: source
INTEGER(kind=gc_int_kind) :: tag
INTEGER(kind=gc_int_kind) :: comm
INTEGER(kind=gc_int_kind) :: request
INTEGER(kind=gc_int_kind) :: error

! Local declarations:
EXTERNAL mpl_irecv

CALL mpl_irecv (buf,      &
                count,    &
                datatype, &
                source,   &
                tag,      &
                comm,     &
                request,  &
                error)

END SUBROUTINE ops_mpl_irecv_char2d
