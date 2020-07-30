!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for mpl_recv for integer arguments.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_recv_integer (buf,      &
                                 count,    &
                                 datatype, &
                                 source,   &
                                 tag,      &
                                 comm,     &
                                 status,   &
                                 error)
USE mpl, ONLY:     &
  gc_int_kind,     &
  mpl_status_size

! Subroutine arguments:
INTEGER(kind=gc_int_kind) :: buf(:)
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

END SUBROUTINE ops_mpl_recv_integer
