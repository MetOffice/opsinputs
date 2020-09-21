!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for mpl_scatterv for integer arguments.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_scatterv_integer (sendbuf,  &
                                     sendcnts, &
                                     displs,   &
                                     sendtype, &
                                     recvbuf,  &
                                     recvcnt,  &
                                     recvtype, &
                                     root,     &
                                     comm,     &
                                     error)
USE mpl, ONLY: &
  gc_int_kind

! Subroutine arguments:
INTEGER(kind=gc_int_kind) :: sendbuf(:)
INTEGER(kind=gc_int_kind) :: sendcnts(:)
INTEGER(kind=gc_int_kind) :: displs(:)
INTEGER(kind=gc_int_kind) :: sendtype
INTEGER(kind=gc_int_kind) :: recvbuf(:)
INTEGER(kind=gc_int_kind) :: recvcnt
INTEGER(kind=gc_int_kind) :: recvtype
INTEGER(kind=gc_int_kind) :: root
INTEGER(kind=gc_int_kind) :: comm
INTEGER(kind=gc_int_kind) :: error

! Local declarations:
EXTERNAL mpl_scatterv

CALL mpl_scatterv (sendbuf,  &
                   sendcnts, &
                   displs,   &
                   sendtype, &
                   recvbuf,  &
                   recvcnt,  &
                   recvtype, &
                   root,     &
                   comm,     &
                   error)

END SUBROUTINE ops_mpl_scatterv_integer
