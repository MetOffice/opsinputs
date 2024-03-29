!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for mpl_scatterv for 2d character arguments.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_scatterv_char2d (sendbuf,  &
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
CHARACTER(len=*)          :: sendbuf(:,:)
INTEGER(kind=gc_int_kind) :: sendcnts(:)
INTEGER(kind=gc_int_kind) :: displs(:)
INTEGER(kind=gc_int_kind) :: sendtype
CHARACTER(len=*)          :: recvbuf(:,:)
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

END SUBROUTINE ops_mpl_scatterv_char2d
