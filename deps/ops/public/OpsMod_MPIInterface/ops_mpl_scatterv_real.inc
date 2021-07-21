!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for mpl_scatterv for real arguments.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_scatterv_real (sendbuf,  &
                                  sendcnts, &
                                  displs,   &
                                  sendtype, &
                                  recvbuf,  &
                                  recvcnt,  &
                                  recvtype, &
                                  root,     &
                                  comm,     &
                                  error)
USE gc_kinds_mod, ONLY: &
  gc_int_kind,          &
  gc_real_kind

! Subroutine arguments:
REAL(kind=gc_real_kind)   :: sendbuf(:)
INTEGER(kind=gc_int_kind) :: sendcnts(:)
INTEGER(kind=gc_int_kind) :: displs(:)
INTEGER(kind=gc_int_kind) :: sendtype
REAL(kind=gc_real_kind)   :: recvbuf(:)
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

END SUBROUTINE ops_mpl_scatterv_real