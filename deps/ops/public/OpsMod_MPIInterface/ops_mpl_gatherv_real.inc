!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for mpl_gatherv for real arguments.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_gatherv_real (sendbuf,  &
                                 sendcnt,  &
                                 sendtype, &
                                 recvbuf,  &
                                 recvcnts, &
                                 displs,   &
                                 recvtype, &
                                 root,     &
                                 comm,     &
                                 error)
USE gc_kinds_mod, ONLY: &
  gc_int_kind,          &
  gc_real_kind

! Subroutine arguments:
REAL(kind=gc_real_kind)   :: sendbuf(:)
INTEGER(kind=gc_int_kind) :: sendcnt
INTEGER(kind=gc_int_kind) :: sendtype
REAL(kind=gc_real_kind)   :: recvbuf(:)
INTEGER(kind=gc_int_kind) :: recvcnts(:)
INTEGER(kind=gc_int_kind) :: displs(:)
INTEGER(kind=gc_int_kind) :: recvtype
INTEGER(kind=gc_int_kind) :: root
INTEGER(kind=gc_int_kind) :: comm
INTEGER(kind=gc_int_kind) :: error

! Local declarations:
EXTERNAL mpl_gatherv

CALL mpl_gatherv (sendbuf,  &
                  sendcnt,  &
                  sendtype, &
                  recvbuf,  &
                  recvcnts, &
                  displs,   &
                  recvtype, &
                  root,     &
                  comm,     &
                  error)

END SUBROUTINE ops_mpl_gatherv_real
