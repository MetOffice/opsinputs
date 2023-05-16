!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for mpl_alltoallv for real arguments.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_alltoallv_real (sendbuf,    &
                                   sendcnts,   &
                                   senddispls, &
                                   sendtype,   &
                                   recvbuf,    &
                                   recvcnts,   &
                                   recvdispls, &
                                   recvtype,   &
                                   comm,       &
                                   error)
USE gc_kinds_mod, ONLY: &
  gc_int_kind,          &
  gc_real_kind

! Subroutine arguments:
REAL(kind=gc_real_kind)   :: sendbuf(:)
INTEGER(kind=gc_int_kind) :: sendcnts(:)
INTEGER(kind=gc_int_kind) :: senddispls(:)
INTEGER(kind=gc_int_kind) :: sendtype
REAL(kind=gc_real_kind)   :: recvbuf(:)
INTEGER(kind=gc_int_kind) :: recvcnts(:)
INTEGER(kind=gc_int_kind) :: recvdispls(:)
INTEGER(kind=gc_int_kind) :: recvtype
INTEGER(kind=gc_int_kind) :: comm
INTEGER(kind=gc_int_kind) :: error

! Local declarations:
EXTERNAL mpl_alltoallv

CALL mpl_alltoallv (sendbuf,    &
                    sendcnts,   &
                    senddispls, &
                    sendtype,   &
                    recvbuf,    &
                    recvcnts,   &
                    recvdispls, &
                    recvtype,   &
                    comm,       &
                    error)

END SUBROUTINE ops_mpl_alltoallv_real
