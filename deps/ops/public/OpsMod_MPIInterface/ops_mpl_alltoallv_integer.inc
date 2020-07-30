!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for mpl_alltoallv for integer arguments.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_alltoallv_integer (sendbuf,    &
                                      sendcnts,   &
                                      senddispls, &
                                      sendtype,   &
                                      recvbuf,    &
                                      recvcnts,   &
                                      recvdispls, &
                                      recvtype,   &
                                      comm,       &
                                      error)
USE mpl, ONLY: &
  gc_int_kind

! Subroutine arguments:
INTEGER(kind=gc_int_kind) :: sendbuf(:)
INTEGER(kind=gc_int_kind) :: sendcnts(:)
INTEGER(kind=gc_int_kind) :: senddispls(:)
INTEGER(kind=gc_int_kind) :: sendtype
INTEGER(kind=gc_int_kind) :: recvbuf(:)
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

END SUBROUTINE ops_mpl_alltoallv_integer
