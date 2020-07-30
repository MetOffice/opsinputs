!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for mpl_gatherv for character arguments.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_gatherv_char (sendbuf,  &
                                 sendcnt,  &
                                 sendtype, &
                                 recvbuf,  &
                                 recvcnts, &
                                 displs,   &
                                 recvtype, &
                                 root,     &
                                 comm,     &
                                 error)
USE mpl, ONLY: &
  gc_int_kind

! Subroutine arguments:
CHARACTER(len=*)          :: sendbuf(:)
INTEGER(kind=gc_int_kind) :: sendcnt
INTEGER(kind=gc_int_kind) :: sendtype
CHARACTER(len=*)          :: recvbuf(:)
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

END SUBROUTINE ops_mpl_gatherv_char
