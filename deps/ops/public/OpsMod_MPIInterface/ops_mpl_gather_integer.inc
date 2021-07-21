!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for mpl_gather for integer arguments.
!-------------------------------------------------------------------------------

SUBROUTINE ops_mpl_gather_integer (sendbuf,  &
                                   sendcnt,  &
                                   sendtype, &
                                   recvbuf,  &
                                   recvcnts, &
                                   recvtype, &
                                   root,     &
                                   comm,     &
                                   error)
USE mpl, ONLY: &
  gc_int_kind

! Subroutine arguments:
INTEGER(kind=gc_int_kind) :: sendbuf(:)
INTEGER(kind=gc_int_kind) :: sendcnt
INTEGER(kind=gc_int_kind) :: sendtype
INTEGER(kind=gc_int_kind) :: recvbuf(:)
INTEGER(kind=gc_int_kind) :: recvcnts
INTEGER(kind=gc_int_kind) :: recvtype
INTEGER(kind=gc_int_kind) :: root
INTEGER(kind=gc_int_kind) :: comm
INTEGER(kind=gc_int_kind) :: error

! Local declarations:
EXTERNAL mpl_gatherv

CALL mpl_gather (sendbuf,  &
                 sendcnt,  &
                 sendtype, &
                 recvbuf,  &
                 recvcnts, &
                 recvtype, &
                 root,     &
                 comm,     &
                 error)

END SUBROUTINE ops_mpl_gather_integer