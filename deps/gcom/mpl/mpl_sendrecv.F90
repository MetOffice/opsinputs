! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Sendrecv()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Send and recv in one call
!     *
!     *  Output:  recvbuf, stat, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Sendrecv (sendbuf, sendcount, sendtype, dest,   sendtag,        &
                         recvbuf, recvcount, recvtype, SOURCE, recvtag,        &
                         comm, STAT, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Status_Size

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  sendbuf(*),                                                                  &
  sendcount,                                                                   &
  sendtype,                                                                    &
  dest,                                                                        &
  sendtag,                                                                     &
  recvbuf(*),                                                                  &
  recvcount,                                                                   &
  recvtype,                                                                    &
  SOURCE,                                                                      &
  recvtag,                                                                     &
  comm,                                                                        &
  STAT(mpl_status_size),                                                       &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_sendcount,                                                                 &
  l_sendtype,                                                                  &
  l_dest,                                                                      &
  l_sendtag,                                                                   &
  l_recvcount,                                                                 &
  l_recvtype,                                                                  &
  l_source,                                                                    &
  l_recvtag,                                                                   &
  l_comm,                                                                      &
  l_status(mpl_status_size),                                                   &
  l_error

!=======================================================================

l_sendcount    = sendcount
l_sendtype     = sendtype
l_dest         = dest
l_sendtag      = sendtag
l_recvcount    = recvcount
l_recvtype     = recvtype
l_source       = SOURCE
l_recvtag      = recvtag
l_comm         = comm

CALL MPI_Sendrecv(sendbuf, l_sendcount, l_sendtype, l_dest,   l_sendtag,       &
                  recvbuf, l_recvcount, l_recvtype, l_source, l_recvtag,       &
                  l_comm, l_status, l_error)

STAT(:)   = l_status(:)
ERROR     = l_error

RETURN
END SUBROUTINE MPL_Sendrecv
