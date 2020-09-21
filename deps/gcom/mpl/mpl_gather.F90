! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Gather()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Gather from many processors to 1
!     *
!     *  Output:  error, recvbuf
!     *
!     ******************************************************************

SUBROUTINE MPL_Gather(sendbuf, sendcnt, sendtype,                              &
                      recvbuf, recvcnt, recvtype,                              &
                      root,    comm,    ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  sendbuf(*),                                                                  &
  recvbuf(*),                                                                  &
  recvcnt,                                                                     &
  sendcnt,                                                                     &
  sendtype,                                                                    &
  recvtype,                                                                    &
  root,                                                                        &
  comm,                                                                        &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_sendcnt,                                                                   &
  l_sendtype,                                                                  &
  l_recvtype,                                                                  &
  l_recvcnt,                                                                   &
  l_root,                                                                      &
  l_comm,                                                                      &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_root     = root
l_comm     = comm
l_recvtype = recvtype
l_sendtype = sendtype
l_sendcnt  = sendcnt
l_recvcnt = recvcnt

CALL MPI_Gather(sendbuf, l_sendcnt, l_sendtype,                                &
                recvbuf, l_recvcnt, l_recvtype,                                &
                l_root,  l_comm,    l_error)

ERROR   = l_error
#endif

RETURN
END SUBROUTINE MPL_Gather
