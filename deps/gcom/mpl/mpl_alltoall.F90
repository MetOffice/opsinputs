! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Alltoall()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Communication from all to all processors
!     *
!     *  Output:  error, recvbuf
!     *
!     ******************************************************************

SUBROUTINE MPL_Alltoall (sendbuf, sendcount, sendtype,                         &
                         recvbuf, recvcount, recvtype,                         &
                         comm, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  sendbuf(*),                                                                  &
  sendcount,                                                                   &
  sendtype,                                                                    &
  recvbuf(*),                                                                  &
  recvcount,                                                                   &
  recvtype,                                                                    &
  comm,                                                                        &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_sendcount,                                                                 &
  l_sendtype,                                                                  &
  l_recvcount,                                                                 &
  l_recvtype,                                                                  &
  l_comm,                                                                      &
  l_error

!=======================================================================

l_sendcount    = sendcount
l_sendtype     = sendtype
l_recvcount    = recvcount
l_recvtype     = recvtype
l_comm     = comm

CALL MPI_Alltoall(sendbuf, l_sendcount, l_sendtype,                            &
                  recvbuf, l_recvcount, l_recvtype,                            &
                  l_comm, l_error)

ERROR = l_error

RETURN
END SUBROUTINE MPL_Alltoall
