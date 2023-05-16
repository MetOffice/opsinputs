! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Allgatherv()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  The block of data sent from the jth process is received by
!     *  every process and placed in the jth block of the receive buffer
!     *  Sizes/offsets may vary.
!     *
!     *  Output:  recvbuf, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Allgatherv(sendbuf, sendcnt,  sendtype,                         &
                          recvbuf, recvcnts, displs,   recvtype,               &
                          comm,    ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  sendbuf(*),                                                                  &
  recvbuf(*),                                                                  &
  recvcnts(*),                                                                 &
  displs(*),                                                                   &
  sendcnt,                                                                     &
  sendtype,                                                                    &
  recvtype,                                                                    &
  comm,                                                                        &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_sendcnt,                                                                   &
  l_sendtype,                                                                  &
  l_recvtype,                                                                  &
  l_comm,                                                                      &
  l_error,                                                                     &
  ssize

INTEGER (KIND=mpl_int_kind), ALLOCATABLE ::                                    &
  l_recvcnts(:),                                                               &
  l_displs(:)

INTEGER (KIND=mpl_int_kind), SAVE ::                                           &
  ssize_save  = -1234,                                                         &
  l_comm_save = -1234

!=======================================================================

l_comm     = comm
l_recvtype = recvtype
l_sendtype = sendtype
l_sendcnt  = sendcnt

! Unlike most of MPL, we need here to know the number of processors
! we are dealing with to allocate some buffers for copying.
! Fortran can't tell how big they are due to implicit sizing.
IF (l_comm == l_comm_save) THEN
  ! Can use saved size
  ssize = ssize_save
ELSE
  ! Need to know how many processors we are dealing with
  CALL MPI_Comm_Size(l_comm, ssize, l_error)
  ssize_save  = ssize
  l_comm_save = l_comm
END IF

ALLOCATE( l_recvcnts(ssize) )
ALLOCATE( l_displs(ssize) )

l_recvcnts(1:ssize) = recvcnts(1:ssize)
l_displs(1:ssize)   = displs(1:ssize)


CALL MPI_Allgatherv(sendbuf, l_sendcnt,  l_sendtype,                           &
                    recvbuf, l_recvcnts, l_displs, l_recvtype,                 &
                    l_comm,  l_error)

ERROR   = l_error
DEALLOCATE( l_recvcnts )
DEALLOCATE( l_displs )

RETURN
END SUBROUTINE MPL_Allgatherv
