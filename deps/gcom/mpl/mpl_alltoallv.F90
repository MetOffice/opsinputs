! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Alltoallv()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Communication from all to all processors
!     *  Sizes/offsets may vary.
!     *
!     *  Output:  error, recvbuf
!     *
!     ******************************************************************

SUBROUTINE MPL_Alltoallv(sendbuf, sendcnts, senddispls, sendtype,              &
                         recvbuf, recvcnts, recvdispls, recvtype,              &
                         comm,    ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  sendbuf(*),                                                                  &
  sendcnts(*),                                                                 &
  senddispls(*),                                                               &
  sendtype,                                                                    &
  recvbuf(*),                                                                  &
  recvcnts(*),                                                                 &
  recvdispls(*),                                                               &
  recvtype,                                                                    &
  comm,                                                                        &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_sendtype,                                                                  &
  l_recvtype,                                                                  &
  l_comm,                                                                      &
  l_error,                                                                     &
  ssize

INTEGER (KIND=mpl_int_kind), ALLOCATABLE ::                                    &
  l_sendcnts(:),                                                               &
  l_senddispls(:),                                                             &
  l_recvcnts(:),                                                               &
  l_recvdispls(:)

INTEGER (KIND=mpl_int_kind), SAVE ::                                           &
  ssize_save  = -1234,                                                         &
  l_comm_save = -1234

!=======================================================================

l_comm     = comm
l_recvtype = recvtype
l_sendtype = sendtype

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

ALLOCATE( l_sendcnts(ssize) )
ALLOCATE( l_senddispls(ssize) )
ALLOCATE( l_recvcnts(ssize) )
ALLOCATE( l_recvdispls(ssize) )

l_sendcnts(1:ssize)   = sendcnts(1:ssize)
l_senddispls(1:ssize) = senddispls(1:ssize)
l_recvcnts(1:ssize)   = recvcnts(1:ssize)
l_recvdispls(1:ssize) = recvdispls(1:ssize)

CALL MPI_Alltoallv(sendbuf, l_sendcnts, l_senddispls, l_sendtype,              &
                   recvbuf, l_recvcnts, l_recvdispls, l_recvtype,              &
                   l_comm,  l_error)

ERROR   = l_error
DEALLOCATE( l_sendcnts )
DEALLOCATE( l_senddispls )
DEALLOCATE( l_recvcnts )
DEALLOCATE( l_recvdispls )

RETURN
END SUBROUTINE MPL_Alltoallv
