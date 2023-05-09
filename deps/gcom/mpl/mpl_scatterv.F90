! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Scatterv()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Scatter a vector from 1 processor to many
!     *  Sizes/offsets may vary.
!     *
!     *  Output:  error, recvbuf
!     *
!     ******************************************************************

SUBROUTINE MPL_Scatterv(sendbuf, sendcnts, displs, sendtype,                   &
                        recvbuf, recvcnt,  recvtype, root, comm, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  sendbuf(*),                                                                  &
  recvbuf(*),                                                                  &
  sendcnts(*),                                                                 &
  displs(*),                                                                   &
  recvcnt,                                                                     &
  sendtype,                                                                    &
  recvtype,                                                                    &
  root,                                                                        &
  comm,                                                                        &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_recvcnt,                                                                   &
  l_sendtype,                                                                  &
  l_recvtype,                                                                  &
  l_root,                                                                      &
  l_comm,                                                                      &
  l_error,                                                                     &
  ssize

INTEGER (KIND=mpl_int_kind), ALLOCATABLE ::                                    &
  l_sendcnts(:),                                                               &
  l_displs(:)

INTEGER (KIND=mpl_int_kind), SAVE ::                                           &
  ssize_save  = -1234,                                                         &
  l_comm_save = -1234

!=======================================================================

l_root     = root
l_comm     = comm
l_recvtype = recvtype
l_sendtype = sendtype
l_recvcnt  = recvcnt

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
ALLOCATE( l_displs(ssize) )

l_sendcnts(1:ssize) = sendcnts(1:ssize)
l_displs(1:ssize)   = displs(1:ssize)


CALL MPI_Scatterv(sendbuf, l_sendcnts, l_displs, l_sendtype,                   &
                  recvbuf, l_recvcnt,  l_recvtype,                             &
                  l_root,  l_comm,     l_error)

ERROR   = l_error
DEALLOCATE( l_sendcnts )
DEALLOCATE( l_displs )

RETURN
END SUBROUTINE MPL_Scatterv
