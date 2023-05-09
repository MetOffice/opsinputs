! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Ibarrier()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Immediate barrier, signal when all the CPUs in communicator
!     *  reach barrier.
!     *
!     *  Output:  error, request
!     *
!     ******************************************************************

SUBROUTINE MPL_Ibarrier (comm, request, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  comm,                                                                        &
  request,                                                                     &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_comm,                                                                      &
  l_request,                                                                   &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_comm = comm

CALL MPI_Ibarrier(l_comm, l_request, l_error)

request = l_request
ERROR   = l_error
#endif

RETURN
END SUBROUTINE MPL_Ibarrier
