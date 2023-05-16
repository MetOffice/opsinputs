! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Barrier()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Wait for all CPUs in communicator
!     *
!     *  Output:  error
!     *
!     ******************************************************************

SUBROUTINE MPL_Barrier (comm, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  comm,                                                                        &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_comm,                                                                      &
  l_error

!=======================================================================

l_comm = comm

CALL MPI_Barrier(l_comm, l_error)

ERROR = l_error

RETURN
END SUBROUTINE MPL_Barrier
