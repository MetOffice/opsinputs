! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Comm_Group()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Accesses group with given communicator
!     *
!     *  Output:  group, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Comm_Group (comm, group, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  comm,                                                                        &
  group,                                                                       &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_comm,                                                                      &
  l_group,                                                                     &
  l_error

!=======================================================================

l_comm = comm

CALL MPI_Comm_Group(l_comm,l_group,l_error)

group = l_group
ERROR = l_error

RETURN
END SUBROUTINE MPL_Comm_Group
