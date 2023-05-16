! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Comm_Set_Errhandler()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Attach a new error handler to a communicator
!     *
!     *  Output: errhandler, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Comm_Set_Errhandler (comm, errhandler, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  comm,                                                                        &
  errhandler,                                                                  &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_comm,                                                                      &
  l_errhandler,                                                                &
  l_error

!=======================================================================

l_comm       = comm
l_errhandler = errhandler

CALL MPI_Comm_Set_Errhandler(l_comm, l_errhandler, l_error)

ERROR        = l_error

RETURN
END SUBROUTINE MPL_Comm_Set_Errhandler
