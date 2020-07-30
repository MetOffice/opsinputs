! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Comm_Get_Errhandler()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Gets the error handler currently attached to comm
!     *
!     *  Output: errhandler, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Comm_Get_Errhandler (comm, errhandler, ERROR)

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

l_comm     = comm

CALL MPI_Comm_Get_Errhandler(l_comm, l_errhandler, l_error)

errhandler = l_errhandler
ERROR      = l_error

RETURN
END SUBROUTINE MPL_Comm_Get_Errhandler
