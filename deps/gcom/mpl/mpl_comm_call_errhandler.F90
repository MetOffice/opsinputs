! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Comm_Call_Errhandler()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Invokes the error handler currently attached to comm
!     *
!     *  Output: error (if error handler returns)
!     *
!     ******************************************************************

SUBROUTINE MPL_Comm_Call_Errhandler (comm, errorcode, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  comm,                                                                        &
  errorcode,                                                                   &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_comm,                                                                      &
  l_errorcode,                                                                 &
  l_error

!=======================================================================

l_comm      = comm
l_errorcode = errorcode

CALL MPI_Comm_Call_Errhandler(l_comm, l_errorcode, l_error)

ERROR      = l_error

RETURN
END SUBROUTINE MPL_Comm_Call_Errhandler
