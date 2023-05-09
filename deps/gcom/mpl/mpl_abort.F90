! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Abort()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Terminates MPI execution environment. (Non-clean)
!     *
!     *  Output:  error
!     *
!     ******************************************************************

SUBROUTINE MPL_Abort (comm, code, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  comm,                                                                        &
  code,                                                                        &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_comm,                                                                      &
  l_code,                                                                      &
  l_error

!=======================================================================

l_comm = comm
l_code = code

CALL MPI_Abort(l_comm,l_code,l_error)

ERROR = l_error

RETURN
END SUBROUTINE MPL_Abort
