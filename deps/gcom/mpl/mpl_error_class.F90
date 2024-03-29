! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Error_Class()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Map an MPI error code to an error class
!     *
!     *  Output: errorclass, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Error_Class (errorcode, errorclass, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  errorcode,                                                                   &
  errorclass,                                                                  &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_errorcode,                                                                 &
  l_errorclass,                                                                &
  l_error

!=======================================================================

l_errorcode  = errorcode

CALL MPI_Error_Class(l_errorcode, l_errorclass, l_error)

errorclass   = l_errorclass
ERROR        = l_error

RETURN
END SUBROUTINE MPL_Error_Class
