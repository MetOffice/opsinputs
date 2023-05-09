! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Error_String()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Returns the error string associated with an error code or class.
!     *
!     *  Output: errorstring, resultlen, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Error_String (errorcode, errorstring, resultlen, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  errorcode,                                                                   &
  resultlen,                                                                   &
  ERROR

CHARACTER(LEN=*) :: errorstring

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_errorcode,                                                                 &
  l_resultlen,                                                                 &
  l_error

!=======================================================================

l_errorcode  = errorcode

CALL MPI_Error_String(l_errorcode, errorstring, l_resultlen, l_error)

resultlen    = l_resultlen
ERROR        = l_error

RETURN
END SUBROUTINE MPL_Error_String
