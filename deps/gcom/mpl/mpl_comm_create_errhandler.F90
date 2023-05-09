! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Comm_Create_Errhandler()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Create an MPI Error Handler
!     *
!     *  Output:  errhandler, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Comm_Create_Errhandler (func, errhandler, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Int_Kind

IMPLICIT NONE

EXTERNAL :: func

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  errhandler,                                                                  &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_errhandler,                                                                &
  l_error

!=======================================================================

CALL MPI_Comm_Create_Errhandler(func, l_errhandler, l_error)

errhandler = l_errhandler
ERROR      = l_error

RETURN
END SUBROUTINE MPL_Comm_Create_Errhandler
