! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Init ()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Initialise MPI
!     *
!     *  Output:  error
!     *
!     ******************************************************************

SUBROUTINE MPL_Init (ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  ERROR                         ! Error code to return

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_error                       ! Error code returned by MPI_INIT

!=======================================================================

CALL MPI_Init(l_error)

ERROR = l_error

RETURN
END SUBROUTINE MPL_Init
