! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Finalize ()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Finalize MPI
!     *
!     *  Output:  error
!     *
!     ******************************************************************

SUBROUTINE MPL_Finalize (ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  ERROR                         ! Error code to return

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_error                       ! Error code returned by MPI_Finalize

!=======================================================================

CALL MPI_Finalize(l_error)

ERROR = l_error

RETURN
END SUBROUTINE MPL_Finalize
