! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_File_sync()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Sync a file to disk with MPI-IO semantics
!     *
!     *  Output:  error
!     *
!     ******************************************************************

SUBROUTINE mpl_file_sync( unt, ERROR )

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  unt,                                                                         &
  ERROR

INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_unit,                                                                      &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_unit = unt

CALL MPI_File_sync( l_unit, l_error )

ERROR = l_error
#endif

RETURN

END SUBROUTINE mpl_file_sync
