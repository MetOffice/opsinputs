! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_File_get_size()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Recover an MPI-IO file's size
!     *
!     *  Output:  sze, error
!     *
!     ******************************************************************

SUBROUTINE mpl_file_get_size( unt, sze, ERROR )

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, mpl_offset_kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  unt,                                                                         &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_offset_kind) ::                                              &
  sze

INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_unit,                                                                      &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_unit = unt

CALL MPI_File_get_size(l_unit,sze,l_error)

ERROR = l_error
#endif


RETURN
END SUBROUTINE mpl_file_get_size
