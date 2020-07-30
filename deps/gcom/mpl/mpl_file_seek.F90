! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_File_seek()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Move an MPI-IO file's local pointer
!     *
!     *  Output:  error
!     *
!     ******************************************************************

SUBROUTINE mpl_file_seek( unt, offset, whence, ERROR )

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, mpl_offset_kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  unt,                                                                         &
  whence,                                                                      &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_offset_kind) ::                                              &
  offset

INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_unit,                                                                      &
  l_whence,                                                                    &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_unit = unt
l_whence = whence

CALL MPI_File_seek( l_unit, offset, l_whence, l_error)

ERROR = l_error
#endif

RETURN
END SUBROUTINE mpl_file_seek
