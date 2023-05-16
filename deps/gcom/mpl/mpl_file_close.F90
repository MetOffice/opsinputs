! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_File_close()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Close MPI-IO file
!     *
!     *  Output:  error
!     *
!     ******************************************************************

SUBROUTINE mpl_file_close( unt, ERROR )

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  unt,                                                                         &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_unit,                                                                      &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_unit = unt

CALL MPI_File_close(l_unit, l_error)

ERROR = l_error
unt = l_unit
#endif

RETURN

END SUBROUTINE mpl_file_close
