! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_File_set_errhandler()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Set errorhandler for MPI-IO file
!     *
!     *  Output:  handler, error
!     *
!     ******************************************************************

SUBROUTINE mpl_file_set_errhandler( unt, handler, ERROR )

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  handler,                                                                     &
  unt,                                                                         &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_handler,                                                                   &
  l_unit,                                                                      &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_unit     = unt
l_handler  = handler

CALL MPI_File_set_errhandler( l_unit, l_handler, l_error )

ERROR = l_error
#endif

RETURN

END SUBROUTINE mpl_file_set_errhandler
