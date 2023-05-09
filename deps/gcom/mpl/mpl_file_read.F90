! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_File_read()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Read from MPI-IO file
!     *
!     *  Output:  buffer, stat, error
!     *
!     ******************************************************************

SUBROUTINE mpl_file_read( unt, buffer, cnt, datatype, STAT, ERROR )

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, mpl_status_size

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  unt,                                                                         &
  buffer(*),                                                                   &
  cnt,                                                                         &
  datatype,                                                                    &
  STAT(mpl_status_size),                                                       &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_unit,                                                                      &
  l_count,                                                                     &
  l_datatype,                                                                  &
  l_status(mpl_status_size),                                                   &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_unit     = unt
l_count    = cnt
l_datatype = datatype

CALL MPI_File_read(l_unit,buffer,l_count,l_datatype,l_status,l_error)

STAT = l_status
ERROR  = l_error
#endif

RETURN
END SUBROUTINE mpl_file_read
