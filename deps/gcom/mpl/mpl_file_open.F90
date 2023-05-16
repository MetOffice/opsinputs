! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_File_open()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Open a file with MPI-IO semantics
!     *
!     *  Output:  unt, error
!     *
!     ******************************************************************

SUBROUTINE mpl_file_open( comm, fn, mode, info, unt, ERROR )

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  comm,                                                                        &
  mode,                                                                        &
  info,                                                                        &
  unt,                                                                         &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
CHARACTER(LEN=*) :: fn

INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_comm,                                                                      &
  l_mode,                                                                      &
  l_info,                                                                      &
  l_unit,                                                                      &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_comm  = comm
l_mode  = mode
l_info  = info

CALL MPI_File_open(l_comm, fn, l_mode, l_info, l_unit, l_error)

unt   = l_unit
ERROR = l_error
#endif

RETURN

END SUBROUTINE mpl_file_open
