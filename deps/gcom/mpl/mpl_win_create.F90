! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Win_Create()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Create an MPI Window object for one-sided communication
!     *
!     *  Output:  win, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Win_Create (base, isize, disp_unit, info, comm,                 &
                           win, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Address_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  base(*),                                                                     &
  disp_unit,                                                                   &
  info,                                                                        &
  comm,                                                                        &
  win,                                                                         &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_disp_unit,                                                                 &
  l_info,                                                                      &
  l_comm,                                                                      &
  l_win,                                                                       &
  l_error

INTEGER (KIND=mpl_address_kind) ::                                             &
  isize

!=======================================================================

#if defined(MPI_SRC)
l_comm      = comm
l_disp_unit = disp_unit
l_info      = info

CALL MPI_Win_Create(base, isize, l_disp_unit, l_info, l_comm,                  &
                    l_win, l_error)

win   = l_win
ERROR = l_error
#endif

RETURN
END SUBROUTINE MPL_Win_Create
