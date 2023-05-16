! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Win_Allocate()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Create an MPI Window object for one-sided communication and
!     *  allocate memory at each process.
!     *
!     *  Output: baseptr, win, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Win_Allocate ( win_size, disp_unit, info,                       &
                              comm,     baseptr,   win,                        &
                              ERROR )

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, mpl_address_kind

USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  disp_unit,                                                                   &
  info,                                                                        &
  comm,                                                                        &
  win,                                                                         &
  ERROR

TYPE(C_PTR) :: baseptr

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_disp_unit,                                                                 &
  l_info,                                                                      &
  l_comm,                                                                      &
  l_win,                                                                       &
  l_error

INTEGER(KIND=mpl_address_kind) ::                                              &
  win_size

!=======================================================================

#if defined(MPI_SRC)
l_disp_unit = disp_unit
l_info      = info
l_comm      = comm

CALL MPI_Win_Allocate( win_size,   l_disp_unit, l_info, l_comm,                &
                       baseptr,    l_win,       l_error )

win       = l_win
ERROR     = l_error
#endif

RETURN
END SUBROUTINE MPL_Win_Allocate
