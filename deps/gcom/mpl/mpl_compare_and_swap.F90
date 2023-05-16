! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Compare_And_Swap()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Compares one element in the compare buffer with the buffer in
!     *  the target and replaces the value at the target with the
!     *  value in the origin buffer if the compare buffer and the
!     *  target buffer are identical. The value at the target is
!     *  returned in the buffer result.
!     *
!     *  Output: result_addr, error
!     *
!     ******************************************************************

SUBROUTINE  MPL_Compare_And_Swap(origin_addr, compare_addr, result_addr,       &
                                 datatype, target_rank, target_disp,           &
                                 win, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Address_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  origin_addr(*),                                                              &
  compare_addr(*),                                                             &
  result_addr(*),                                                              &
  datatype,                                                                    &
  target_rank,                                                                 &
  win,                                                                         &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_datatype,                                                                  &
  l_target_rank,                                                               &
  l_win,                                                                       &
  l_error

INTEGER (KIND=mpl_address_kind) ::                                             &
  target_disp


!=======================================================================

l_datatype      = datatype
l_target_rank   = target_rank
l_win           = win

CALL MPI_Compare_And_Swap(origin_addr, compare_addr, result_addr,              &
                          l_datatype, l_target_rank, target_disp,              &
                          l_win, l_error)

ERROR = l_error

RETURN
END SUBROUTINE MPL_Compare_And_Swap
