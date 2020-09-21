! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Fetch_And_Op()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Combines the contents of the origin buffer with that of a
!     *  target buffer and returns the target buffer value.
!     *
!     *  Output: result_addr, error
!     *
!     ******************************************************************

SUBROUTINE  MPL_Fetch_And_Op(origin_addr, result_addr, datatype, target_rank,  &
                             target_disp, op, win, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Address_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  origin_addr(*),                                                              &
  result_addr(*),                                                              &
  datatype,                                                                    &
  target_rank,                                                                 &
  op,                                                                          &
  win,                                                                         &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_datatype,                                                                  &
  l_target_rank,                                                               &
  l_op,                                                                        &
  l_win,                                                                       &
  l_error

INTEGER (KIND=mpl_address_kind) ::                                             &
  target_disp


!=======================================================================

#if defined(MPI_SRC)
l_datatype      = datatype
l_target_rank   = target_rank
l_op            = op
l_win           = win

CALL MPI_Fetch_And_Op(origin_addr, result_addr, l_datatype, l_target_rank,     &
                      target_disp, l_op, l_win, l_error)

ERROR = l_error
#endif

RETURN
END SUBROUTINE MPL_Fetch_And_Op
