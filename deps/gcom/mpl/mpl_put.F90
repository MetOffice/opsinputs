! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Put()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Puts data into a remote processor
!     *
!     *  Output:  error
!     *
!     ******************************************************************

SUBROUTINE MPL_Put (origin_addr, origin_count, origin_datatype,                &
                    target_rank, target_disp, target_count, target_datatype,   &
                    win, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Address_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  origin_addr(*),                                                              &
  origin_count,                                                                &
  origin_datatype,                                                             &
  target_rank,                                                                 &
  target_count,                                                                &
  target_datatype,                                                             &
  win,                                                                         &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_origin_count,                                                              &
  l_origin_datatype,                                                           &
  l_target_rank,                                                               &
  l_target_count,                                                              &
  l_target_datatype,                                                           &
  l_win,                                                                       &
  l_error

INTEGER (KIND=mpl_address_kind) ::                                             &
  target_disp

!=======================================================================

#if defined(MPI_SRC)
l_origin_count    = origin_count
l_origin_datatype = origin_datatype
l_target_rank     = target_rank
l_target_count    = target_count
l_target_datatype = target_datatype
l_win             = win

CALL MPI_Put( origin_addr, l_origin_count, l_origin_datatype,                  &
              l_target_rank, target_disp, l_target_count, l_target_datatype,   &
              l_win, l_error)

ERROR = l_error
#endif

RETURN
END SUBROUTINE MPL_Put
