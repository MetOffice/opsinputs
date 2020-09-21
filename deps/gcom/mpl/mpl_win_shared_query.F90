! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Win_Shared_Query()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Query the size and base pointer for a patch of a shared memory
!     *  window.
!     *
!     *  Output: segment_size, disp_unit, baseptr
!     *
!     ******************************************************************

SUBROUTINE MPL_Win_Shared_Query ( win,       RANK,    segment_size,            &
                                  disp_unit, baseptr, ERROR )

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, mpl_address_kind

USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  win,                                                                         &
  RANK,                                                                        &
  segment_size,                                                                &
  disp_unit,                                                                   &
  ERROR

TYPE(C_PTR) :: baseptr

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_win,                                                                       &
  l_rank,                                                                      &
  l_disp_unit,                                                                 &
  l_error

INTEGER(KIND=mpl_address_kind) ::                                              &
  l_segment_size

!=======================================================================

#if defined(MPI_SRC)
l_win  = win
l_rank = RANK

CALL MPI_Win_Shared_Query( l_win, l_rank, l_segment_size, l_disp_unit,         &
                           baseptr, l_error )

segment_size = l_segment_size
disp_unit    = l_disp_unit
ERROR        = l_error
#endif

RETURN
END SUBROUTINE MPL_Win_Shared_Query
