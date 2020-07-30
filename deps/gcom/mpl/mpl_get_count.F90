! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Get_count()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Recover size of MPI-IO read/write from returned status object
!     *
!     *  Output:  cnt, error
!     *
!     ******************************************************************


SUBROUTINE MPL_Get_Count (STAT, datatype, cnt, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, mpl_status_size

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  STAT(mpl_status_size),                                                       &
  cnt,                                                                         &
  datatype,                                                                    &
  ERROR


! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_status(mpl_status_size),                                                   &
  l_count,                                                                     &
  l_datatype,                                                                  &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_status(:) = STAT(:)
l_datatype  = datatype

CALL MPI_Get_count(l_status, l_datatype, l_count, l_error)

cnt     = l_count
ERROR   = l_error
#endif

RETURN
END SUBROUTINE MPL_Get_Count
