! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Comm_Split()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Splits a communicator
!     *
!     *  Output:  newcomm, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Comm_Split (comm, color, key, newcomm, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  comm,                                                                        &
  color,                                                                       &
  key,                                                                         &
  newcomm,                                                                     &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_comm,                                                                      &
  l_color,                                                                     &
  l_key,                                                                       &
  l_newcomm,                                                                   &
  l_error

!=======================================================================

l_comm  = comm
l_color = color
l_key   = key

CALL MPI_Comm_Split(l_comm, l_color, l_key, l_newcomm, l_error)

newcomm = l_newcomm
ERROR   = l_error

RETURN
END SUBROUTINE MPL_Comm_Split
