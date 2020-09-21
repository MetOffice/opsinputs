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
!     *  Creates new communicators based on colors and keys.
!     *
!     *  Output: newcomm, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Comm_Split_type (comm, split_type, key,                         &
                                info, newcomm,    ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  comm,                                                                        &
  split_type,                                                                  &
  key,                                                                         &
  info,                                                                        &
  newcomm,                                                                     &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_comm,                                                                      &
  l_split_type,                                                                &
  l_key,                                                                       &
  l_info,                                                                      &
  l_newcomm,                                                                   &
  l_error

!=======================================================================

l_comm       = comm
l_split_type = split_type
l_key        = key
l_info       = info

CALL MPI_Comm_split_type(l_comm, l_split_type, l_key, l_info, l_newcomm, l_error)

newcomm    = l_newcomm
ERROR      = l_error

RETURN
END SUBROUTINE MPL_Comm_Split_type
