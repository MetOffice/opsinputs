! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Group_Free()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Frees a group
!     *
!     *  Output:  error
!     *
!     ******************************************************************

SUBROUTINE MPL_Group_Free(group, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  group,                                                                       &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_group,                                                                     &
  l_error

!=======================================================================

l_group = group

CALL MPI_Group_Free(l_group, l_error)

group = l_group
ERROR = l_error

RETURN
END SUBROUTINE MPL_Group_Free
