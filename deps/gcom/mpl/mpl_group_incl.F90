! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Group_Incl()
!
!     ******************************************************************
!     * Purpose:
!     *  Produces a group by reordering an existing group and taking
!     *  only listed members
!     *
!     *  Output:  newgroup, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Group_Incl (group, n, irank, newgroup, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  group,                                                                       &
  n,                                                                           &
  irank(n),                                                                    &
  newgroup,                                                                    &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_group,                                                                     &
  l_n,                                                                         &
  l_irank(n),                                                                  &
  l_newgroup,                                                                  &
  l_error

!=======================================================================

l_group = group
l_n     = n
l_irank(:) = irank(:)

CALL MPI_Group_Incl(l_group, l_n, l_irank, l_newgroup, l_error)

newgroup = l_newgroup
ERROR    = l_error

RETURN
END SUBROUTINE MPL_Group_Incl
