! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Group_Translate_Ranks()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Translates ranks of processes in one group to another
!     *
!     *  Output:  ranks2, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Group_Translate_Ranks (group1, n, ranks1,                       &
                                      group2, ranks2, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  group1,                                                                      &
  n,                                                                           &
  ranks1(n),                                                                   &
  group2,                                                                      &
  ranks2(n),                                                                   &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_group1,                                                                    &
  l_n,                                                                         &
  l_ranks1(n),                                                                 &
  l_group2,                                                                    &
  l_ranks2(n),                                                                 &
  l_error

!=======================================================================

l_group1    = group1
l_n         = n
l_group2    = group2
l_ranks1(:) = ranks1(:)

CALL MPI_Group_Translate_Ranks(l_group1, l_n, l_ranks1,                        &
                               l_group2, l_ranks2, l_error)

ranks2(:) = l_ranks2(:)
ERROR     = l_error

RETURN
END SUBROUTINE MPL_Group_Translate_Ranks
