! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Comm_Rank ()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Return the rank (process number) of the calling processor.
!     *
!     *  Output: rank, ierr
!     *
!     ******************************************************************

SUBROUTINE MPL_Comm_Rank (comm, RANK, ierr)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  comm,                                                                        &
  RANK,                                                                        &
  ierr

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind)  ::                                                &
  l_comm,                                                                      &
  l_rank,                                                                      &
  l_ierr

!=======================================================================

!Cast Model precision input arguments to MPI library precision.
l_comm = comm

!Call MPI routine
CALL MPI_Comm_Rank(l_comm, l_rank, l_ierr)

!Cast returned arguments to Model Preciison
ierr = l_ierr
RANK = l_rank

RETURN
END SUBROUTINE MPL_Comm_Rank
