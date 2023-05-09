! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Comm_Size ()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Return the size of the comunicating group.
!     *
!     *  Output: sze, ierr
!     *
!     ******************************************************************

SUBROUTINE MPL_Comm_Size (comm, sze, ierr)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  comm,                                                                        &
  sze,                                                                         &
  ierr

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind)  ::                                                &
  l_comm,                                                                      &
  l_size,                                                                      &
  l_ierr

!=======================================================================

!Cast Model precision input arguments to MPI library precision.
l_comm = comm

!Call MPI routine
CALL MPI_Comm_Size(l_comm, l_size, l_ierr)

!Cast returned arguments to Model Preciison
ierr = l_ierr
sze  = l_size

RETURN
END SUBROUTINE MPL_Comm_Size
