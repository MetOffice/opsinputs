! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Cart_Create()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Makes a new communicator to which Cartesian topology information
!     *  has been attached
!     *
!     *  Output:  comm_cart, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Cart_Create (comm_old, ndims, dims, periods, reorder,           &
                            comm_cart, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind, gc_log_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Log_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  comm_old,                                                                    &
  ndims,                                                                       &
  dims(ndims),                                                                 &
  comm_cart,                                                                   &
  ERROR

LOGICAL (KIND=gc_log_kind) ::                                                  &
  periods(ndims),                                                              &
  reorder

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_comm_old,                                                                  &
  l_ndims,                                                                     &
  l_dims(ndims),                                                               &
  l_comm_cart,                                                                 &
  l_error

LOGICAL (KIND=mpl_log_kind) ::                                                 &
  l_periods(ndims),                                                            &
  l_reorder

!=======================================================================

l_comm_old = comm_old
l_ndims = ndims
l_dims(:) = dims(:)
l_periods(:) = periods(:)
l_reorder = reorder

CALL MPI_Cart_create(l_comm_old, l_ndims, l_dims, l_periods, l_reorder,        &
                     l_comm_cart, l_error)

comm_cart = l_comm_cart
ERROR = l_error

RETURN
END SUBROUTINE MPL_Cart_Create
