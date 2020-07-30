! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Type_Create_Resized()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Returns a new data type with new extent and upper and lower bounds
!     *
!     *  Output:  newtype, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Type_Create_Resized (oldtype, lb, extent,                       &
                                    newtype, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Address_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  oldtype,                                                                     &
  lb,                                                                          &
  extent,                                                                      &
  newtype,                                                                     &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_oldtype,                                                                   &
  l_newtype,                                                                   &
  l_error

INTEGER (KIND=mpl_address_kind) ::                                             &
  l_lb,                                                                        &
  l_extent

!=======================================================================

l_oldtype    = oldtype
l_lb         = lb
l_extent     = extent

CALL MPI_Type_Create_Resized(l_oldtype, l_lb, l_extent,                        &
                             l_newtype, l_error)

newtype      = l_newtype
ERROR        = l_error

RETURN
END SUBROUTINE MPL_Type_Create_Resized
