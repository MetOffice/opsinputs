! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Type_Extent()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *   Returns the extent of a data type, the difference between the
!     *   upper and lower bounds of the data type
!     *   -- use of this routine is deprecated.  Use MPI_Type_get_extent
!     *
!     *  Output:  extent, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Type_Extent (datatype, extent, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  datatype,                                                                    &
  extent,                                                                      &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_datatype,                                                                  &
  l_extent,                                                                    &
  l_error

!=======================================================================

l_datatype    = datatype

CALL MPI_Type_Extent(l_datatype, l_extent, l_error)

extent   = l_extent
ERROR    = l_error

RETURN
END SUBROUTINE MPL_Type_Extent
