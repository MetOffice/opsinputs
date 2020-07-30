! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Type_Struct()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Creates a struct data type
!     *  -- use of this routine is deprecated. Use MPI_Type_create_struct.
!     *
!     *  Output:  newtype, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Type_Struct (cnt, array_of_blocklengths,                        &
                            array_of_displacements, array_of_types,            &
                            newtype, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  cnt,                                                                         &
  array_of_blocklengths(cnt),                                                  &
  array_of_displacements(cnt),                                                 &
  array_of_types(cnt),                                                         &
  newtype,                                                                     &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_count,                                                                     &
  l_array_of_blocklengths(cnt),                                                &
  l_array_of_displacements(cnt),                                               &
  l_array_of_types(cnt),                                                       &
  l_newtype,                                                                   &
  l_error

!=======================================================================

l_count                     = cnt
l_array_of_blocklengths(:)  = array_of_blocklengths(:)
l_array_of_displacements(:) = array_of_displacements(:)
l_array_of_types(:)         = array_of_types(:)

CALL MPI_Type_Struct(l_count, l_array_of_blocklengths,                         &
                     l_array_of_displacements, l_array_of_types,               &
                     l_newtype, l_error)

newtype  = l_newtype
ERROR    = l_error

RETURN
END SUBROUTINE MPL_Type_Struct
