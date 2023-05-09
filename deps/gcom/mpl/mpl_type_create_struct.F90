! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Type_Create_Struct()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Creates a structured data type
!     *
!     *  Output:  newtype, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Type_Create_Struct (icount, array_of_blocklengths,              &
                            array_of_displacements, array_of_types,            &
                            newtype, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Address_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  icount,                                                                      &
  array_of_blocklengths(icount),                                               &
  array_of_types(icount),                                                      &
  newtype,                                                                     &
  ERROR

INTEGER (KIND=mpl_address_kind) ::                                             &
  array_of_displacements(icount)

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_count,                                                                     &
  l_array_of_blocklengths(icount),                                             &
  l_array_of_types(icount),                                                    &
  l_newtype,                                                                   &
  l_error

!=======================================================================

#if defined (MPI_SRC)
l_count                     = icount
l_array_of_blocklengths(:)  = array_of_blocklengths(:)
l_array_of_types(:)         = array_of_types(:)

CALL MPI_Type_Create_Struct(l_count, l_array_of_blocklengths,                  &
                     array_of_displacements, l_array_of_types,                 &
                     l_newtype, l_error)

newtype  = l_newtype
ERROR    = l_error
#endif

RETURN
END SUBROUTINE MPL_Type_Create_Struct
