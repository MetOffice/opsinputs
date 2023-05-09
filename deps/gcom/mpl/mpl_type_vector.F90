! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Type_Vector()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Creates a vector (strided) datatype.
!     *
!     *  Output:  newtype, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Type_Vector (cnt, blocklength, stride, oldtype, newtype,        &
                            ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  cnt,                                                                         &
  blocklength,                                                                 &
  stride,                                                                      &
  oldtype,                                                                     &
  newtype,                                                                     &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_count,                                                                     &
  l_blocklength,                                                               &
  l_stride,                                                                    &
  l_oldtype,                                                                   &
  l_newtype,                                                                   &
  l_error

!=======================================================================

l_count       = cnt
l_blocklength = blocklength
l_stride      = stride
l_oldtype     = oldtype

CALL MPI_Type_Vector(l_count, l_blocklength, l_stride, l_oldtype,              &
                     l_newtype ,l_error)

newtype  = l_newtype
ERROR    = l_error

RETURN
END SUBROUTINE MPL_Type_Vector
