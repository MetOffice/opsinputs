! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Type_Free()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Free up MPL types
!     *
!     *  Output:  error
!     *
!     ******************************************************************

SUBROUTINE MPL_Type_Free (datatype, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  datatype,                                                                    &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_datatype,                                                                  &
  l_error

!=======================================================================

#if defined (MPI_SRC)
l_datatype = datatype

CALL MPI_Type_Free(l_datatype,l_error)

datatype = l_datatype
ERROR    = l_error
#endif

RETURN
END SUBROUTINE MPL_Type_Free
