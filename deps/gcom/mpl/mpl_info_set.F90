! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Info_set()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Adds a value/key pair to an info object
!     *
!     *  Output:  error
!     *
!     ******************************************************************

SUBROUTINE mpl_info_set( info, key, val, ERROR )

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  info,                                                                        &
  ERROR

CHARACTER(LEN=*) :: key, val

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_info,                                                                      &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_info  = info

CALL MPI_Info_set(l_info, key, val, l_error)

ERROR = l_error
#endif

RETURN

END SUBROUTINE mpl_info_set
