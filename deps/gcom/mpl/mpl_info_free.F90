! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Info_free()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Free info object
!     *
!     *  Output:  info, error
!     *
!     ******************************************************************

SUBROUTINE mpl_info_free( info, ERROR )

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind) ::                                                  &
  info,                                                                        &
  ERROR

INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_info,                                                                      &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_info = info

CALL MPI_Info_free(l_info, l_error)

info = l_info
ERROR = l_error
#endif

RETURN

END SUBROUTINE mpl_info_free
