! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Info_create()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Create info object
!     *
!     *  Output:  info, error
!     *
!     ******************************************************************

SUBROUTINE mpl_info_create( info, ERROR )

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  info,                                                                        &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_info,                                                                      &
  l_error

!=======================================================================

#if defined(MPI_SRC)
CALL MPI_Info_create(l_info, l_error)

ERROR = l_error
info  = l_info
#endif

RETURN

END SUBROUTINE mpl_info_create
