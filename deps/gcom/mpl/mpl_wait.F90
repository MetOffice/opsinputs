! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Wait()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Wait until request has completed
!     *
!     *  Output:  stat, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Wait (request, STAT, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Status_Size

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  request,                                                                     &
  STAT(mpl_status_size),                                                       &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_request,                                                                   &
  l_status(mpl_status_size),                                                   &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_request = request

CALL MPI_Wait( l_request, l_status, l_error)

STAT(:)   = l_status(:)
request   = l_request
ERROR     = l_error
#endif

RETURN
END SUBROUTINE MPL_Wait
