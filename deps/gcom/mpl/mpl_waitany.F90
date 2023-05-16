! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Waitany()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Wait until any one request has completed
!     *
!     *  Output:  request, stat, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Waitany (cnt, requests, request, STAT, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Status_Size

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  cnt,                                                                         &
  request,                                                                     &
  requests(cnt),                                                               &
  STAT(mpl_status_size),                                                       &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_count,                                                                     &
  l_request,                                                                   &
  l_requests(cnt),                                                             &
  l_status(mpl_status_size),                                                   &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_count       = cnt
l_requests(:) = requests(:)

CALL MPI_Waitany(l_count, l_requests, l_request, l_status, l_error)

STAT          = l_status
requests(:)   = l_requests(:)
request       = l_request
ERROR         = l_error
#endif

RETURN
END SUBROUTINE MPL_Waitany
