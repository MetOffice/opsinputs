! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Waitall()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Wait until all requests have completed
!     *
!     *  Output:  statuses, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Waitall (cnt, requests, statuses, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Status_Size

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  cnt,                                                                         &
  requests(cnt),                                                               &
  statuses(mpl_status_size,cnt),                                               &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_count,                                                                     &
  l_requests(cnt),                                                             &
  l_statuses(mpl_status_size,cnt),                                             &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_count       = cnt
l_requests(:) = requests(:)

CALL MPI_Waitall(l_count, l_requests, l_statuses, l_error)

statuses(:,:) = l_statuses(:,:)
requests(:)   = l_requests(:)
ERROR         = l_error
#endif

RETURN
END SUBROUTINE MPL_Waitall
