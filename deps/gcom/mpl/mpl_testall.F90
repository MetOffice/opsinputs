! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Testall()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Test requests have completed
!     *
!     *  Output:  error, flag, statuses
!     *
!     ******************************************************************

SUBROUTINE MPL_Testall (cnt, requests, flag, statuses, ERROR)

USE gc_kinds_mod, ONLY:                                                        &
    gc_int_kind,  gc_log_kind

USE mpl, ONLY:                                                                 &
    MPL_Int_Kind, MPL_Log_Kind,                                                &
    MPL_Status_Size

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  cnt,                                                                         &
  requests(cnt),                                                               &
  statuses(mpl_status_size,cnt),                                               &
  ERROR

LOGICAL (KIND=gc_log_kind) :: flag

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_count,                                                                     &
  l_requests(cnt),                                                             &
  l_statuses(mpl_status_size,cnt),                                             &
  l_error

LOGICAL (KIND=mpl_log_kind) :: l_flag

!=======================================================================

#if defined(MPI_SRC)
l_count       = cnt
l_requests(:) = requests(:)

CALL MPI_Testall(l_count, l_requests, l_flag, l_statuses, l_error)

flag          = l_flag
statuses(:,:) = l_statuses(:,:)
requests(:)   = l_requests(:)
ERROR         = l_error
#endif

RETURN
END SUBROUTINE MPL_Testall
