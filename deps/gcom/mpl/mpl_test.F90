! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Test()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Test requests have completed
!     *
!     *  Output:  flag, stat, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Test (request, flag, STAT, ERROR)

USE gc_kinds_mod, ONLY:                                                        &
   gc_int_kind, gc_log_kind

USE mpl, ONLY:                                                                 &
    MPL_Int_Kind, MPL_Log_Kind,                                                &
    MPL_Status_Size

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  request        ,                                                             &
  STAT(mpl_status_size),                                                       &
  ERROR

LOGICAL (KIND=gc_log_kind) :: flag

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_request,                                                                   &
  l_status(mpl_status_size),                                                   &
  l_error

LOGICAL (KIND=mpl_log_kind) :: l_flag

!=======================================================================

#if defined(MPI_SRC)
l_request = request

CALL MPI_Test(l_request, l_flag, l_status, l_error)

flag          = l_flag
STAT(:)       = l_status(:)
request       = l_request
ERROR         = l_error
#endif

RETURN
END SUBROUTINE MPL_Test
