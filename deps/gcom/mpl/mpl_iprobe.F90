! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Iprobe()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Returns flag=true if there is a message that can be received
!     *  that matches the pattern specified by the arguments source,
!     *  tag and comm.
!     *
!     *  Output:  flag, istatus,  error
!     *
!     ******************************************************************

SUBROUTINE MPL_Iprobe (SOURCE, tag, comm, flag, istatus, ERROR)

USE gc_kinds_mod, ONLY:                                                        &
    gc_int_kind,  gc_log_kind

USE mpl, ONLY:                                                                 &
    MPL_Int_Kind, MPL_Log_Kind,                                                &
    MPL_Status_Size

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  SOURCE,                                                                      &
  tag,                                                                         &
  comm,                                                                        &
  istatus(mpl_status_size),                                                    &
  ERROR
LOGICAL (KIND=gc_log_kind) :: flag

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_source,                                                                    &
  l_tag,                                                                       &
  l_comm,                                                                      &
  l_status(mpl_status_size),                                                   &
  l_error
LOGICAL (KIND=mpl_log_kind) :: l_flag

!=======================================================================

l_source   = SOURCE
l_tag      = tag
l_comm     = comm

CALL MPI_Iprobe(l_source, l_tag, l_comm, l_flag, l_status, l_error)

flag       = l_flag
istatus(:) = l_status(:)
ERROR      = l_error

RETURN
END SUBROUTINE MPL_Iprobe
