! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Probe()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Probe unreceived message to find status
!     *
!     *  Output:  stat, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Probe (SOURCE, tag, comm, STAT, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Status_Size

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  SOURCE,                                                                      &
  tag,                                                                         &
  comm,                                                                        &
  STAT(mpl_status_size),                                                       &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_source,                                                                    &
  l_tag,                                                                       &
  l_comm,                                                                      &
  l_status(mpl_status_size),                                                   &
  l_error

!=======================================================================

l_source   = SOURCE
l_tag      = tag
l_comm     = comm

CALL MPI_Probe(l_source, l_tag, l_comm, l_status, l_error)

STAT(:)   = l_status(:)
ERROR     = l_error

RETURN
END SUBROUTINE MPL_Probe
