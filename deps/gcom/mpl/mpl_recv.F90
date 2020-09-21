! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Recv()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Normal Receive
!     *
!     *  Output:  buf, stat, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Recv (buf, cnt, datatype, SOURCE, tag, comm, STAT, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Status_Size

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  buf(*),                                                                      &
  cnt,                                                                         &
  datatype,                                                                    &
  SOURCE,                                                                      &
  tag,                                                                         &
  comm,                                                                        &
  STAT(mpl_status_size),                                                       &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_count,                                                                     &
  l_source,                                                                    &
  l_tag,                                                                       &
  l_datatype,                                                                  &
  l_comm,                                                                      &
  l_status(mpl_status_size),                                                   &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_count    = cnt
l_source   = SOURCE
l_tag      = tag
l_datatype = datatype
l_comm     = comm

CALL MPI_Recv(buf, l_count, l_datatype, l_source, l_tag, l_comm,               &
              l_status, l_error)

STAT(:)   = l_status(:)
ERROR     = l_error
#endif

RETURN
END SUBROUTINE MPL_Recv
