! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Bsend()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Buffered Send
!     *
!     *  Output:  error
!     *
!     ******************************************************************

SUBROUTINE MPL_Bsend (buf, cnt, datatype, dest, tag, comm, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  buf(*),                                                                      &
  cnt,                                                                         &
  datatype,                                                                    &
  dest,                                                                        &
  tag,                                                                         &
  comm,                                                                        &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_count,                                                                     &
  l_dest,                                                                      &
  l_tag,                                                                       &
  l_datatype,                                                                  &
  l_comm,                                                                      &
  l_error

!=======================================================================

l_count    = cnt
l_dest     = dest
l_tag      = tag
l_datatype = datatype
l_comm     = comm

CALL MPI_Bsend(buf, l_count, l_datatype, l_dest, l_tag, l_comm, l_error)

ERROR = l_error

RETURN
END SUBROUTINE MPL_Bsend
