! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Issend()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Immediate Synchronised Send
!     *
!     *  Output:  error, request
!     *
!     ******************************************************************

SUBROUTINE MPL_Issend (buf, cnt, datatype, dest, tag, comm, request, ERROR)

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
  request,                                                                     &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_count,                                                                     &
  l_dest,                                                                      &
  l_tag,                                                                       &
  l_datatype,                                                                  &
  l_comm,                                                                      &
  l_request,                                                                   &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_count    = cnt
l_dest     = dest
l_tag      = tag
l_datatype = datatype
l_comm     = comm

CALL MPI_Issend(buf, l_count, l_datatype, l_dest, l_tag, l_comm,               &
               l_request, l_error)

request = l_request
ERROR   = l_error
#endif

RETURN
END SUBROUTINE MPL_Issend
