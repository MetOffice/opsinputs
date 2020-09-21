! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Allreduce()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Reductions with answers going to all.
!     *
!     *  Output:  recvbuf, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Allreduce (sendbuf, recvbuf, cnt, datatype, op, comm, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  sendbuf(*),                                                                  &
  recvbuf(*),                                                                  &
  cnt,                                                                         &
  datatype,                                                                    &
  op,                                                                          &
  comm,                                                                        &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_count,                                                                     &
  l_op,                                                                        &
  l_datatype,                                                                  &
  l_comm,                                                                      &
  l_error

!=======================================================================

l_count    = cnt
l_op       = op
l_datatype = datatype
l_comm     = comm

CALL MPI_Allreduce(sendbuf, recvbuf, l_count, l_datatype, l_op,                &
                   l_comm, l_error)

ERROR = l_error

RETURN
END SUBROUTINE MPL_Allreduce
