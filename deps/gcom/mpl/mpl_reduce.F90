! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Reduce()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Reductions with answers going to root.
!     *
!     *  Output:  recvbuf, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Reduce (sendbuf, recvbuf, cnt, datatype, op, root, comm, ERROR)

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
  root,                                                                        &
  comm,                                                                        &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_count,                                                                     &
  l_op,                                                                        &
  l_root,                                                                      &
  l_datatype,                                                                  &
  l_comm,                                                                      &
  l_error

!=======================================================================

#if defined(MPI_SRC)
l_count    = cnt
l_op       = op
l_datatype = datatype
l_comm     = comm
l_root     = root

CALL MPI_Reduce(sendbuf, recvbuf, l_count, l_datatype, l_op, l_root,           &
                l_comm, l_error)

ERROR = l_error
#endif

RETURN
END SUBROUTINE MPL_Reduce
