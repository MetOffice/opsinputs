! (C) Crown Copyright 2020, the Met Office. All rights reserved.
!

!> MPL functions used by VarObsWriter or CxWriter, but not wrapped by OPS.

module opsinputs_mpl_mod

implicit none

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

!> Wrapper of the MPL_Allgather function for integer arguments.
subroutine opsinputs_mpl_allgather_integer(sendbuf, sendcount,  sendtype, &
                                           recvbuf, recvcount,  recvtype, &
                                           comm,    error)
use mpl, only: gc_int_kind
implicit none

! Subroutine arguments:
integer(kind=gc_int_kind) :: sendbuf(:)
integer(kind=gc_int_kind) :: sendcount
integer(kind=gc_int_kind) :: sendtype
integer(kind=gc_int_kind) :: recvbuf(:)
integer(kind=gc_int_kind) :: recvcount
integer(kind=gc_int_kind) :: recvtype
integer(kind=gc_int_kind) :: comm
integer(kind=gc_int_kind) :: error

! Local declarations:
external mpl_allgather

! Body:
call mpl_allgather(sendbuf,   &
                   sendcount, &
                   sendtype,  &
                   recvbuf,   &
                   recvcount, &
                   recvtype,  &
                   comm,      &
                   error)

end subroutine opsinputs_mpl_allgather_integer

end module opsinputs_mpl_mod
