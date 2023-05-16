! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Op_Create()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Creates a new collective operation
!     *
!     *  Output: op, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Op_Create (func, commute, op, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind, gc_log_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Log_Kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind) ::                                                  &
  op,                                                                          &
  ERROR

LOGICAL (KIND=gc_log_kind) :: commute

EXTERNAL :: func

INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_op,                                                                        &
  l_error

LOGICAL (KIND=mpl_log_kind) :: l_commute

!=======================================================================

l_commute  = commute

CALL MPI_Op_Create(func, l_commute, l_op, l_error)

op      = l_op
ERROR   = l_error

RETURN
END SUBROUTINE MPL_Op_Create
