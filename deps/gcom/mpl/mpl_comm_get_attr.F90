! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Comm_Get_Attr ()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Get attributes associated with communicator
!     *
!     *  Output: attribute_val, flag, ierror
!     *
!     ******************************************************************

SUBROUTINE MPL_Comm_Get_Attr (comm, keyval, attribute_val, flag, ierror)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Address_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  comm,                                                                        &
  keyval,                                                                      &
  ierror

INTEGER (KIND=mpl_address_kind) ::                                             &
  attribute_val

LOGICAL (KIND=gc_int_kind) ::                                                  &
  flag

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind)  ::                                                &
  l_comm,                                                                      &
  l_keyval,                                                                    &
  l_ierror

LOGICAL (KIND=mpl_int_kind) ::                                                 &
  l_flag

!=======================================================================

l_comm   = comm
l_keyval = keyval
l_flag   = flag

CALL MPI_Comm_Get_Attr(l_comm, l_keyval, attribute_val, l_flag, l_ierror)

flag   = l_flag
ierror = l_ierror

RETURN
END SUBROUTINE MPL_Comm_Get_Attr
