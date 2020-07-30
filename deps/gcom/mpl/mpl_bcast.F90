! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_BCast ()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Broadcast data
!     *
!     *  Output: ierr
!     *
!     ******************************************************************

SUBROUTINE MPL_BCast (buffer, cnt, datatype, root, comm, ierr )

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  buffer(*),                                                                   &
  cnt,                                                                         &
  datatype,                                                                    &
  root,                                                                        &
  comm,                                                                        &
  ierr

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind)  ::                                                &
  l_count,                                                                     &
  l_datatype,                                                                  &
  l_root,                                                                      &
  l_comm,                                                                      &
  l_ierr

!=======================================================================

#if defined(MPI_SRC)
l_count     = cnt
l_datatype  = datatype
l_root      = root
l_comm      = comm

CALL MPI_Bcast(buffer, l_count, l_datatype, l_root, l_comm, l_ierr)

ierr = l_ierr
#endif

RETURN
END SUBROUTINE MPL_BCast
