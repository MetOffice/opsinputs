! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Buffer_Attach()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Setup buffer for buffered comms
!     *
!     *  Output:  error
!     *
!     ******************************************************************

SUBROUTINE MPL_Buffer_Attach (buffer, bufsize, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::                                                  &
  buffer(*),                                                                   &
  bufsize,                                                                     &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_bufsize,                                                                   &
  l_error

!=======================================================================

l_bufsize = bufsize

CALL MPI_Buffer_Attach(buffer,l_bufsize,l_error)

ERROR = l_error

RETURN
END SUBROUTINE MPL_Buffer_Attach
