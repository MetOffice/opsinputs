! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine mpl_init_thread()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Initialize the (threaded) MPI execution environment
!     *
!     *  Output: provided, error
!     *
!     ******************************************************************

SUBROUTINE Mpl_Init_Thread (required, provided, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::ERROR
INTEGER (KIND=gc_int_kind) ::required
INTEGER (KIND=gc_int_kind) ::provided

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::  l_error
INTEGER (KIND=mpl_int_kind) ::  l_provided
INTEGER (KIND=mpl_int_kind) ::  l_required

!=======================================================================

l_required=required

CALL MPI_Init_Thread(l_required, l_provided, l_error)

provided=l_provided
ERROR = l_error

RETURN
END SUBROUTINE Mpl_Init_Thread
