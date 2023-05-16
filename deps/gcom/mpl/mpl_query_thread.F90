! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************

! Subroutine mpl_query_thread()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Returns the current level of thread support
!     *
!     *  Output: provided, error
!     *
!     ******************************************************************

SUBROUTINE Mpl_Query_Thread (provided,ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind) ::ERROR
INTEGER (KIND=gc_int_kind) ::provided

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::  l_error
INTEGER (KIND=mpl_int_kind) ::  l_provided

!=======================================================================

CALL MPI_Query_Thread(l_provided,l_error)

provided = l_provided
ERROR    = l_error

RETURN
END SUBROUTINE Mpl_Query_Thread
