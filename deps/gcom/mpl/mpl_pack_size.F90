! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Pack_size()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Size of Pack Data
!     *
!     *  Output:  sze, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Pack_size (incount, datatype, comm, sze, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind), INTENT(IN)  :: incount
INTEGER (KIND=gc_int_kind), INTENT(IN)  :: datatype
INTEGER (KIND=gc_int_kind), INTENT(IN)  :: comm
INTEGER (KIND=gc_int_kind), INTENT(OUT) :: sze
INTEGER (KIND=gc_int_kind), INTENT(OUT) :: ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::  l_incount
INTEGER (KIND=mpl_int_kind) ::  l_datatype
INTEGER (KIND=mpl_int_kind) ::  l_comm
INTEGER (KIND=mpl_int_kind) ::  l_size
INTEGER (KIND=mpl_int_kind) ::  l_error

!=======================================================================

#if defined(MPI_SRC)
l_incount  = incount
l_datatype = datatype
l_comm     = comm

CALL MPI_Pack_size(l_incount, l_datatype, l_comm, l_size, l_error)

sze      = l_size
ERROR    = l_error
#endif

RETURN
END SUBROUTINE MPL_Pack_size
