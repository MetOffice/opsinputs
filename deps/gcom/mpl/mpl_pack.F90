! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Pack()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Pack Data
!     *
!     *  Output:  posn, outbuf, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Pack (inbuf, incount, datatype, outbuf, outsize, posn,          &
                     comm, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind), INTENT(IN)    :: inbuf(*)
INTEGER (KIND=gc_int_kind), INTENT(IN)    :: incount
INTEGER (KIND=gc_int_kind), INTENT(IN)    :: datatype
INTEGER (KIND=gc_int_kind), INTENT(OUT)   :: outbuf(*)
INTEGER (KIND=gc_int_kind), INTENT(IN)    :: outsize
INTEGER (KIND=gc_int_kind), INTENT(IN OUT) :: posn
INTEGER (KIND=gc_int_kind), INTENT(IN)    :: comm
INTEGER (KIND=gc_int_kind), INTENT(OUT)   :: ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::  l_incount
INTEGER (KIND=mpl_int_kind) ::  l_datatype
INTEGER (KIND=mpl_int_kind) ::  l_outsize
INTEGER (KIND=mpl_int_kind) ::  l_position
INTEGER (KIND=mpl_int_kind) ::  l_comm
INTEGER (KIND=mpl_int_kind) ::  l_error

!=======================================================================

#if defined(MPI_SRC)
l_incount  = incount
l_datatype = datatype
l_outsize  = outsize
l_position = posn
l_comm     = comm

CALL MPI_Pack(inbuf, l_incount, l_datatype, outbuf, l_outsize,                 &
             l_position, l_comm, l_error)

ERROR    = l_error
posn     = l_position
#endif

RETURN
END SUBROUTINE MPL_Pack
