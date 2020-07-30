! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Unpack()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Unpack Data
!     *
!     *  Output:  posn, outbuf, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Unpack (inbuf, insize, posn, outbuf, outcount,                  &
                     datatype, comm, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
INTEGER (KIND=gc_int_kind), INTENT(IN)    :: inbuf(*)
INTEGER (KIND=gc_int_kind), INTENT(IN)    :: insize
INTEGER (KIND=gc_int_kind), INTENT(IN OUT) :: posn
INTEGER (KIND=gc_int_kind), INTENT(OUT)   :: outbuf(*)
INTEGER (KIND=gc_int_kind), INTENT(IN)    :: outcount
INTEGER (KIND=gc_int_kind), INTENT(IN)    :: datatype
INTEGER (KIND=gc_int_kind), INTENT(IN)    :: comm
INTEGER (KIND=gc_int_kind), INTENT(OUT)   :: ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::  l_insize
INTEGER (KIND=mpl_int_kind) ::  l_position
INTEGER (KIND=mpl_int_kind) ::  l_outcount
INTEGER (KIND=mpl_int_kind) ::  l_datatype
INTEGER (KIND=mpl_int_kind) ::  l_comm
INTEGER (KIND=mpl_int_kind) ::  l_error

!=======================================================================

#if defined(MPI_SRC)
l_insize   = insize
l_position = posn
l_outcount = outcount
l_datatype = datatype
l_comm     = comm

CALL MPI_Unpack(inbuf, l_insize, l_position, outbuf, l_outcount, l_datatype,   &
             l_comm, l_error)

posn     = l_position
ERROR    = l_error
#endif

RETURN
END SUBROUTINE MPL_Unpack
