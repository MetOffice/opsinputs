! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Subroutine MPL_Get_Processor_Name()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Get the identifier for the CPU - name can be different
!     *  depending on MPI implementation.
!     *
!     *  Output:  nme, resultlen, error
!     *
!     ******************************************************************

SUBROUTINE MPL_Get_Processor_Name( nme, resultlen, ERROR)

USE gc_kinds_mod, ONLY: gc_int_kind
USE mpl, ONLY: MPL_Int_Kind, MPL_Max_Processor_Name

IMPLICIT NONE

! Arguments and Variables at Model/GCOM precision level
CHARACTER (LEN=mpl_max_processor_name) :: nme
INTEGER (KIND=gc_int_kind) ::                                                  &
  resultlen,                                                                   &
  ERROR

! Arguments and Variables at Internal/MPI Library precision level
INTEGER (KIND=mpl_int_kind) ::                                                 &
  l_resultlen,                                                                 &
  l_error

!=======================================================================

#if defined(MPI_SRC)
CALL MPI_Get_Processor_Name( nme, l_resultlen, l_error )

resultlen = l_resultlen
ERROR     = l_error
#endif

RETURN
END SUBROUTINE MPL_Get_Processor_Name
