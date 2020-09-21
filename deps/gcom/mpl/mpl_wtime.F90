! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file Copyright
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Function MPL_Wtime()
!
!     ******************************************************************
!     * Purpose:
!     *
!     *  Provide a wallclock time in seconds
!     *
!     *  Return:  mpl_wtime
!     *
!     *  Note:- If gc_real_kind is lower than 8-bytes this is likely to
!     *         reduce precision of returned timer values
!     ******************************************************************

FUNCTION MPL_Wtime ()

USE gc_kinds_mod, ONLY: gc_real_kind

#if defined(MPI_SRC)
USE mpl, ONLY: mpi_wtime
#endif

IMPLICIT NONE

REAL (KIND=gc_real_kind) :: mpl_wtime

!=======================================================================

#if defined(MPI_SRC)

mpl_wtime = mpi_wtime()

#else

mpl_wtime = -1

#endif

RETURN
END FUNCTION MPL_Wtime
