! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

#include "gc_prolog.h"


SUBROUTINE gc_exit()
!     ******************************************************************
!     * Purpose:
!     *
!     *  Controlled cleanup of the parallel system.
!     *
!     * Input:
!     *
!     * Output:
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE gc_globals_mod, ONLY:                                                      &
    gc__finaled

#if defined(MPI_SRC)
USE gc_kinds_mod, ONLY: gc_int_kind
#endif

IMPLICIT NONE

#if defined(MPI_SRC)
INTEGER (KIND=gc_int_kind) :: info
CALL gc__free_mpi_types()
CALL mpl_finalize(info)
#endif

gc__finaled = .TRUE.

RETURN
END SUBROUTINE gc_exit
