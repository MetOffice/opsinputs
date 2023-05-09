! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

FUNCTION gc_nproc ()
!     ******************************************************************
!     * Purpose:
!     *
!     *  Return the total number of processors.
!     *
!     * Input:
!     *
!     * Output:
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE gc_globals_mod, ONLY:                                                      &
    gc__nproc

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind) :: gc_nproc

gc_nproc = gc__nproc

RETURN
END FUNCTION gc_nproc
