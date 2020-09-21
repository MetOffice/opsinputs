! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! Please refer to Copyright file in top level GCOM directory
!                 for further details
! *****************************COPYRIGHT*******************************

FUNCTION gc_me ()
!     ******************************************************************
!     * Purpose:
!     *
!     *  Return my node id, in the range 0...nprocs-1.
!     *
!     * Input:
!     *
!     * Output:
!     *
!     * NOTES:
!     *
!     ******************************************************************

USE gc_globals_mod, ONLY:                                                      &
    gc__me

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind) :: gc_me

gc_me = gc__me

RETURN
END FUNCTION gc_me
