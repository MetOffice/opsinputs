! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

!=======================================================================
!  This is an INTERNAL routine to be used within the GC interface ONLY.
!=======================================================================

#include "gc_prolog.h"

SUBROUTINE gc__errlim(iabrt, sub, lim, mval, aval)

!     Support function to exit or abort (if IABRT > 0) if an internal
!     limit is exceeded.

!     Currently, only abort is supported (RS 970408)
!
USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

#include "gc_functions.h"

INTEGER (KIND=gc_int_kind) :: iabrt, mval, aval
CHARACTER(LEN=*) :: sub, lim

WRITE(*,*) 'GC_', sub, '(): internal limit MAX_', lim,                         &
           ' exceeded on processor ', gc_me()
WRITE(*,*) 'Maximum value is ', mval, '. Actual value is ',                    &
           aval, '. Exiting.'

CALL gc_abort(gc_me(), gc_nproc(), '*** STATIC LIMIT EXCEEDED ***')

RETURN
END SUBROUTINE gc__errlim
