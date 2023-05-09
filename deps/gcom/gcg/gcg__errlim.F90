! *****************************COPYRIGHT*******************************
! (c) CROWN COPYRIGHT, Met Office, All Rights Reserved.
! *****************************COPYRIGHT*******************************

!=======================================================================
!  This is an INTERNAL routine to be used within the GC interface ONLY.
!=======================================================================

#include "gcg_prolog.h"

SUBROUTINE gcg__errlim(iabrt, sub, lim, mval, aval)

!     Support function to exit or abort (if IABRT > 0) if an internal
!     limit is exceeded.

!     Currently, only abort is supported (RS 970408)

USE gc_kinds_mod, ONLY: gc_int_kind

IMPLICIT NONE

INTEGER (KIND=gc_int_kind) :: iabrt, mval, aval
CHARACTER(LEN=*)           :: sub, lim
INTEGER (KIND=gc_int_kind) :: gcg__nproc, gcg__me
#include "gc_functions.h"
EXTERNAL gc_me, gc_nproc

gcg__me = gc_me()
gcg__nproc = gc_nproc()
WRITE(*,*) 'GCG_', sub, '(): internal limit MAX_', lim,                        &
           ' exceeded on processor ', gcg__me
WRITE(*,*) 'Maximum value is ', mval, '. Actual value is ',                    &
           aval, '. Exiting.'

CALL gc_abort(gcg__me, gcg__nproc, '*** DEFINED LIMIT EXCEEDED ***')

RETURN
END SUBROUTINE gcg__errlim
