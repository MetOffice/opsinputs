!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Contains interfaces to GCOM routines.
!-------------------------------------------------------------------------------

MODULE OpsMod_Gcom

IMPLICIT NONE

SAVE

#ifdef EXTERNAL_GCOM

! This allows GCOM to be linked via a library

EXTERNAL gc_abort
EXTERNAL gc_cbcast
EXTERNAL gc_crecv
EXTERNAL gc_csend
EXTERNAL gc_exit
EXTERNAL gc_gsync
EXTERNAL gc_init
EXTERNAL gc_irecv
EXTERNAL gc_isend
EXTERNAL gc_rrecv
EXTERNAL gc_rsend
EXTERNAL gc_setopt
EXTERNAL gcg_cbcast
EXTERNAL gcg_gsync
EXTERNAL gcg_ibcast
EXTERNAL gcg_imax
EXTERNAL gcg_isum
EXTERNAL gcg_rbcast
EXTERNAL gcg_rmin
EXTERNAL gcg_rsum
EXTERNAL gcg_split

#endif

#ifndef EXTERNAL_GCOM

! This allows GCOM to be compiled in line

CONTAINS

#include "gc_abort.F90"
#include "gc_cbcast.F90"
#include "gc_crecv.F90"
#include "gc_csend.F90"
#include "gc_exit.F90"
#include "gc_gsync.F90"
#include "gc_init.F90"
#include "gc_irecv.F90"
#include "gc_isend.F90"
#include "gc_rrecv.F90"
#include "gc_rsend.F90"
#include "gc_setopt.F90"
#include "gcg_cbcast.F90"
#include "gcg_gsync.F90"
#include "gcg_ibcast.F90"
#include "gcg_imax.F90"
#include "gcg_isum.F90"
#include "gcg_rbcast.F90"
#include "gcg_rmin.F90"
#include "gcg_rsum.F90"
#include "gcg_split.F90"

#endif

END MODULE OpsMod_Gcom
