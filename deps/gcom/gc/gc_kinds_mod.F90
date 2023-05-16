! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************
MODULE gc_kinds_mod

IMPLICIT NONE
PRIVATE

PUBLIC ::                                                                      &
gc_int_kind,                                                                   &
gc_integer32,                                                                  &
gc_integer64,                                                                  &
gc_real_kind,                                                                  &
gc_real32,                                                                     &
gc_real64,                                                                     &
gc_log_kind

! Description:
!   Contains parameters defining kinds for 32 and 64 integers
!   and reals.

! Parameters for 32 and 64 bit kinds

! Precision and range for 64 bit real
INTEGER, PARAMETER :: gc_prec64  = 15
INTEGER, PARAMETER :: gc_range64 = 307

! Precision and range for 32 bit real
INTEGER, PARAMETER :: gc_prec32  = 6
INTEGER, PARAMETER :: gc_range32 = 37

! Range for integers
INTEGER, PARAMETER :: gc_irange64=15
INTEGER, PARAMETER :: gc_irange32=9

! Kind for 64 bit real
INTEGER, PARAMETER :: gc_real64  =                                             &
                      SELECTED_REAL_KIND(gc_prec64,gc_range64)
! Kind for 32 bit real
INTEGER, PARAMETER :: gc_real32  =                                             &
                      SELECTED_REAL_KIND(gc_prec32,gc_range32)
! Kind for 64 bit integer
INTEGER, PARAMETER :: gc_integer64 =                                           &
                      SELECTED_INT_KIND(gc_irange64)
! Kind for 32 bit integer
INTEGER, PARAMETER :: gc_integer32 =                                           &
                      SELECTED_INT_KIND(gc_irange32)

#if defined(PREC_32B)
INTEGER, PARAMETER :: gc_int_kind  = gc_integer32
INTEGER, PARAMETER :: gc_log_kind  = gc_integer32
INTEGER, PARAMETER :: gc_real_kind = gc_real32
#else
INTEGER, PARAMETER :: gc_int_kind  = gc_integer64
INTEGER, PARAMETER :: gc_log_kind  = gc_integer64
INTEGER, PARAMETER :: gc_real_kind = gc_real64
#endif

END MODULE
