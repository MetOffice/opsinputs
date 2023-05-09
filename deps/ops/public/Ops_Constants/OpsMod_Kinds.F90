!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Module to store kind values for various datatypes. Platform dependencies
! can be isolated here.
!-------------------------------------------------------------------------------

MODULE OpsMod_Kinds

#ifdef HAVE_ISO_FORTRAN_ENV
#ifndef HAVE_ISO_FORTRAN_ENV_INT8
#define HAVE_ISO_FORTRAN_ENV_INT8 1
#endif

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: &
#ifdef HAVE_ISO_FORTRAN_ENV_INT8
  int8,                                  &
#endif
  int16,                                 &
  int32,                                 &
  int64,                                 &
  real32,                                &
  real64

#endif

IMPLICIT NONE

#ifdef HAVE_ISO_FORTRAN_ENV

#ifdef HAVE_ISO_FORTRAN_ENV_INT8
INTEGER, PARAMETER :: integer8 = int8
#else
INTEGER, PARAMETER :: integer8 = SELECTED_INT_KIND (2)
#endif
INTEGER, PARAMETER :: integer16 = int16
INTEGER, PARAMETER :: integer32 = int32
INTEGER, PARAMETER :: integer64 = int64
INTEGER, PARAMETER :: logical32 = int32
INTEGER, PARAMETER :: logical64 = int64

#else

! Precision and range for 64 bit real
INTEGER, PARAMETER :: prec64  = 15
INTEGER, PARAMETER :: range64 = 307

! Precision and range for 32 bit real
INTEGER, PARAMETER :: prec32  = 6
INTEGER, PARAMETER :: range32 = 37

! Range for integers
INTEGER, PARAMETER :: irange64=15
INTEGER, PARAMETER :: irange32=9

! Kind for 64 bit real
INTEGER, PARAMETER :: real64  = SELECTED_REAL_KIND(prec64,range64)
! Kind for 32 bit real
INTEGER, PARAMETER :: real32  = SELECTED_REAL_KIND(prec32,range32)
! Kind for 64 bit integer
INTEGER, PARAMETER :: integer64 = SELECTED_INT_KIND(irange64)
! Kind for 32 bit integer
INTEGER, PARAMETER :: integer32 = SELECTED_INT_KIND(irange32)

! 16bit integers not included from um_types.
INTEGER, PARAMETER :: integer8 = SELECTED_INT_KIND (2)
INTEGER, PARAMETER :: integer16 = SELECTED_INT_KIND (4)

! Kinds for 64 and 32 bit logicals. Note that there is no
! "selected_logical_kind", but using the equivalent integer kind is a
! workaround that works on every platform we have tested.
INTEGER, PARAMETER :: logical64 = integer64
INTEGER, PARAMETER :: logical32 = integer32

#endif

! There are certain situations where, according to the Fortran standard,
! "default" integer must be used.  When compiler options to promote all integers
! and reals to 64 bits are in use there is no consistent definition of what
! default integer is.
#ifdef DEFAULT_INTEGER_32BIT
INTEGER, PARAMETER :: integer_default = integer32
INTEGER, PARAMETER :: logical_default = logical32
#else
INTEGER, PARAMETER :: integer_default = integer64
INTEGER, PARAMETER :: logical_default = logical64
#endif

END MODULE OpsMod_Kinds
