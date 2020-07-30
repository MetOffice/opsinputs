!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Contains wrapper routines for some portio2a functions.
!-------------------------------------------------------------------------------

MODULE OpsMod_Portio_Wrappers

IMPLICIT NONE

SAVE

INTERFACE Ops_Buffin32_Wrapper
  MODULE PROCEDURE Ops_Buffin32_Wrapper_int
  MODULE PROCEDURE Ops_Buffin32_Wrapper_real
  MODULE PROCEDURE Ops_Buffin32_Wrapper_real32
END INTERFACE Ops_Buffin32_Wrapper

INTERFACE Ops_Buffout32_Wrapper
  MODULE PROCEDURE Ops_Buffout32_Wrapper_real
  MODULE PROCEDURE Ops_Buffout32_Wrapper_real32
END INTERFACE Ops_Buffout32_Wrapper

INTERFACE Ops_Buffin_Wrapper
  MODULE PROCEDURE Ops_Buffin_Wrapper_char
  MODULE PROCEDURE Ops_Buffin_Wrapper_int
  MODULE PROCEDURE Ops_Buffin_Wrapper_int2d
  MODULE PROCEDURE Ops_Buffin_Wrapper_int32
  MODULE PROCEDURE Ops_Buffin_Wrapper_int322d
  MODULE PROCEDURE Ops_Buffin_Wrapper_real
  MODULE PROCEDURE Ops_Buffin_Wrapper_real32
END INTERFACE Ops_Buffin_Wrapper

INTERFACE Ops_Buffout_Wrapper
  MODULE PROCEDURE Ops_Buffout_Wrapper_char
  MODULE PROCEDURE Ops_Buffout_Wrapper_char_array
  MODULE PROCEDURE Ops_Buffout_Wrapper_int
  MODULE PROCEDURE Ops_Buffout_Wrapper_int2d
  MODULE PROCEDURE Ops_Buffout_Wrapper_int32
  MODULE PROCEDURE Ops_Buffout_Wrapper_int322d
  MODULE PROCEDURE Ops_Buffout_Wrapper_real
  MODULE PROCEDURE Ops_Buffout_Wrapper_real2dfrom1d
  MODULE PROCEDURE Ops_Buffout_Wrapper_real32
  MODULE PROCEDURE Ops_Buffout_Wrapper_real322dfrom1d
END INTERFACE Ops_Buffout_Wrapper

CONTAINS

INCLUDE 'Ops_Buffin32_Wrapper_int.inc'
INCLUDE 'Ops_Buffin32_Wrapper_real.inc'
INCLUDE 'Ops_Buffin32_Wrapper_real32.inc'
INCLUDE 'Ops_Buffin_Wrapper_char.inc'
INCLUDE 'Ops_Buffin_Wrapper_int.inc'
INCLUDE 'Ops_Buffin_Wrapper_int2d.inc'
INCLUDE 'Ops_Buffin_Wrapper_int32.inc'
INCLUDE 'Ops_Buffin_Wrapper_int322d.inc'
INCLUDE 'Ops_Buffin_Wrapper_real.inc'
INCLUDE 'Ops_Buffin_Wrapper_real32.inc'
INCLUDE 'Ops_Buffout32_Wrapper_real.inc'
INCLUDE 'Ops_Buffout32_Wrapper_real32.inc'
INCLUDE 'Ops_Buffout_Wrapper_char.inc'
INCLUDE 'Ops_Buffout_Wrapper_char_array.inc'
INCLUDE 'Ops_Buffout_Wrapper_int.inc'
INCLUDE 'Ops_Buffout_Wrapper_int2d.inc'
INCLUDE 'Ops_Buffout_Wrapper_int32.inc'
INCLUDE 'Ops_Buffout_Wrapper_int322d.inc'
INCLUDE 'Ops_Buffout_Wrapper_real.inc'
INCLUDE 'Ops_Buffout_Wrapper_real2dfrom1d.inc'
INCLUDE 'Ops_Buffout_Wrapper_real32.inc'
INCLUDE 'Ops_Buffout_Wrapper_real322dfrom1d.inc'
INCLUDE 'Ops_Setpos_Wrapper.inc'

INCLUDE 'ops_coex.inc'
INCLUDE 'ops_coex_old.inc'
INCLUDE 'ops_xpnd_all.inc'

END MODULE OpsMod_Portio_Wrappers
