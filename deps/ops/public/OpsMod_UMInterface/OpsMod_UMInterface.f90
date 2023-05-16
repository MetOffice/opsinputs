!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! This module makes certain UM portio functions available and handles
! dependencies on portio header files.
!-------------------------------------------------------------------------------

MODULE OpsMod_UMInterface

IMPLICIT NONE

SAVE

! Depends on: ops_portio2a.o
! Depends on: ops_portutils.o

EXTERNAL abort
EXTERNAL ops_buffin
EXTERNAL ops_buffin16
EXTERNAL ops_buffin32
EXTERNAL ops_buffin8
EXTERNAL ops_buffo32
EXTERNAL ops_buffou8
EXTERNAL ops_buffout
EXTERNAL ops_file_close
EXTERNAL ops_file_open
EXTERNAL ops_fort_get_env
EXTERNAL ops_setpos
EXTERNAL ops_setpos8
EXTERNAL ops_setpos32

END MODULE OpsMod_UMInterface
