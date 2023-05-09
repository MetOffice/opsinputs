!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Module for sleeping functionality.
!-------------------------------------------------------------------------------

MODULE GenMod_Sleep

IMPLICIT NONE

SAVE

INTERFACE
  ! DEPENDS ON: Gen_SleepWrapper.o
  SUBROUTINE Gen_SleepWrapper (seconds, &
                               ret) BIND (C, name = "Gen_SleepWrapper")
    USE ISO_C_BINDING, ONLY: &
      C_INT,                 &
      C_LONG
    INTEGER(kind=C_INT), VALUE :: seconds
    INTEGER(kind=C_LONG)       :: ret
  END SUBROUTINE Gen_SleepWrapper
END INTERFACE

CONTAINS

INCLUDE 'Gen_BlockingWait.inc'
INCLUDE 'Gen_Sleep.inc'
INCLUDE 'Gen_SpinWait.inc'

END MODULE GenMod_Sleep
