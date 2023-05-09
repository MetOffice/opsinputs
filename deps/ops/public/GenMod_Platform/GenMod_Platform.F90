!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Module for general platform specific Fortran code.
!-------------------------------------------------------------------------------

MODULE GenMod_Platform

IMPLICIT NONE

SAVE

! dummy initial value to avoid divide by zero if not set
INTEGER :: ClockTicksPerSecond = 1

INTERFACE
  ! DEPENDS ON: Gen_CPUTime.o
  SUBROUTINE Gen_CPUTime (t,  &
                          rc)
  REAL    :: t
  INTEGER :: rc
  END SUBROUTINE Gen_CPUTime

  ! DEPENDS ON: Gen_ClockTicks.o
  SUBROUTINE Gen_ClockTicks (pi_clockticks)
  INTEGER :: pi_clockticks
  END SUBROUTINE Gen_ClockTicks

  ! DEPENDS ON: Gen_GetHostname.o
  SUBROUTINE Gen_GetHostname (hostname,     &
                              hostname_len, &
                              iret)
  CHARACTER(len=*) :: hostname
  INTEGER          :: hostname_len
  INTEGER          :: iret
  END SUBROUTINE Gen_GetHostname

  ! DEPENDS ON: Gen_Memory.o
  SUBROUTINE Gen_Memory (memory_used)
  INTEGER :: memory_used
  END SUBROUTINE Gen_Memory
END INTERFACE

CONTAINS

#include "Gen_FlushUnit.inc"
INCLUDE 'Gen_SetupCPUTime.inc'
INCLUDE 'GenFn_ThreadCount.inc'
INCLUDE 'GenFn_ThreadMax.inc'
INCLUDE 'GenFn_ThreadNum.inc'

END MODULE GenMod_Platform
