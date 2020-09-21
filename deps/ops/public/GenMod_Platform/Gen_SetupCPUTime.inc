!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Finds out the scaling factor for CPU time, to allow Gen_CPUTime to return
! seconds. CPU time is found from the POSIX sysconv(_SC_CLK_TCK) function.
!
! NOTE
!   Tracing not used to avoid circular dependancy within the module.
!
! TODO
!   Use SYSTEM_CLOCK(COUNT_RATE = ClockTicksPerSecond)?
!-------------------------------------------------------------------------------

SUBROUTINE Gen_SetupCPUTime

IMPLICIT NONE

CALL Gen_ClockTicks (ClockTicksPerSecond)

END SUBROUTINE Gen_SetupCPUTime
