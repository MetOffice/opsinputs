!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Returns the absolute current CPU time in seconds via a C system call.
!
! NOTE
!   Tracing not used to avoid circular dependancy.
!
! TODO
!   Use CPU_TIME(gen_cpu_time)?
!-------------------------------------------------------------------------------

REAL FUNCTION gen_cpu_time ()

USE GenMod_Platform, ONLY: &
  Gen_CPUTime

IMPLICIT NONE

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = 'gen_cpu_time'
INTEGER                     :: rc = 0

CALL Gen_CPUTime (gen_cpu_time, &
                  rc)

IF (rc == 1) THEN
  CALL gen_fail (RoutineName,            &
                 'Cannot find CPU time')
END IF

END FUNCTION gen_cpu_time
