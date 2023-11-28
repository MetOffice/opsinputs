!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Converts a model run string (QU etc) to a number for use by ODB.
!-------------------------------------------------------------------------------

FUNCTION OpsFn_RunidToNum (run)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_Kinds, ONLY: &
  integer_default

IMPLICIT NONE

! Function arguments:
CHARACTER(len=*), INTENT(IN)  :: run

! Function result:
INTEGER                       :: OpsFn_RunidToNum
INTEGER                       :: run_num
INTEGER                       :: time_num
INTEGER(kind=integer_default) :: error_code

! Local declarations:
CHARACTER(len=*), PARAMETER  :: RoutineName = "OpsFn_RunidToNum"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

SELECT CASE (run(1:2))
  CASE ('QU')
    run_num = 1  ! GLOBL_AIR
  CASE ('QG')
    run_num = 2  ! GLOBL_AIR_FRCT
  CASE ('QS')
    run_num = 6  ! GLOBL_STPR
  CASE ('QW')
    run_num = 7  ! GLOBL_WAVE
  CASE ('QO')
    run_num = 8  ! GLOBL_SEA
  CASE ('QM')
    run_num = 11 ! MSCAL_AIR_UK
  CASE ('Q4')
    run_num = 12 ! UK 4km
  CASE ('QV')
    run_num = 13 ! UK variable resolution
  CASE ('QY')
    run_num = 14 ! NAE forecast
  CASE ('QZ')
    run_num = 15 ! NAE update
  CASE DEFAULT
    run_num = IMDI
END SELECT

READ (run(3:4), *, IOSTAT = error_code) time_num
IF (error_code /= 0) THEN
  time_num = IMDI
END IF

IF (run_num == IMDI .OR. time_num == IMDI) THEN
  OpsFn_RunidToNum = 0
ELSE
  OpsFn_RunidToNum = 100 * run_num + time_num
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END FUNCTION OpsFn_RunidToNum
