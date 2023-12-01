!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Sets up the ODB report type values.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ReadODBReportTypeMap

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  StatusOK,            &
  UseTrace

USE OpsMod_IOUtils, ONLY: &
  Ops_OpenEnvRead

IMPLICIT NONE

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_ReadODBReportTypeMap"
INTEGER                     :: unit
INTEGER                     :: iostat
INTEGER                     :: obsgroup
INTEGER                     :: satid
INTEGER                     :: report_type
INTEGER                     :: num_lines
CHARACTER(len=200)          :: message

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (.NOT. have_read_map) THEN

  CALL Ops_OpenEnvRead ("OPS_ODB_REPORT_TYPE_MAP",        &
                        unit,                             &
                        status_if_env_not_set = StatusOK)

  IF (unit > 0) THEN
    num_lines = 0
    DO
      READ (unit, *, IOSTAT = iostat) obsgroup, satid, report_type
      IF (iostat < 0) EXIT
      IF (iostat > 0) THEN
        WRITE (message, '(A,I0)') "Failure in read of report type map, rc = ", iostat
        CALL gen_fail (RoutineName, &
                       message)
      END IF
      num_lines = num_lines + 1
    END DO
    REWIND (unit)
    ALLOCATE (map(num_lines,3))
    num_lines = 0
    DO
      READ (unit, *, IOSTAT = iostat) obsgroup, satid, report_type
      IF (iostat < 0) EXIT
      IF (iostat > 0) THEN
        WRITE (message, '(A,I0)') "Failure in read of report type map (2), rc = ", iostat
        CALL gen_fail (RoutineName, &
                       message)
      END IF
      num_lines = num_lines + 1
      map(num_lines,1) = obsgroup
      map(num_lines,2) = satid
      map(num_lines,3) = report_type
    END DO
  ELSE
    ALLOCATE (map(0,0))
  END IF

  have_read_map = .TRUE.

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ReadODBReportTypeMap
