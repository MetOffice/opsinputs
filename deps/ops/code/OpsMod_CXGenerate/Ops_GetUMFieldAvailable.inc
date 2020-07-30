!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Gets the availability of the current field
!
! Inputs:
!   FieldsInfo: information about fields in UM dump, prepared in
!       Ops_CXSetupArrays
!   iTime: current integer time step
!   iField: current interger field
!   NumForecastTimes: Total number of time steps
!
! Outputs:
!     ForecastTime: integer time step to use if not missing
!     ForecastMissing: logical defining if a forcast is available
!-------------------------------------------------------------------------------

SUBROUTINE Ops_GetUMFieldAvailable (ForecastTime,     &
                                    ForecastMissing,  &
                                    FieldsInfo,       &
                                    iTime,            &
                                    iField,           &
                                    NumForecastTimes, &
                                    iDumpField)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(OUT)              :: ForecastTime
LOGICAL, INTENT(OUT)              :: ForecastMissing
TYPE (FieldInfo_type), INTENT(IN) :: FieldsInfo(:)
INTEGER, INTENT(IN)               :: iTime
INTEGER, INTENT(IN)               :: iField
INTEGER, INTENT(IN)               :: NumForecastTimes
INTEGER, OPTIONAL, INTENT(OUT)    :: iDumpField

! Local declarations:
INTEGER                           :: j
INTEGER                           :: iDumpField_local
CHARACTER(len=*), PARAMETER       :: RoutineName = 'Ops_GetUMFieldAvailable'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

ForecastTime = iTime
ForecastMissing = .FALSE.

! If the field for this forecast time is not available in the dump,
! find the field at the next available forecast time.

iDumpField_local = FieldsInfo(iField) % StartPositions(iTime)
IF (iDumpField_local == IMDI .AND. &
    .NOT. (FieldsInfo(iField) % DerivedField)) THEN
  DO j = iTime, NumForecastTimes ! Look for the next available forecast time
    IF (FieldsInfo(iField) % StartPositions(j) /= IMDI) THEN
      ForecastTime = j
      iDumpField_local = FieldsInfo(iField) % StartPositions(ForecastTime)
    END IF
  END DO
END IF

IF (FieldsInfo(iField) % StartPositions(ForecastTime) == IMDI .AND. &
    .NOT. FieldsInfo(iField) % DerivedField) THEN
  ForecastMissing = .TRUE.
END IF

IF (PRESENT (iDumpField)) iDumpField = iDumpField_local

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_GetUMFieldAvailable
