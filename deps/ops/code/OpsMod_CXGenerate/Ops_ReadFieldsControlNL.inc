!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Reads a fields control namelist.  The relevant information is read using a
! namelist read. If a problem is encountered a flag is raised and a report
! issued.
!
! Inputs:
!     Name of the control file to read
!
! Outputs:
!     The values in the namelist in that control file
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ReadFieldsControlNL

USE GenMod_Control, ONLY: &
    GeneralMode,          &
    VerboseMode,          &
    mype

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    StatusOK,          &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI

USE OpsMod_Control, ONLY: &
    ModelType,            &
    ModelType_ATMOS

USE OpsMod_IOUtils, ONLY: &
    Ops_CloseFile,        &
    Ops_OpenEnvRead,      &
    Ops_OpenFileRead

IMPLICIT NONE

! Local declarations:
CHARACTER (LEN=*), PARAMETER :: RoutineName = "Ops_ReadFieldsControlNL"
INTEGER                      :: Unit
INTEGER                      :: ForecastPeriod

NAMELIST / FieldsControlNL /  &
   cx_interp_method,          &
   ForecastPeriod,            &
   ForecastPeriods,           &
   BGE_ForecastPeriod,        &
   SSTBackground_Age,         &
   UseValidityTimes,          &
   LevelLapse,                &
   ZVarMinLapse,              &
   xRadiusLapse,              &
   yRadiusLapse,              &
   cx_no_interpolation,       &
   seaice_theshold_for_sst

!-----------------------------------------------------------------------------
! 0. Setup and initialise namelist variables
!-----------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

ForecastPeriod = IMDI
ForecastPeriods = IMDI
IF (ModelType /= ModelType_ATMOS) THEN
  BGE_ForecastPeriod = 0
ELSE
  BGE_ForecastPeriod = 6
END IF
SSTBackground_Age = IMDI
UseValidityTimes = .FALSE.

!-----------------------------------------------------------------------------
! 1. Try to open the namelist
!-----------------------------------------------------------------------------

CALL Ops_OpenEnvRead ("OPS_EX_FIELDSCONTROL_NL",        &
                      Unit,                             &
                      status_if_env_not_set = StatusOK)
IF (Unit <= 0) THEN
  CALL Ops_OpenFileRead ("ops_fields_control.nl", &
                         Unit)
END IF

!-----------------------------------------------------------------------------
! 2. Read the namelist
!-----------------------------------------------------------------------------

READ (Unit, NML = FieldsControlNL)
IF (GeneralMode >= VerboseMode .AND. mype == 0) THEN
  WRITE (MessageOut, NML = FieldsControlNL)
END IF

!-----------------------------------------------------------------------------
! 3. Close the namelist file
!-----------------------------------------------------------------------------

CALL Ops_CloseFile (Unit)

!-----------------------------------------------------------------------------
! 4. Ensure that forecastperiods array is set correctly
!-----------------------------------------------------------------------------

IF (ForecastPeriods(1) == IMDI .AND. ForecastPeriod /= IMDI) THEN
  ForecastPeriods(1) = ForecastPeriod
END IF

!-------------------------------------------------------------------------------
! 5. Tracing and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ReadFieldsControlNL
