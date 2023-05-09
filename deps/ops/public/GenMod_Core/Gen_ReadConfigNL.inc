!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Reads the GEN config namelist.
!
! The relevant information is read using a NAMELIST read. If a problem is
! encountered a flag is raised and a report issued.
!
! INPUTS
!   Name of the control file to read (via environment variable)
!
! OUTPUTS
!   The values in the namelist in that control file
!-------------------------------------------------------------------------------

SUBROUTINE Gen_ReadConfigNL

USE GenMod_Control, ONLY: &
    GeneralMode,          &
    mype,                 &
    VerboseMode,          &
    DebugMode,            &
    L_RUDI,               &
    L_IUDI,               &
    RUDI,                 &
    IUDI,                 &
    ErrorSleep,           &
    GCOMBitRep,           &
    GenMode,              &
    WarningToError,       &
    WarningsMax,          &
    StatsAppend

IMPLICIT NONE

! Local declarations:
INTEGER                     :: file_unit
CHARACTER(len=*), PARAMETER :: RoutineName = "Gen_ReadConfigNL"

NAMELIST / Gen_ConfigNL / &
  L_RUDI,                 &
  L_IUDI,                 &
  RUDI,                   &
  IUDI,                   &
  ErrorSleep,             &
  GCOMBitRep,             &
  GenMode,                &
  WarningToError,         &
  WarningsMax,            &
  StatsAppend

CALL gen_get_unit_from_env (file_unit,           &
                            (/'GEN_CONFIG_NL'/), &
                            'gen_config.nl')
IF (file_unit /= -1) THEN
    READ (file_unit, NML = Gen_ConfigNL)
    IF (GeneralMode >= DebugMode .AND. mype == 0) WRITE (MessageOut, NML = Gen_ConfigNL)
    CLOSE (file_unit)
    CALL gen_free_unit (file_unit)
ELSE IF (GeneralMode >= VerboseMode) THEN
    CALL gen_message (RoutineName,                         &
                      "Gen_ConfigNL: use default values.")
END IF

END SUBROUTINE Gen_ReadConfigNL
