!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Reads the random control namelist and sets the random number variables with
! the values.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ReadRandomControlNL

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

USE OpsMod_IOUtils, ONLY: &
    Ops_OpenEnvRead,      &
    Ops_CloseFile

IMPLICIT NONE

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_ReadRandomControlNL"
INTEGER                     :: unit

NAMELIST / RandomControlNL / meto_m,                &
                             meto_a,                &
                             meto_c,                &
                             initialise_with_clock, &
                             initializer,           &
                             junk_size,             &
                             random_number_method

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL Ops_OpenEnvRead ("OPS_RANDOMCONTROL_NL",           &
                      Unit,                             &
                      status_if_env_not_set = StatusOK)

IF (Unit >= 0) THEN
  READ (Unit, NML = RandomControlNL)
  IF (GeneralMode >= VerboseMode .AND. mype == 0) THEN
    WRITE (MessageOut, NML = RandomControlNL)
  END IF
  CALL Ops_CloseFile (Unit)
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ReadRandomControlNL
