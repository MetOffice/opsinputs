!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Reads a varobs control namelist.
!
! Inputs: Obsgroup of the control file to read
!
! Outputs: The values in the namelist in that control file
!
! The relevant information is read using a NAMELIST read.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ReadVarobsControlNL (Obsgroup,      &
                                    Varfields_arg)

USE GenMod_Control, ONLY: &
  VerboseMode,            &
  GeneralMode,            &
  mype

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  MessageOut,          &
  StatusOK,            &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_IOUtils, ONLY: &
  Ops_CloseFile,          &
  Ops_OpenEnvRead,        &
  Ops_OpenFileRead

USE OpsMod_ObsGroupInfo, ONLY: &
  OpsFn_ObsGroupNumToName

USE OpsMod_Varfields, ONLY: &
  ActualMaxVarfield

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)         :: Obsgroup
INTEGER, INTENT(OUT)        :: Varfields_arg(:)

! Local declarations:
INTEGER                     :: Unit
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_ReadVarobsControlNL"
INTEGER                     :: Varfields(ActualMaxVarfield)
INTEGER                     :: Varfields_add(ActualMaxVarfield)
INTEGER                     :: Varfields_remove(ActualMaxVarfield)
INTEGER                     :: i
INTEGER                     :: array_loc(1)

NAMELIST / VarobsControlNL / &
           Varfields,        &
           Varfields_add,    &
           Varfields_remove

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! Initialize namelist variables where appropriate...

Varfields = IMDI
Varfields_arg = IMDI
Varfields_add = IMDI
Varfields_remove = IMDI

CALL Ops_OpenEnvRead ("OPS_VAROBSCONTROL_NL_DIR",                                    &
                      Unit,                                                          &
                      filename = TRIM (OpsFn_ObsGroupNumToName (ObsGroup)) // ".nl", &
                      status_if_env_not_set = StatusOK,                              &
                      status_if_open_failed = StatusOK)
IF (Unit <= 0) THEN
  CALL Ops_OpenFileRead ("ops_varobs_control/" // TRIM (OpsFn_ObsGroupNumToName (ObsGroup)) // ".nl", &
                         Unit,                                                                        &
                         status_if_open_failed = StatusOK)
END IF

IF (Unit > 0) THEN
  READ (Unit, NML = VarobsControlNL)
  CALL Ops_CloseFile (Unit)
END IF

IF (ALL (Varfields == IMDI)) THEN
  ! Namelist not there or doesn't set varfields, so use default list
  CALL Ops_GetDefaultVarfields (Obsgroup,      &
                                Varfields_arg)
ELSE
  ! Varfields have been set by namelists
  Varfields_arg(1:COUNT (Varfields /= IMDI)) = Varfields(1:COUNT (Varfields /= IMDI))
END IF

DO i = 1, COUNT (Varfields_add /= IMDI)
  IF (ALL (Varfields_arg /= Varfields_add(i))) THEN
    Varfields_arg(COUNT (Varfields_arg /= IMDI) + 1) = Varfields_add(i)
  END IF
END DO

DO i = 1, COUNT (Varfields_remove /= IMDI)
  IF (ANY (Varfields_arg == Varfields_remove(i))) THEN
    array_loc = MINLOC (ABS (VarFields_arg(1:COUNT (VarFields_arg /= IMDI)) - VarFields_remove(i)))
    Varfields_arg(array_loc(1):COUNT (Varfields_arg /= IMDI) - 1) = &
                              Varfields_arg(array_loc(1) + 1:COUNT (Varfields_arg /= IMDI))
    Varfields_arg(COUNT (Varfields_arg /= IMDI):) = IMDI
  END IF
END DO

IF (GeneralMode >= VerboseMode .AND. mype == 0) THEN
  WRITE (MessageOut, NML = VarobsControlNL)
  WRITE (MessageOut, '(A,20I4)') "Using varfields ", Varfields_arg(1:COUNT (Varfields_arg /= IMDI))
END IF

IF (ALL (Varfields_arg == IMDI)) THEN
  CALL gen_fail (RoutineName,                                                                     &
                 "No valid varfields selected for " // TRIM (OpsFn_ObsGroupNumToName (ObsGroup)))
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ReadVarobsControlNL
