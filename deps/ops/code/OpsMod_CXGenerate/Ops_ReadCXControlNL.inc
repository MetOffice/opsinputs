!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Reads a CX control file NAMELIST.
!
! Inputs:
!     ObsGroup - obsgroup whose namelist is to be read
!     BGECall - whether background or background error fields are to be read
!
! Outputs:
!     CxFields_arg - List of CX fields
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ReadCXControlNL (Obsgroup,     &
                                CxFields_arg, &
                                BGECall,      &
                                ops_call)

USE GenMod_Control, ONLY: &
    DiagnosticMode,       &
    GeneralMode,          &
    mype,                 &
    VerboseMode

USE GenMod_Core, ONLY: &
    gen_fail,          &
    gen_message,       &
    gen_trace_entry,   &
    gen_trace_exit,    &
    gen_warn,          &
    MessageOut,        &
    StatusOK,          &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI

USE OpsMod_EnvUtils, ONLY: &
    ops_env_is_set

USE OpsMod_IOUtils, ONLY: &
    Ops_CloseFile,        &
    Ops_OpenEnvRead,      &
    Ops_OpenFileRead

USE OpsMod_ObsGroupInfo, ONLY: &
    OpsFn_ObsGroupNumToName

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)           :: Obsgroup
INTEGER, INTENT(OUT)          :: CxFields_arg(:)
LOGICAL, INTENT(IN)           :: BGECall
LOGICAL, OPTIONAL, INTENT(IN) :: ops_call

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_ReadCXControlNL"
CHARACTER(len=100)            :: ErrorMessage
INTEGER                       :: Unit
INTEGER                       :: CxFields(MaxModelCodes)
INTEGER                       :: CxFields_add(MaxModelCodes)
INTEGER                       :: CxFields_remove(MaxModelCodes)
INTEGER                       :: CxFields_ops(MaxModelCodes)
INTEGER                       :: CxFields_add_ops(MaxModelCodes)
INTEGER                       :: CxFields_remove_ops(MaxModelCodes)
INTEGER                       :: CxFields_var(MaxModelCodes)
INTEGER                       :: CxFields_add_var(MaxModelCodes)
INTEGER                       :: CxFields_remove_var(MaxModelCodes)
INTEGER                       :: i
INTEGER                       :: array_loc(1)
LOGICAL                       :: local_ops_call

NAMELIST / CXControlNL / &
   CxLevels,             &
   CxFields,             &
   CxFields_add,         &
   CxFields_remove,      &
   CxFields_ops,         &
   CxFields_add_ops,     &
   CxFields_remove_ops,  &
   CxFields_var,         &
   CxFields_add_var,     &
   CxFields_remove_var

!-----------------------------------------------------------------------------
! 0. Setup and tracing
!-----------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (PRESENT (ops_call)) THEN
  local_ops_call = ops_call
ELSE
  local_ops_call = .TRUE.
END IF

!-----------------------------------------------------------------------------
! 1. Try to open the namelist
!-----------------------------------------------------------------------------

CxFields_arg = IMDI
CxFields = IMDI
CxFields_add = IMDI
CxFields_remove = IMDI
CxFields_ops = IMDI
CxFields_add_ops = IMDI
CxFields_remove_ops = IMDI
CxFields_var = IMDI
CxFields_add_var = IMDI
CxFields_remove_var = IMDI

IF (BGECall) THEN
  CALL Ops_OpenEnvRead ("OPS_CXBGERR_CONTROL_NL_DIR",                                  &
                        Unit,                                                          &
                        filename = TRIM (OpsFn_ObsGroupNumToName (ObsGroup)) // ".nl", &
                        status_if_env_not_set = StatusOK,                              &
                        status_if_open_failed = StatusOK)
  IF (Unit <= 0) THEN
    CALL Ops_OpenFileRead ("ops_cxbgerr_control/" // TRIM (OpsFn_ObsGroupNumToName (ObsGroup)) // ".nl", &
                           Unit,                                                                         &
                           status_if_open_failed = StatusOK)
  END IF
ELSE
  CALL Ops_OpenEnvRead ("OPS_CX_CONTROL_NL_DIR",                                       &
                        Unit,                                                          &
                        filename = TRIM (OpsFn_ObsGroupNumToName (ObsGroup)) // ".nl", &
                        status_if_env_not_set = StatusOK,                              &
                        status_if_open_failed = StatusOK)
  IF (Unit <= 0) THEN
    CALL Ops_OpenFileRead ("ops_cx_control/" // TRIM (OpsFn_ObsGroupNumToName (ObsGroup)) // ".nl", &
                           Unit,                                                                    &
                           status_if_open_failed = StatusOK)
  END IF
END IF

!-----------------------------------------------------------------------------
! 2. Read the namelist
!-----------------------------------------------------------------------------

IF (Unit <= 0) THEN
  IF (GeneralMode >= DiagnosticMode) THEN
    IF (BGECall) THEN
      ErrorMessage = "CX bgerr namelist not specified for " // &
                           TRIM (OpsFn_ObsGroupNumToName (ObsGroup)) // ", using defaults"
    ELSE
      ErrorMessage = "CX namelist not specified for " // &
                           TRIM (OpsFn_ObsGroupNumToName (ObsGroup)) // ", using defaults"
    END IF
    CALL gen_message (RoutineName,  &
                      ErrorMessage)
  END IF
ELSE
  READ (Unit, NML = CXControlNL)
  CALL Ops_CloseFile (Unit)
END IF

IF (local_ops_call .AND. ANY (CxFields_ops /= IMDI)) THEN
  CxFields = CxFields_ops
ELSE IF ((.NOT. local_ops_call) .AND. ANY (CxFields_var /= IMDI)) THEN
  CxFields = CxFields_var
END IF

IF (ALL (CxFields == IMDI)) THEN
  ! Namelist not there or doesn't set CX fields, so use default list
  IF (BGECall) THEN
    CALL Ops_GetDefaultBgerrFields (Obsgroup,     &
                                    CxFields_arg)
  ELSE
    CALL Ops_GetDefaultCxFields (Obsgroup,     &
                                 CxFields_arg)
  END IF
ELSE
  ! CX fields have been set by namelists
  CxFields_arg(1:COUNT (CxFields /= IMDI)) = CxFields(1:COUNT (CxFields /= IMDI))
END IF

IF (local_ops_call .AND. ANY (CxFields_add_ops /= IMDI)) THEN
  CxFields_add = CxFields_add_ops
ELSE IF ((.NOT. local_ops_call) .AND. ANY (CxFields_add_var /= IMDI)) THEN
  CxFields_add = CxFields_add_var
END IF

IF (local_ops_call .AND. ANY (CxFields_remove_ops /= IMDI)) THEN
  CxFields_remove = CxFields_remove_ops
ELSE IF ((.NOT. local_ops_call) .AND. ANY (CxFields_remove_var /= IMDI)) THEN
  CxFields_remove = CxFields_remove_var
END IF

DO i = 1, COUNT (CxFields_add /= IMDI)
  IF (ALL (CxFields_arg /= CxFields_add(i))) THEN
    CxFields_arg(COUNT (CxFields_arg /= IMDI) + 1) = CxFields_add(i)
  END IF
END DO

DO i = 1, COUNT (CxFields_remove /= IMDI)
  IF (ANY (CxFields_arg == CxFields_remove(i))) THEN
    array_loc = MINLOC (ABS (CxFields_arg(1:COUNT (CxFields_arg /= IMDI)) - CxFields_remove(i)))
    CxFields_arg(array_loc(1):COUNT (CxFields_arg /= IMDI) - 1) = &
                              CxFields_arg(array_loc(1) + 1:COUNT (CxFields_arg /= IMDI))
    CxFields_arg(COUNT (CxFields_arg /= IMDI):) = IMDI
  END IF
END DO

IF (GeneralMode >= VerboseMode .AND. mype == 0) THEN
  WRITE (MessageOut, NML = CXControlNL)
  IF (BGECall) THEN
    WRITE (MessageOut, '(A,200I6)') "Using bgerr fields ", CxFields_arg(1:COUNT (CxFields_arg /= IMDI))
  ELSE
    WRITE (MessageOut, '(A,200I6)') "Using CX fields ", CxFields_arg(1:COUNT (CxFields_arg /= IMDI))
  END IF
END IF

IF (ALL (CxFields_arg == IMDI)) THEN
  IF (BGECall) THEN
    CALL gen_warn (RoutineName,                      &
                   "No valid bgerr fields selected")
  ELSE
    CALL gen_fail (RoutineName,                   &
                   "No valid cx fields selected")
  END IF
END IF

IF (CxLevels <= 0) THEN
  WRITE (ErrorMessage, '(A,I0,A)') "CxLevels value ", CxLevels, " invalid, must be greater than 0"
  CALL gen_fail (RoutineName,  &
                 ErrorMessage)
END IF

IF (ops_env_is_set ("OPS_UMBACK_NETCDF") .AND. .NOT. BGECall) THEN
  cxfields_arg(COUNT (cxfields_arg /= IMDI) + 1) = 999998
  cxfields_arg(COUNT (cxfields_arg /= IMDI) + 1) = 999999
END IF

!-------------------------------------------------------------------------------
! 2. Tracing, and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ReadCXControlNL
