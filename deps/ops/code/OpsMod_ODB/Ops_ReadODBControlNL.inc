!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Reads an ODB control namelist.  Name of the ODB control namelist is taken from
! OPS_ODBCONTROL_NL.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ReadODBControlNL

USE GenMod_Control, ONLY: &
    mype,                 &
    GeneralMode,          &
    DiagnosticMode,       &
    VerboseMode

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    StatusOK,          &
    UseTrace

USE OpsMod_IOUtils, ONLY: &
    Ops_CloseFile,        &
    Ops_OpenEnvRead,      &
    Ops_OpenFileRead

USE OpsMod_ObsGroupInfo, ONLY: &
    OpsFn_ObsGroupNameToNum

USE OpsMod_ODBInfo, ONLY:          &
    fail_with_unrecognised_satids, &
    force_conv_body_creation,      &
    merge_to_existing_odb,         &
    num_batches_for_write,         &
    num_cloud_vars,                &
    number_of_pools_per_batch,     &
    store_seconds_of_60,           &
    update_table_number,           &
    use_all_pools,                 &
    verbose_odb

USE OpsMod_Process, ONLY: &
    VerRun

USE OpsMod_SatRad_SetUp, ONLY: &
    GeoCloudSrc,               &
    SrcObsGroupGeoCloud

IMPLICIT NONE

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_ReadODBControlNL"
INTEGER                     :: Unit

NAMELIST / ODBControlNL /        &
  fail_with_unrecognised_satids, &
  force_conv_body_creation,      &
  GeoCloudSrc,                   &
  merge_to_existing_odb,         &
  num_batches_for_write,         &
  num_cloud_vars,                &
  number_of_pools_per_batch,     &
  store_seconds_of_60,           &
  use_all_pools,                 &
  update_table_number,           &
  verbose_odb,                   &
  VerRun

!-----------------------------------------------------------------------------
! 0. Setup and tracing
!-----------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (GeneralMode >= VerboseMode) THEN
  verbose_odb = .TRUE.
END IF

!-----------------------------------------------------------------------------
! 1. Open the namelist
!-----------------------------------------------------------------------------

CALL Ops_OpenEnvRead ("OPS_ODBCONTROL_NL",              &
                      Unit,                             &
                      status_if_env_not_set = StatusOK)
IF (Unit <= 0) THEN
  CALL Ops_OpenFileRead ("ops_odb_control.nl",             &
                         Unit,                             &
                         status_if_open_failed = StatusOK)
END IF

IF (Unit > 0) THEN
  !-----------------------------------------------------------------------------
  ! 2. Read the namelist to obtain extraction info.
  !-----------------------------------------------------------------------------

  READ (Unit, NML = ODBControlNL)
  IF (GeneralMode >= DiagnosticMode .AND. mype == 0) THEN
    WRITE (MessageOut, NML = ODBControlNL)
  END IF

  CALL Ops_CloseFile (Unit)
END IF

SrcObsGroupGeocloud = OpsFn_ObsGroupNameToNum (GeoCloudSrc)

!-------------------------------------------------------------------------------
! 3. Error reporting, tracing, and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ReadODBControlNL
