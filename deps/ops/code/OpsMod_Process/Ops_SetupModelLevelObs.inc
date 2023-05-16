!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! General setup for a model level ob_type structure based on a reported level
! structure.  Sets up the header and some common elements.  This should be
! called initially by routines that set up obs group specific model level
! ob_type structures.
!
! Inputs: RepObs - Reported level observations data
!         BGCx - Background CX data
!
! Outputs:  ModObs - Model level observations data
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetupModelLevelObs (RepObs, &
                                   BGCx,   &
                                   ModObs)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE OpsMod_CXInfo, ONLY: &
  Cx_type

USE OpsMod_ObsInfo, ONLY: &
  OB_type,                &
  ops_alloc,              &
  Ops_SetupObType

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(IN)  :: RepObs ! Obs data on report levels
TYPE (Cx_type), INTENT(IN)  :: BGCx   ! Structure of background columns
TYPE (OB_type), INTENT(OUT) :: ModObs ! Obs data on model levels

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = 'Ops_SetupModelLevelObs'

!--------------------------------------
! 0. Trace and Initialise constants
!--------------------------------------
IF (UseTrace) CALL gen_trace_entry (RoutineName)

!------------------
! 1. Set up DPIs
!------------------
CALL Ops_SetupObType (ModObs)

ModObs % header % ModelLevel = .TRUE.
ModObs % header % ValidityTime = RepObs % header % ValidityTime
ModObs % header % NumCxBatches = RepObs % header % NumCxBatches
ModObs % header % NumObsLocal = RepObs % header % NumObsLocal
ModObs % header % NumObsTotal = RepObs % header % NumObsTotal
ModObs % header % ObsGroup = RepObs % header % ObsGroup
ModObs % header % ObsGroupName = RepObs % header % ObsGroupName
ModObs % header % ModelForecastLength = RepObs % header % ModelForecastLength
IF (ASSOCIATED (RepObs % header % ObsPerBatchPerPE)) THEN
  ALLOCATE (ModObs % header % ObsPerBatchPerPE(SIZE (RepObs % header % ObsPerBatchPerPE, DIM = 1), &
                                               0:SIZE (RepObs % header % ObsPerBatchPerPE, DIM = 2) - 1))
  ModObs % header % ObsPerBatchPerPE = RepObs % header % ObsPerBatchPerPE
END IF

IF (ASSOCIATED (RepObs % header % ObsPerPE)) THEN
  ALLOCATE (ModObs % header % ObsPerPE(0:SIZE (RepObs % header % ObsPerPE) - 1))
  ModObs % header % ObsPerPE = RepObs % header % ObsPerPE
END IF

IF (ASSOCIATED (RepObs % header % odb_query_info)) THEN
  ALLOCATE (ModObs % header % odb_query_info(SIZE (RepObs % header % odb_query_info)))
  ModObs % header % odb_query_info = RepObs % header % odb_query_info
END IF

!-------------------
! 2. Pressure levels
!-------------------

IF (BGCx % header % PlevelsA % Present) THEN
  CALL ops_alloc (ModObs % header % PlevelsA,       &
                  "ModObs % PLevelsA",              &
                  RepObs % header % NumObsLocal,    &
                  ModObs % PLevelsA,                &
                  HdrIn = BGCx % header % PlevelsA)
  IF (RepObs % header % NumObsLocal > 0) THEN
    ModObs % PlevelsA % Value = BGCx % PlevelsA
  END IF
END IF

IF (BGCx % header % PlevelsB % Present) THEN
  CALL ops_alloc (ModObs % header % PlevelsB,       &
                  "ModObs % PLevelsB",              &
                  RepObs % header % NumObsLocal,    &
                  ModObs % PLevelsB,                &
                  HdrIn = BGCx % header % PlevelsB)
  IF (RepObs % header % NumObsLocal > 0) THEN
    ModObs % PlevelsB % Value = BGCx % PlevelsB
  END IF
END IF

!------------
! 3. MetaData
!------------

IF (RepObs % header % ReportPGE % Present) THEN
  CALL ops_alloc (ModObs % header % ReportPGE,         &
                  "ModObs % ReportPGE",                &
                  RepObs % header % NumObsLocal,       &
                  ModObs % ReportPGE,                  &
                  HdrIn = RepObs % header % ReportPGE)
  IF (RepObs % header % NumObsLocal > 0) THEN
    ModObs % ReportPGE = RepObs % ReportPGE
  END IF
END IF

IF (RepObs % header % ReportFlags % Present) THEN
  CALL ops_alloc (ModObs % header % ReportFlags,         &
                  "ModObs % ReportFlags",                &
                  RepObs % header % NumObsLocal,         &
                  ModObs % ReportFlags,                  &
                  HdrIn = RepObs % header % ReportFlags)
  IF (RepObs % header % NumObsLocal > 0) THEN
    ModObs % ReportFlags = RepObs % ReportFlags
  END IF
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetupModelLevelObs
