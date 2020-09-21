!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Set up the integer header part of the varobs header.
!
! Inputs: Fortran90 structures containing information (i) observation data, (ii)
!         Cx header information, (iii) varobs header information, and (iv)
!         number of varobs to be written.
!
! Outputs: Fortran90 structure containing varobs header information, with
!          modified integer header values.
!
! Set up a number of variables defining observations, and assign to the integer
! constants part of the varobs header.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetupVarobsIntC (Observations,        &
                                CxHeader,            &
                                Varobs,              &
                                NumVarObsTotal,      &
                                varfield_array_size)

USE GenMod_Control, ONLY: &
    nproc

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI

USE GenMod_ModelIO, ONLY: &
    UM_header_type

USE GenMod_UMHeaderConstants, ONLY: &
    IC_PLevels,                     &
    IC_XLen,                        &
    IC_YLen,                        &
    IC_WetLevels,                   &
    IC_NumObsTotal,                 &
    IC_LenObCol,                    &
    IC_ObsGroup,                    &
    IC_ObsRelease,                  &
    IC_NumObVarMax,                 &
    IC_NumCxPItemMax,               &
    IC_NumCxSfVarMax,               &
    IC_NumCxUaVarMax,               &
    IC_NumMeta,                     &
    IC_NumItem,                     &
    IC_NumObVar,                    &
    IC_NumObPItem,                  &
    IC_NumCxPItem,                  &
    IC_NumCxSfVar,                  &
    IC_NumCxUaVar,                  &
    IC_NumObLev,                    &
    IC_NumVarBatches,               &
    IC_NumObPItemMax,               &
    IC_NumMetaMax,                  &
    IC_NumItemMax,                  &
    IC_ShipWind,                    &
    IC_ShipWind_10m,                &
    IC_GroundGPSOperator,           &
    IC_GPSRO_Operator_pseudo,       &
    IC_GPSRO_Operator_pseudo_choice,&
    IC_GPSRO_Operator_press,        &
    IC_GPSRO_Operator_press_choice

USE OpsMod_GroundGPS, ONLY: &
    GroundGPSOperator

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupSurface,           &
    ObsGroupGroundGPS,         &
    ObsGroupGPSRO

USE OpsMod_ObsInfo, ONLY: &
    OB_type

USE OpsMod_GPSROInfo_BA, ONLY: &
    GPSRO_pseudo_ops,          &
    GPSRO_vert_interp_ops

USE OpsMod_VarobsLib, ONLY: &
    varobs_type

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)               :: NumVarObsTotal
TYPE (UM_header_type), INTENT(IN) :: CxHeader
TYPE (OB_type), INTENT(IN)        :: Observations
TYPE (varobs_type), INTENT(INOUT) :: Varobs
INTEGER, INTENT(IN)               :: varfield_array_size

! Local declarations:
CHARACTER(len=*), PARAMETER       :: RoutineName = "Ops_SetupVarobsIntC"
INTEGER                           :: ObsRelease

!-------------------------------------------------------------------------------
! 0. Tracing
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
! 1. Initialise variables
!-------------------------------------------------------------------------------

ObsRelease = Observations % header % ObsRelease

!-------------------------------------------------------------------------------
! 2. Assign integer header values.
!-------------------------------------------------------------------------------

varobs % IntC(:) = IMDI

! values taken from Cx header
varobs % IntC(IC_XLen) = CxHeader % IntC(IC_XLen)
varobs % IntC(IC_YLen) = CxHeader % IntC(IC_Ylen)
varobs % IntC(IC_PLevels) = CxHeader % IntC(IC_PLevels)
varobs % IntC(IC_WetLevels) = CxHeader % IntC(IC_WetLevels)

IF (Observations % header % ObsGroup == ObsGroupSurface .AND. Observations % header % MarineWindAdjusted >= 1) THEN
  varobs % IntC(IC_ShipWind) = IC_ShipWind_10m
END IF

! Set varobs indicator for type of GroundGPS Operator chosen

IF (Observations % header % ObsGroup == ObsGroupGroundGPS) THEN
  varobs % IntC(IC_GroundGPSOperator) = GroundGPSOperator
END IF

! Set varobs indicator for type of GPSRO Operator options chosen

IF (Observations % header % ObsGroup == ObsGroupGPSRO .AND. GPSRO_pseudo_ops) THEN
  varobs % IntC(IC_GPSRO_Operator_pseudo) = IC_GPSRO_Operator_pseudo_choice
END IF

IF (Observations % header % ObsGroup == ObsGroupGPSRO .AND. GPSRO_vert_interp_ops) THEN
  varobs % IntC(IC_GPSRO_Operator_press) = IC_GPSRO_Operator_press_choice
END IF

! Number of obs in varobs file
varobs % IntC(IC_NumObsTotal) = NumVarObsTotal

! Length of ob column
varobs % IntC(IC_LenObCol) = LenObCol

! Observation group
varobs % IntC(IC_ObsGroup) = Observations % header % ObsGroup

! Version number
varobs % IntC(IC_ObsRelease) = ObsRelease

! Maximum no. of non met variables
varobs % IntC(IC_NumMetaMax) = NumMetaMax

! Maximum no. of data items per variable
varobs % IntC(IC_NumItemMax) = NumItemMax

! Maximum no. of variables in ob column
varobs % IntC(IC_NumObVarMax) = varfield_array_size

! Maximum no. of items defining uair pressure (ob)
varobs % IntC(IC_NumObPItemMax) = NumObPItemMax

! Maximum no. of items defining uair pressure (Cx)
varobs % IntC(IC_NumCxPItemMax) = NumCxPItemMax

! Maximum no. of surface variables in Cx column
varobs % IntC(IC_NumCxSfVarMax) = NumCxSfVarMax

! Maximum no. of uair variables in Cx column
varobs % IntC(IC_NumCxUaVarMax) = NumCxUaVarMax

! Actual no. of non met variables
varobs % IntC(IC_NumMeta) = NumMeta

! Actual no. of data items per variable
varobs % IntC(IC_NumItem) = NumItem

! Actual no. of variables in ob column
varobs % IntC(IC_NumObVar) = NumObVariables

! Actual no. of items defining uair pressure (ob)
varobs % IntC(IC_NumObPItem) = NumObPItem

! Actual no. of items defining uair pressure (Cx)
varobs % IntC(IC_NumCxPItem) = NumCxPItem

! Actual no. of surface variables in Cx column
varobs % IntC(IC_NumCxSfVar) = NumCxSfVar

! Actual no. of uair variables in Cx column
varobs % IntC(IC_NumCxUaVar) = NumCxUaVar

! Number of observation levels
varobs % IntC(IC_NumObLev) = NumObLev  ! Declared in module

! Number of batches of observations
varobs % IntC(IC_NumVarBatches) = nproc

!-------------------------------------------------------------------------------
! 3. Tracing and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetupVarobsIntC
