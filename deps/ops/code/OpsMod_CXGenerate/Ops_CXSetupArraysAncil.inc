!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Called by Ops_CXGenerate to set up both the FieldsInfo and CxArrays arrays.
! Also allocates arrays in the Cx structure.
!
! The arrays to be filled within the CX structure are allocated, and an array of
! structures (CXArray_Type) is prepared which points to each field in the cx
! structure.  Also, the positions and number of levels of each field within the
! UM dump is determined.
!
! Inputs:
!     UMHdr: The header from the UM dump, required for field information.
!     ForecastTimes: The forecast times required for time interpolation.
!     Obs: Array of structures containing observations for each obsgroup,
!       required for observation locations.
!     RequiredFields: The stashcodes required for each group (Group,Field)
!     NumLevelsRequired: The number of levels required for each field
!       (Group,Field)
!     ModelTimeUTC: Validity time UTC in the UM dump.
!
! Outputs:
!     CXs: Array of cx structures for each obsgroup, with required arrays
!       allocated.
!     FieldsInfo:
!     CxArrays: Array of structures
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CXSetupArraysAncil (UMHdr,             &
                                   ForecastTimes,     &
                                   Obs,               &
                                   RequiredFields,    &
                                   NumLevelsRequired, &
                                   CXs,               &
                                   CXArrays,          &
                                   FieldsInfo,        &
                                   ModelTimeUTC)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE GenMod_ModelIO, ONLY: &
  UM_Header_Type

USE OpsMod_Ancil

USE OpsMod_CXInfo, ONLY: &
  Cx_Type

USE OpsMod_MiscTypes, ONLY: &
  ZCodeSurface,             &
  ZCodeLevelsA,             &
  ZCodeLevelsB

USE OpsMod_ObsInfo, ONLY: &
  Ob_Type

IMPLICIT NONE

! Subroutine arguments:
TYPE (UM_Header_Type), INTENT(IN)     :: UMHdr
INTEGER, INTENT(IN)                   :: ForecastTimes(:)
TYPE (Ob_Type), INTENT(IN)            :: Obs(:)
INTEGER, INTENT(IN)                   :: RequiredFields(:,:)
INTEGER, INTENT(IN)                   :: NumLevelsRequired(:)
TYPE (Cx_Type), TARGET, INTENT(INOUT) :: Cxs(:)
TYPE (CXArray_type), INTENT(INOUT)    :: CXArrays(:,:)
TYPE (FieldInfo_Type), INTENT(INOUT)  :: FieldsInfo(:)
INTEGER, INTENT(IN)                   :: ModelTimeUTC(:)

! Local declarations:
TYPE (Cx_Type), POINTER               :: Cx
TYPE (Cx_Type), TARGET                :: CxDummy
INTEGER                               :: iField
INTEGER                               :: ZCode
INTEGER                               :: NumForecastTimes
INTEGER                               :: iGroup
CHARACTER(len=*), PARAMETER           :: RoutineName = 'Ops_CXSetupArraysAncil'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

NumForecastTimes = COUNT (ForecastTimes(:) /= IMDI)

CALL CxDummy % init

DO iGroup = -1, SIZE (Obs)

  IF (iGroup == -1) THEN
    ! Pass to count number of fields required
    Cx => CxDummy
  ELSE IF (iGroup > 0) THEN
    ! passes to set up CX array info
    CX => Cxs(iGroup)
  END IF

  iField = 0

  ! Single level arrays
  !---------------------

  ZCode = ZCodeSurface

  CALL Ops_CXAddField (AncilItem_SST,     &
                       Cx % Header % SST, &
                       R1 = CX % SST)
  CALL Ops_CXAddField (AncilItem_SeaIce,     &
                       Cx % Header % SeaIce, &
                       R1 = CX % SeaIce)
  CALL Ops_CXAddField (AncilCode_SeaHeight,     &
                       Cx % Header % SeaHeight, &
                       R1 = CX % SeaHeight)
  CALL Ops_CXAddField (AncilCode_WaveHeight,     &
                       Cx % Header % WaveHeight, &
                       R1 = CX % WaveHeight)
  CALL Ops_CXAddField (AncilCode_SeaSrfcHeight,     &
                       Cx % Header % SeaSrfcHeight, &
                       R1 = CX % SeaSrfcHeight)
  CALL Ops_CXAddField (AncilCode_MeanSeaHeight,     &
                       Cx % Header % MeanSeaHeight, &
                       R1 = CX % MeanSeaHeight)
  CALL Ops_CXAddField (AncilCode_SeaIce,     &
                       Cx % Header % SeaIce, &
                       R1 = CX % SeaIce)
  CALL Ops_CXAddField (AncilCode_CHL,     &
                       Cx % Header % CHL, &
                       R1 = CX % CHL)
  CALL Ops_CXAddField (AncilCode_KD490,     &
                       Cx % Header % KD490, &
                       R1 = CX % KD490)
  CALL Ops_CXAddField (AncilCode_LCHL,     &
                       Cx % Header % LCHL, &
                       R1 = CX % LCHL)
  CALL Ops_CXAddField (AncilCode_LKD490,     &
                       Cx % Header % LKD490, &
                       R1 = CX % LKD490)
  CALL Ops_CXAddField (AncilCode_CHLOb_Err,     &
                       Cx % Header % CHLOb_Err, &
                       R1 = CX % CHLOb_Err)
  CALL Ops_CXAddField (AncilCode_KD490Ob_Err,     &
                       Cx % Header % KD490Ob_Err, &
                       R1 = CX % KD490Ob_Err)
  CALL Ops_CXAddField (AncilCode_LCHLOb_Err,     &
                       Cx % Header % LCHLOb_Err, &
                       R1 = CX % LCHLOb_Err)
  CALL Ops_CXAddField (AncilCode_LKD490Ob_Err,     &
                       Cx % Header % LKD490Ob_Err, &
                       R1 = CX % LKD490Ob_Err)

  ! Extra single level Fields for OceanSound
  CALL Ops_CXAddField (AncilCode_SSTMes_Var,     &
                       Cx % Header % SSTMes_Var, &
                       R1 = CX % SSTMes_Var)
  CALL Ops_CXAddField (AncilCode_SSTMes_HCor,     &
                       Cx % Header % SSTMes_HCor, &
                       R1 = CX % SSTMes_HCor)
  CALL Ops_CXAddField (AncilCode_SSTSyn_Var,     &
                       Cx % Header % SSTSyn_Var, &
                       R1 = CX % SSTSyn_Var)
  CALL Ops_CXAddField (AncilCode_SSTSyn_HCor,     &
                       Cx % Header % SSTSyn_HCor, &
                       R1 = CX % SSTSyn_HCor)
  CALL Ops_CXAddField (AncilCode_SSTOb_Err,     &
                       Cx % Header % SSTOb_Err, &
                       R1 = CX % SSTOb_Err)
  CALL Ops_CXAddField (AncilCode_SSHMes_Var,     &
                       Cx % Header % SSHMes_Var, &
                       R1 = CX % SSHMes_Var)
  CALL Ops_CXAddField (AncilCode_SSHMes_HCor,     &
                       Cx % Header % SSHMes_HCor, &
                       R1 = CX % SSHMes_HCor)
  CALL Ops_CXAddField (AncilCode_SSHSyn_Var,     &
                       Cx % Header % SSHSyn_Var, &
                       R1 = CX % SSHSyn_Var)
  CALL Ops_CXAddField (AncilCode_SSHSyn_HCor,     &
                       Cx % Header % SSHSyn_HCor, &
                       R1 = CX % SSHSyn_HCor)
  CALL Ops_CXAddField (AncilCode_SSHOb_Err,     &
                       Cx % Header % SSHOb_Err, &
                       R1 = CX % SSHOb_Err)
  CALL Ops_CXAddField (AncilCode_SeaIceMes_Var,     &
                       Cx % Header % SeaIceMes_Var, &
                       R1 = CX % SeaIceMes_Var)
  CALL Ops_CXAddField (AncilCode_SeaIceMes_HCor,     &
                       Cx % Header % SeaIceMes_HCor, &
                       R1 = CX % SeaIceMes_HCor)
  CALL Ops_CXAddField (AncilCode_SeaIceSyn_Var,     &
                       Cx % Header % SeaIceSyn_Var, &
                       R1 = CX % SeaIceSyn_Var)
  CALL Ops_CXAddField (AncilCode_SeaIceSyn_HCor,     &
                       Cx % Header % SeaIceSyn_HCor, &
                       R1 = CX % SeaIceSyn_HCor)
  CALL Ops_CXAddField (AncilCode_SeaIceOb_Err,     &
                       Cx % Header % SeaIceOb_Err, &
                       R1 = CX % SeaIceOb_Err)

  ! Multiple level arrays, on levelsA (rho levels) in new dynamics
  !----------------------------------------------------------------
  ZCode = ZCodeLevelsA
  ! Extra level for P? May not be a problem, I don't know...
  ! SondeDrift in CX interpolation requires p before other multi-level fields

  CALL Ops_CXAddField (AncilCode_salt,     &
                       Cx % Header % salt, &
                       R2 = CX % salt)

  ! Extra multi level Fields for OceanSound
  CALL Ops_CXAddField (AncilCode_TempMes_Var,     &
                       Cx % Header % TempMes_Var, &
                       R2 = CX % TempMes_Var)
  CALL Ops_CXAddField (AncilCode_TempMes_HCor,     &
                       Cx % Header % TempMes_HCor, &
                       R2 = CX % TempMes_HCor)
  CALL Ops_CXAddField (AncilCode_TempSyn_Var,     &
                       Cx % Header % TempSyn_Var, &
                       R2 = CX % TempSyn_Var)
  CALL Ops_CXAddField (AncilCode_TempSyn_HCor,     &
                       Cx % Header % TempSyn_HCor, &
                       R2 = CX % TempSyn_HCor)
  CALL Ops_CXAddField (AncilCode_TempOb_Err,     &
                       Cx % Header % TempOb_Err, &
                       R2 = CX % TempOb_Err)
  CALL Ops_CXAddField (AncilCode_SalMes_Var,     &
                       Cx % Header % SalMes_Var, &
                       R2 = CX % SalMes_Var)
  CALL Ops_CXAddField (AncilCode_SalMes_HCor,     &
                       Cx % Header % SalMes_HCor, &
                       R2 = CX % SalMes_HCor)
  CALL Ops_CXAddField (AncilCode_SalSyn_Var,     &
                       Cx % Header % SalSyn_Var, &
                       R2 = CX % SalSyn_Var)
  CALL Ops_CXAddField (AncilCode_SalSyn_HCor,     &
                       Cx % Header % SalSyn_HCor, &
                       R2 = CX % SalSyn_HCor)
  CALL Ops_CXAddField (AncilCode_SalOb_Err,     &
                       Cx % Header % SalOb_Err, &
                       R2 = CX % SalOb_Err)

  ! Multiple level arrays, on levelsB (theta levels) in new dynamics
  !------------------------------------------------------------------
  IF (iGroup > 0) THEN
    IF (Cx % Header % NewDynamics) ZCode = ZCodeLevelsB
  END IF

  CALL Ops_CXAddField (AncilCode_theta_Ocean, &
                       Cx % Header % Theta,   &
                       R2 = CX % Theta)

END DO

CALL CxDummy % deallocate

IF (UseTrace) CALL gen_trace_exit (RoutineName)

CONTAINS

INCLUDE 'Ops_CXAddField.inc'

END SUBROUTINE Ops_CXSetupArraysAncil
