!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! This routine performs some initial setup of the CX creation step.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CXSetup (Observations,  &
                        CXFiles,       &
                        UMHeader,      &
                        CxFields,      &
                        CxLevels_all,  &
                        ForecastTimes, &
                        ModelTimeUTC,  &
                        BGECall,       &
                        NumCxLevels,   &
                        ops_call)

USE GenMod_CLookAdd, ONLY: &
    LBDAT,                 &
    LBHR,                  &
    LBMIN,                 &
    LBMON,                 &
    LBYR

USE GenMod_Control, ONLY: &
    GeneralMode,          &
    mype,                 &
    VerboseMode

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    gen_warn,          &
    MessageOut,        &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI

USE GenMod_ModelIO, ONLY: &
    UM_header_type

USE GenMod_UMHeaderConstants, ONLY: &
    FH_ModelVersion,                &
    FH_SubModel,                    &
    IC_FirstConstantRhoLevel,       &
    IC_NumObsTotal,                 &
    IC_PLevels

USE OpsMod_Control, ONLY: &
    ModelType,            &
    ModelType_Ocean,      &
    ModelType_SST

USE OpsMod_CXInfo, ONLY: &
    Cx_type

USE OpsMod_DateTime, ONLY:   &
    OpsFn_DateTime_ToMinUTC

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupSeaIce

USE OpsMod_ObsInfo, ONLY: &
    OB_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_Type), INTENT(INOUT)        :: Observations(:)
TYPE (CX_type), INTENT(INOUT)        :: CXFiles(:)
TYPE (UM_Header_Type), INTENT(INOUT) :: UMheader
INTEGER, INTENT(INOUT)               :: CxFields(:,:)
INTEGER, INTENT(INOUT)               :: CxLevels_all(:)
INTEGER, ALLOCATABLE, INTENT(OUT)    :: ForecastTimes(:)
INTEGER, ALLOCATABLE, INTENT(OUT)    :: ModelTimeUTC(:)
LOGICAL, INTENT(IN)                  :: BGECall
INTEGER, OPTIONAL, INTENT(OUT)       :: NumCxLevels(:)
LOGICAL, OPTIONAL, INTENT(IN)        :: ops_call

! Local declarations:
CHARACTER(len=*), PARAMETER          :: RoutineName = "Ops_CXSetup"
INTEGER                              :: IGroup
INTEGER, ALLOCATABLE                 :: Itemp(:)
INTEGER                              :: iField
CHARACTER(len=80)                    :: ErrorMessage(2)
INTEGER                              :: NoForecastTimesUTC
INTEGER                              :: UTC1
INTEGER                              :: UTC2

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CxFields = IMDI

DO IGroup = 1, SIZE (Observations)

  CXFiles(iGroup) % header % ObsGroup = Observations(iGroup) % header % ObsGroup

  ! IF GRIB and OD then reallocate IntC and copy data across, including NumObsCx
  IF (Observations(IGroup) % Header % ObsGroup == ObsGroupSeaIce .AND. &
      SIZE (UMHeader % IntC) < IC_NumObsTotal) THEN
    ALLOCATE (Itemp(SIZE (UMHeader % IntC)))
    Itemp(:) = UMHeader % IntC(:)
    DEALLOCATE (UMHeader % IntC)
    ALLOCATE (UMHeader % IntC(IC_NumObsTotal))
    UMHeader % IntC = IMDI
    UMHeader % IntC(1:SIZE (Itemp)) = Itemp(:)
    DEALLOCATE (Itemp)
  END IF

  ! Num model levels in bgerr hdr is IntC(IC_Plevels):
  CxLevels = UMheader % IntC(IC_Plevels)

  CALL Ops_ReadCXControlNL (Observations(IGroup) % header % ObsGroup, &
                            CxFields(IGroup,:),                       &
                            BGECall,                                  &
                            ops_call)

  IF (.NOT. ALLOCATED (ForecastTimes)) THEN
    IF (.NOT. UseValidityTimes) THEN
      ALLOCATE (ModelTimeUTC(UMheader % NumFlds))
      ModelTimeUTC = IMDI
      IF (BGECall) THEN
        ALLOCATE (ForecastTimes(1))
        ForecastTimes(1) = BGE_ForecastPeriod
      ELSE
        ALLOCATE (ForecastTimes(SIZE (ForecastPeriods)))
        ForecastTimes(:) = ForecastPeriods(:)
      END IF
    ELSE
      ALLOCATE (ModelTimeUTC(UMheader % NumFlds))
      ModelTimeUTC = IMDI
      IF (BGECall) THEN
        ALLOCATE (ForecastTimes(1))
        ForecastTimes(1) = BGE_ForecastPeriod
      ELSE
        ALLOCATE (ForecastTimes(SIZE (ForecastPeriods)))
        ForecastTimes(:) = IMDI
        ModelTimeUTC = OpsFn_DateTime_ToMinUTC (UMheader % Lookup(LBYR,1:UMheader % NumFlds),  &
                                                UMheader % Lookup(LBMON,1:UMheader % NumFlds), &
                                                UMheader % Lookup(LBDAT,1:UMheader % NumFlds), &
                                                UMheader % Lookup(LBHR,1:UMheader % NumFlds),  &
                                                UMheader % Lookup(LBMIN,1:UMheader % NumFlds))
        ! Search time in lookup, and count number of different times
        NoForecastTimesUTC = 1
        UTC1 = ModelTimeUTC(1)
        ForecastTimes(1) = UTC1
        DO iField = 1, UMheader % NumFlds
          UTC2 = ModelTimeUTC(iField)
          IF (UTC2 /= UTC1 .AND. UTC2 /= IMDI) THEN
            NoForecastTimesUTC = NoForecastTimesUTC + 1
            ForecastTimes(NoForecastTimesUTC) = UTC2
            UTC1 = UTC2
            IF (SIZE (ForecastTimes) == NoForecastTimesUTC) EXIT
          END IF
        END DO
      END IF
    END IF
  END IF
  IF (PRESENT (NumCxLevels)) NumCxLevels(iGroup) = CxLevels
  Errormessage(:) = ' '
  IF (CxLevels > UMheader % IntC(IC_Plevels) .OR. CxLevels == IMDI) THEN
    CxLevels_all(IGroup) = UMheader % IntC(IC_Plevels)
    IF (CxLevels > UMheader % IntC(IC_Plevels)) THEN
      IF (BGECall) THEN
        Errormessage(1) = 'Number of bgerr levels specified by user'
      ELSE
        Errormessage(1) = 'Number of bg levels specified by user'
      END IF
      Errormessage(2) = 'is greater than the number in the file header'
      CALL gen_warn (RoutineName,       &
                     ErrorMessage(1:2))
    END IF
  ELSE
    CxLevels_all(IGroup) = CxLevels
  END IF
  IF (mype == 0 .AND. GeneralMode >=  VerboseMode) THEN
    WRITE (MessageOut, '(A, I0)') 'NumCxLEVELS = ',  CxLevels
    WRITE (MessageOut, '(A, 1000I8)') 'CxFields = ', CxFields(IGroup,:)
    WRITE (MessageOut, '(A, I0, A, I4)') 'CxLevels_all(', IGroup, &
                                         ')= ', CxLevels_all(IGroup)
  END IF

  CALL CXFiles(IGroup) % init

  CXFiles(IGroup) % Header % NumLocal = Observations(IGroup) % Header % NumObsLocal
  CXFiles(IGroup) % Header % NumTotal = Observations(IGroup) % Header % NumObsTotal
  CXFiles(IGroup) % Header % ModelVersion = UMheader % FixHd(FH_ModelVersion)
  CXFiles(IGroup) % Header % SubModel = UMheader % FixHd(FH_SubModel)
  CXFiles(IGroup) % Header % NewDynamics = CXFiles(IGroup) % Header % ModelVersion >= 500

  IF (ModelType == ModelType_Ocean) THEN
    CXFiles(IGroup) % Header % NewDynamics = .FALSE.
  END IF

  IF (CXFiles(IGroup) % Header % NewDynamics) THEN
    IF (.NOT. BGECall .AND. ModelType /= ModelType_SST) THEN
      CXFiles(IGroup) % Header % FirstConstantRhoLevel = UMheader % IntC(IC_FirstConstantRhoLevel)
    END IF
  ELSE
    CXFiles(IGroup) % Header % FirstConstantRhoLevel = IMDI
  END IF
END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CXSetup
