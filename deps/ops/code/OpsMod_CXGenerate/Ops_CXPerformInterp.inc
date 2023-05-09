!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Called by Ops_CXGenerate to perform horizontal and time interpolation.  Each
! level of each field is read in from the dump in turn, at all forecast periods.
! The observations in each group which require each forecast time are
! interpolated. The two values for each observation are weighted and summed.
!
! If Radiosondes are processed and SondeDrift > 0 then slanted CX columns
! are produced for those reports with level dependent positions.
! As described in OSDP5 this requires a few iterations to find where the
! ascents intersect the model levels - represented by the p fields.
! This requires that the p fields appear in FieldsInfo before the other
! multi-level fields - as set up in Ops_CXSetupArrays.
! As coded it is also necessary that the p fields are ordered from bottom
! to top, this is implicitly assumed elsewhere in the routine as well.
!
! Inputs:
!   FieldsInfo: information about fields in UM dump, prepared in
!       Ops_CXSetupArrays
!   LocsInfo: information about the locations of the observations in
!       each observation group, prepared in Ops_PreproLocations
!   ForecastTimes: integer arrays giving forecast times.
!   ModelTimeUTC: Validity time UTC in the UM dump.
!
! Outputs:
!   CxArrays: data arrays in each structure are filled. These are shared
!       with the Cx structure, hence cx structure is filled.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CXPerformInterp (Obs,            &
                                domain,         &
                                FieldsInfo,     &
                                CXArrays,       &
                                LocsInfo,       &
                                ForecastTimes,  &
                                RequiredFields, &
                                BGECall,        &
                                ModelTimeUTC)

USE GenMod_CLookAdd, ONLY: &
  BDX,                     &
  BDY,                     &
  BZX,                     &
  BZY

USE GenMod_Constants, ONLY: &
  TwoPi

USE GenMod_Control, ONLY: &
  GeneralMode,            &
  VerboseMode,            &
  mype

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  MessageOut,          &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI,                                  &
  RMDI

USE GenMod_UMHeaderConstants, ONLY: &
  IC_XLen,                          &
  IC_YLen

USE OpsMod_Constants, ONLY: &
  deg2rad

USE OpsMod_Control, ONLY: &
  CycleUTC

USE OpsMod_DateTime, ONLY: &
  seconds_in_hour

USE OpsMod_FieldDataShared, ONLY: &
  FieldDataShared

USE OpsMod_FieldRead, ONLY: &
  Ops_GetUMFieldDims,       &
  Ops_ReadUMField

USE OpsMod_HorizontalInterp, ONLY: &
  interp_type_nearest,             &
  Ops_HorizontalInterp

USE OpsMod_Kinds, ONLY: &
  real32

USE OpsMod_ModelIO, ONLY: &
  DomainInfo_type,        &
  NetCDFFields_type,      &
  Ops_GetNetCDFField,     &
  Ops_InitNetCDFFields

USE OpsMod_MPLInterface, ONLY: &
  ops_mpl_barrier

USE OpsMod_ObsGroupInfo, ONLY: &
  ObsGroupSonde

USE OpsMod_ObsInfo, ONLY: &
  Ob_Type

USE OpsMod_SondeDrift, ONLY:  &
  SondeDrift,                 &
  SLevel

USE OpsMod_Stash, ONLY:      &
  StashCode_LapseRate,       &
  StashCode_PSurfParamA,     &
  StashCode_PSurfParamB,     &
  StashItem_p,               &
  StashItem_p_unslanted,     &
  StashItem_q,               &
  StashItem_q_unslanted,     &
  StashItem_theta,           &
  StashItem_theta_unslanted, &
  StashItem_u,               &
  StashItem_u_unslanted,     &
  StashCode_u10,             &
  StashItem_v,               &
  StashItem_v_unslanted,     &
  StashCode_v10

USE OpsMod_Timer, ONLY: &
  Ops_Timer

IMPLICIT NONE

! Subroutine arguments:
TYPE (Ob_Type), INTENT(IN)           :: Obs(:)    ! Only for Sonde drift
TYPE (DomainInfo_type), INTENT(IN)   :: domain
TYPE (FieldInfo_type), INTENT(INOUT) :: FieldsInfo(:)
TYPE (CXArray_type), INTENT(IN)      :: CXArrays(:,:)
TYPE (LocInfo_type), INTENT(INOUT)   :: LocsInfo(:)
INTEGER, INTENT(IN)                  :: ForecastTimes(:)
INTEGER, INTENT(IN)                  :: RequiredFields(:,:)
LOGICAL, INTENT(IN)                  :: BGECall
INTEGER, INTENT(IN)                  :: ModelTimeUTC(:)

! Local declarations:
INTEGER                              :: NumForecastTimes
INTEGER                              :: CentralForecastTime
INTEGER                              :: NumLevels
INTEGER                              :: NumObs
INTEGER                              :: iTime
INTEGER                              :: iField
INTEGER                              :: iDumpField
INTEGER                              :: iLevel
INTEGER                              :: iLevelCounter
INTEGER                              :: iSorted
INTEGER                              :: iGroup
INTEGER                              :: iOb
INTEGER                              :: jLev
INTEGER                              :: First     ! First observation which requires the present time
INTEGER                              :: New       ! First new observation which requires the present time
INTEGER                              :: Last      ! Last observation which requires the present time
INTEGER                              :: ForecastTime
LOGICAL                              :: ForecastMissing
INTEGER                              :: StashCode
INTEGER                              :: Niter
INTEGER                              :: iter
LOGICAL                              :: Slant
LOGICAL                              :: UseLevelTime
REAL                                 :: DLambda
REAL                                 :: DPhi
REAL                                 :: LambdaOrigin
REAL                                 :: PhiOrigin
REAL                                 :: x
REAL                                 :: Denom
REAL                                 :: TimeToSec
REAL                                 :: FieldTime
REAL                                 :: obTime1
REAL                                 :: obTime2
REAL                                 :: Weight
REAL(real32), POINTER                :: FieldData(:,:)
REAL(real32), ALLOCATABLE, TARGET    :: DumpFieldData(:,:)
REAL, ALLOCATABLE                    :: DataOut(:)
REAL, ALLOCATABLE                    :: STime(:)
REAL, ALLOCATABLE                    :: SLambda(:)
REAL, ALLOCATABLE                    :: SPhi(:)
REAL, POINTER                        :: Field(:)
REAL(real32), ALLOCATABLE            :: ParamA(:,:)
REAL(real32), ALLOCATABLE            :: ParamB(:,:)
REAL(real32), ALLOCATABLE            :: LapseRate(:,:) ! near surface lapse rate
INTEGER                              :: FieldDims(2)
INTEGER                              :: cx_interp_method_local
CHARACTER(len=*), PARAMETER          :: RoutineName = 'Ops_CXPerformInterp'
TYPE (NetCDFFields_type)             :: ncfields
INTEGER                              :: i
INTEGER                              :: j
INTEGER                              :: istat
INTEGER                              :: ShareComm

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL Ops_Timer (RoutineName)

CALL Ops_InitNetCDFFields (ncfields)

! Setup and copy the model state into the shared memory.
! Could read model earlier but would requires code refactor.
!--------------------------------------------------------------------------
! Dont use SharedMemory with background error files
IF (SharedMemory .AND. .NOT. BGECall) THEN
  CALL Ops_InitFieldDataShared (domain,                &
                                FieldsInfo,            &
                                CXArrays,              &
                                ForecastTimes,         &
                                ModelTimeUTC,          &
                                ReadOnAllPEs,          &
                                SharedMemoryTimeSlice, &
                                ncfields)
END IF

! Loop over forecast times, horizontally interpolating the required subset
! of observations.
!--------------------------------------------------------------------------

x = 0.0
NumForecastTimes = COUNT (ForecastTimes(:) /= IMDI)
IF (.NOT. UseValidityTimes) THEN
  CentralForecastTime = ForecastTimes(ISHFT (NumForecastTimes + 1,-1))
  TimeToSec = seconds_in_hour
ELSE
  CentralForecastTime = CycleUTC
  TimeToSec = 60.0
END IF

ShareComm = FieldDataShared % GetShareComm ()
DO iTime = 1, NumForecastTimes

  IF (SharedMemory .AND. .NOT. BGECall .AND. SharedMemoryTimeSlice) THEN

    ! Ensure all PE's have finished the previous read before starting the next read
    CALL ops_mpl_barrier (ShareComm, &
                          istat)

    CALL Ops_ReadModelShared (FieldDataShared, &
                              domain,          &
                              FieldsInfo,      &
                              CXArrays,        &
                              ForecastTimes,   &
                              ModelTimeUTC,    &
                              ncfields,        &
                              iTime)

    ! Ensure all fields have been read before starting interpolation
    CALL ops_mpl_barrier (ShareComm, &
                          istat)

  END IF

  iLevelCounter = 0

  FieldTime = (ForecastTimes(iTime) - CentralForecastTime) * TimeToSec
  IF (iTime == 1) THEN
    ! NB No check here on overall time window for Slant obs, done in Ops_SondeQcBackgr
    obTime1 = -HUGE (1.0)
    Denom = 1.0
  ELSE
    obTime1 = (ForecastTimes(iTime - 1) - CentralForecastTime) * TimeToSec
    Denom = 1.0 / ((ForecastTimes(iTime) - ForecastTimes(iTime - 1)) * TimeToSec)
  END IF
  IF (iTime < NumForecastTimes) THEN
    obTime2 = (ForecastTimes(iTime + 1) - CentralForecastTime) * TimeToSec
  ELSE
    obTime2 = HUGE (1.0)
  END IF

  DO iField = 1, SIZE (FieldsInfo)

    ! If the field for this forecast time is not available in the dump,
    ! find the field at the next available forecast time.
    CALL Ops_GetUMFieldAvailable (ForecastTime,            &
                                  ForecastMissing,         &
                                  FieldsInfo,              &
                                  iTime,                   &
                                  iField,                  &
                                  NumForecastTimes,        &
                                  iDumpField = iDumpField)
    IF (ForecastMissing) CYCLE

    StashCode = FieldsInfo(iField) % StashCode

    IF (ANY (cx_no_interpolation == StashCode)) THEN
      cx_interp_method_local = interp_type_nearest
    ELSE
      cx_interp_method_local = cx_interp_method
    END IF

    ! Work out field grid information from UM header. Reasonably assume that
    ! this is the same for different levels and different times.

    IF (.NOT. FieldsInfo(iField) % DerivedField) THEN
      IF (domain % variable_resolution) THEN ! Variable resolution grid

        ! Need STASH code to determine what grid this field is on
        SELECT CASE (StashCode)
          CASE (StashItem_u, StashCode_u10, StashItem_u_unslanted)
            PhiOrigin = domain % header(1) % RowDepC(1)
            LambdaOrigin = domain % header(1) % ColDepC(domain % header(1) % IntC(IC_XLen) + 1)
          CASE (StashItem_v, StashCode_v10, StashItem_v_unslanted)
            PhiOrigin = domain % header(1) % RowDepC(domain % header(1) % IntC(IC_YLen) + 1)
            LambdaOrigin = domain % header(1) % ColDepC(1)
          CASE DEFAULT
            PhiOrigin = domain % header(1) % RowDepC(1)
            LambdaOrigin = domain % header(1) % ColDepC(1)
        END SELECT

        DLambda = 0.0      ! undefined for variable grid
        DPhi = 0.0         ! -ditto-

      ELSE IF (StashCode >= 100000) THEN

        DPhi = (ncfields % netcdf_latitudes(2) - ncfields % netcdf_latitudes(1)) * deg2rad
        DLambda = (ncfields % netcdf_longitudes(2) - ncfields % netcdf_longitudes(1)) * deg2rad
        PhiOrigin = ncfields % netcdf_latitudes(1)
        LambdaOrigin = ncfields % netcdf_longitudes(1)

      ELSE       ! regular grid

        DLambda = TRANSFER (domain % header(1) % Lookup(BDX,iDumpField),x) * deg2rad
        DPhi = TRANSFER (domain % header(1) % Lookup(BDY,iDumpField),x) * deg2rad
        LambdaOrigin = TRANSFER (domain % header(1) % Lookup(BZX,iDumpField),x)
        PhiOrigin = TRANSFER (domain % header(1) % Lookup(BZY,iDumpField),x)

      END IF

      ! convert from degrees to radians
      LambdaOrigin = LambdaOrigin * deg2rad
      PhiOrigin = PhiOrigin * deg2rad

      IF (LambdaOrigin < 0.0 .AND. .NOT. domain % variable_resolution) THEN
        LambdaOrigin = LambdaOrigin + TwoPi  ! allow for meridian
      END IF

      ! Zeroth row/col in header, find first row/col, applicable to reg grid only
      LambdaOrigin = LambdaOrigin + DLambda
      PhiOrigin = PhiOrigin + DPhi

    END IF

    VertLevels: DO iLevel = 1, MAXVAL (CXArrays(:,iField) % NumLevels)
      ! Read in the present UM field for this level and time
      !------------------------------------------------------
      IF (GeneralMode >= VerboseMode .AND. mype == 0) THEN
        WRITE (MessageOut, '(A,L1,A,I0)') 'FieldInfo % DerivedField is ', &
          FieldsInfo(iField) % DerivedField, ' for stash code ', StashCode
      END IF

      ! Dont use SharedMemory with background error files
      IF (SharedMemory .AND. .NOT. BGECall) THEN
        ! Counter to locate the data in memory.
        iLevelCounter = iLevelCounter + 1
        FieldData => FieldDataShared % GetData(iField,iLevelCounter,iTime)
      ELSE IF (FieldsInfo(iField) % DerivedField) THEN
        IF (.NOT. ALLOCATED (ParamA)) THEN
          CALL Ops_DeriveField (domain % header(1),          &
                                ForecastTimes(iTime),        &
                                ModelTimeUTC,                &
                                ParamA,                      &
                                ParamB,                      &
                                LapseRate,                   &
                                ReadOnAllPEs = ReadOnAllPEs)
        END IF
        ALLOCATE (DumpFieldData(SIZE (ParamA(:,1)), SIZE (ParamA(1,:))))
        SELECT CASE (StashCode)
          CASE (StashCode_PSurfParamA)
            DumpFieldData(:,:) = ParamA(:,:)
          CASE (StashCode_PSurfParamB)
            DumpFieldData(:,:) = ParamB(:,:)
          CASE (StashCode_LapseRate)
            DumpFieldData(:,:) = LapseRate(:,:)
        END SELECT
        FieldData => DumpFieldData
      ELSE
        IF (StashCode < 100000) THEN
          IF (StashCode == StashItem_theta_unslanted) THEN
            CALL Ops_GetUMFieldDims (FieldDims,                          &
                                     domain % header(1),                 &
                                     StashItem_theta,                    &
                                     level = iLevel,                     &
                                     Time = ForecastTimes(ForecastTime), &
                                     ModelTimeUTC = ModelTimeUTC)
          ELSE IF (StashCode == StashItem_u_unslanted) THEN
            CALL Ops_GetUMFieldDims (FieldDims,                          &
                                     domain % header(1),                 &
                                     StashItem_u,                        &
                                     level = iLevel,                     &
                                     Time = ForecastTimes(ForecastTime), &
                                     ModelTimeUTC = ModelTimeUTC)
          ELSE IF (StashCode == StashItem_v_unslanted) THEN
            CALL Ops_GetUMFieldDims (FieldDims,                          &
                                     domain % header(1),                 &
                                     StashItem_v,                        &
                                     level = iLevel,                     &
                                     Time = ForecastTimes(ForecastTime), &
                                     ModelTimeUTC = ModelTimeUTC)
          ELSE IF (StashCode == StashItem_q_unslanted) THEN
            CALL Ops_GetUMFieldDims (FieldDims,                          &
                                     domain % header(1),                 &
                                     StashItem_q,                        &
                                     level = iLevel,                     &
                                     Time = ForecastTimes(ForecastTime), &
                                     ModelTimeUTC = ModelTimeUTC)
          ELSE IF (StashCode == StashItem_p_unslanted) THEN
            CALL Ops_GetUMFieldDims (FieldDims,                          &
                                     domain % header(1),                 &
                                     StashItem_p,                        &
                                     level = iLevel,                     &
                                     Time = ForecastTimes(ForecastTime), &
                                     ModelTimeUTC = ModelTimeUTC)
          ELSE
            CALL Ops_GetUMFieldDims (FieldDims,                          &
                                     domain % header(1),                 &
                                     StashCode,                          &
                                     level = iLevel,                     &
                                     Time = ForecastTimes(ForecastTime), &
                                     ModelTimeUTC = ModelTimeUTC)
          END IF
        ELSE
          FieldDims(1) = ncfields % netcdf_num_lats
          FieldDims(2) = ncfields % netcdf_num_lons
        END IF
        ALLOCATE (DumpFieldData(FieldDims(1), FieldDims(2)))

        IF (StashCode < 100000) THEN
          IF (StashCode == StashItem_theta_unslanted) THEN
            CALL Ops_ReadUMField (domain % header(1),                        &
                                  StashItem_theta,                           &
                                  DumpFieldData,                             &
                                  level = iLevel,                            &
                                  Time = ForecastTimes(ForecastTime),        &
                                  ModelTimeUTC = ModelTimeUTC,               &
                                  Vector = FieldsInfo(iField) % VectorField, &
                                  IntField = FieldsInfo(iField) % IntField,  &
                                  ReadOnAllPEs = ReadOnAllPEs)
          ELSE IF (StashCode == StashItem_u_unslanted) THEN
            CALL Ops_ReadUMField (domain % header(1),                        &
                                  StashItem_u,                               &
                                  DumpFieldData,                             &
                                  level = iLevel,                            &
                                  Time = ForecastTimes(ForecastTime),        &
                                  ModelTimeUTC = ModelTimeUTC,               &
                                  Vector = FieldsInfo(iField) % VectorField, &
                                  IntField = FieldsInfo(iField) % IntField,  &
                                  ReadOnAllPEs = ReadOnAllPEs)
          ELSE IF (StashCode == StashItem_v_unslanted) THEN
            CALL Ops_ReadUMField (domain % header(1),                        &
                                  StashItem_v,                               &
                                  DumpFieldData,                             &
                                  level = iLevel,                            &
                                  Time = ForecastTimes(ForecastTime),        &
                                  ModelTimeUTC = ModelTimeUTC,               &
                                  Vector = FieldsInfo(iField) % VectorField, &
                                  IntField = FieldsInfo(iField) % IntField,  &
                                  ReadOnAllPEs = ReadOnAllPEs)
          ELSE IF (StashCode == StashItem_q_unslanted) THEN
            CALL Ops_ReadUMField (domain % header(1),                        &
                                  StashItem_q,                               &
                                  DumpFieldData,                             &
                                  level = iLevel,                            &
                                  Time = ForecastTimes(ForecastTime),        &
                                  ModelTimeUTC = ModelTimeUTC,               &
                                  Vector = FieldsInfo(iField) % VectorField, &
                                  IntField = FieldsInfo(iField) % IntField,  &
                                  ReadOnAllPEs = ReadOnAllPEs)
          ELSE IF (StashCode == StashItem_p_unslanted) THEN
            CALL Ops_ReadUMField (domain % header(1),                        &
                                  StashItem_p,                               &
                                  DumpFieldData,                             &
                                  level = iLevel,                            &
                                  Time = ForecastTimes(ForecastTime),        &
                                  ModelTimeUTC = ModelTimeUTC,               &
                                  Vector = FieldsInfo(iField) % VectorField, &
                                  IntField = FieldsInfo(iField) % IntField,  &
                                  ReadOnAllPEs = ReadOnAllPEs)
          ELSE
            CALL Ops_ReadUMField (domain % header(1),                        &
                                  StashCode,                                 &
                                  DumpFieldData,                             &
                                  level = iLevel,                            &
                                  Time = ForecastTimes(ForecastTime),        &
                                  ModelTimeUTC = ModelTimeUTC,               &
                                  Vector = FieldsInfo(iField) % VectorField, &
                                  IntField = FieldsInfo(iField) % IntField,  &
                                  ReadOnAllPEs = ReadOnAllPEs)
          END IF
        ELSE IF (StashCode == 999998) THEN
          CALL Ops_GetNetCDFField (ncfields, &
                                   "cha")
          DO i = 1, SIZE (ncfields % netcdf_cha, DIM = 1)
            DO j = 1, SIZE (ncfields % netcdf_cha, DIM = 2)
              DumpFieldData(j,i) = ncfields % netcdf_cha(i,j,iTime)
            END DO
          END DO
        ELSE IF (StashCode == 999999) THEN
          CALL Ops_GetNetCDFField (ncfields, &
                                   "mss")
          DO i = 1, SIZE (ncfields % netcdf_mss, DIM = 1)
            DO j = 1, SIZE (ncfields % netcdf_mss, DIM = 2)
              DumpFieldData(j,i) = ncfields % netcdf_mss(i,j,iTime)
            END DO
          END DO
        END IF
        FieldData => DumpFieldData
      END IF

      ! Loop over observation groups, interpolating present field level for
      ! each.
      !-------------------------------------------------------------------------

      DO iGroup = 1, SIZE (LocsInfo)

        IF (ALL (RequiredFields(iGroup,:) /= StashCode)) CYCLE
        IF (iLevel > CXArrays(iGroup,iField) % NumLevels) CYCLE

        Slant = .NOT. BGECall .AND. SondeDrift > 0 .AND. &
                Obs(iGroup) % Header % ObsGroup == ObsGroupSonde .AND. &
                Obs(iGroup) % Header % PlevelsA % Present .AND. &
                Obs(iGroup) % Header % level_time % Present .AND. &
                Obs(iGroup) % Header % level_lat % Present .AND. &
                Obs(iGroup) % Header % level_lon % Present .AND. &
                StashCode /= StashItem_theta_unslanted .AND. &
                StashCode /= StashItem_u_unslanted .AND. &
                StashCode /= StashItem_v_unslanted .AND. &
                StashCode /= StashItem_q_unslanted .AND. &
                StashCode /= StashItem_p_unslanted
        UseLevelTime = Slant .AND. SondeDrift > 0
        IF (Slant .AND. StashCode == StashItem_p .AND. &
            .NOT. ASSOCIATED (LocsInfo(iGroup) % SPhi)) THEN
          NumLevels = CXArrays(iGroup,iField) % NumLevels
          NumObs = Obs(iGroup) % Header % NumObsLocal
          ALLOCATE (LocsInfo(iGroup) % SLambda(NumLevels,NumObs))
          ALLOCATE (LocsInfo(iGroup) % SPhi(NumLevels,NumObs))
          ALLOCATE (LocsInfo(iGroup) % STime(NumLevels,NumObs))
          ALLOCATE (LocsInfo(iGroup) % UseLevel(NumLevels,NumObs))
          ALLOCATE (SLevel(NumLevels,NumObs))
          DO jLev = 1, NumLevels
            LocsInfo(iGroup) % SLambda(jLev,:) = LocsInfo(iGroup) % Lambda(:)
            LocsInfo(iGroup) % SPhi(jLev,:) = LocsInfo(iGroup) % Phi(:)
          END DO
          LocsInfo(iGroup) % UseLevel(:,:) = .TRUE.
          SLevel(:,:) = 0
          Last = LocsInfo(iGroup) % LastInWindow(NumForecastTimes)
          DO iSorted = LocsInfo(iGroup) % FirstInWindow(0), Last
            iOb = LocsInfo(iGroup) % Index(iSorted)
            LocsInfo(iGroup) % STime(:,iSorted) = Obs(iGroup) % time(iOb)
          END DO
        END IF

        ! Perform horizontal interpolation
        !----------------------------------
        First = LocsInfo(iGroup) % FirstInWindow(iTime - 1)
        New = LocsInfo(iGroup) % FirstInWindow(iTime)
        IF (iTime == 1 .OR. iTime == NumForecastTimes) New  = First
        Last = LocsInfo(iGroup) % LastInWindow(iTime)
        IF (Slant .AND. iTime /= 1 .AND. UseLevelTime) THEN
          DO iSorted = LocsInfo(iGroup) % FirstInWindow(0), First - 1
            iOb = LocsInfo(iGroup) % Index(iSorted)
            IF (MAXVAL (Obs(iGroup) % level_time(iOb,:)) > obTime1) THEN
              First = iSorted
              EXIT
            END IF
          END DO
        END IF

        IF (First <= Last) THEN

          ALLOCATE (SPhi(First:Last))
          ALLOCATE (SLambda(First:Last))
          ALLOCATE (STime(First:Last))
          ALLOCATE (DataOut(First:Last))
          DO iSorted = First, Last
            iOb = LocsInfo(iGroup) % Index(iSorted)
            STime(iSorted) = Obs(iGroup) % time(iOb)
          END DO

          ! Slant latitude/longitude - initialise with station position
          SPhi(:) = LocsInfo(iGroup) % Phi(First:Last)
          SLambda(:) = LocsInfo(iGroup) % Lambda(First:Last)
          IF (Slant .AND. (StashCode == StashItem_p .OR. &
              StashCode == StashItem_u .OR. StashCode == StashItem_v)) THEN
            SPhi(:) = LocsInfo(iGroup) % SPhi(iLevel,First:Last)
            SLambda(:) = LocsInfo(iGroup) % SLambda(iLevel,First:Last)
            IF (UseLevelTime) &
              STime(:) = LocsInfo(iGroup) % STime(iLevel,First:Last)
          END IF
          IF (Slant .AND. &
              (StashCode == StashItem_q .OR. StashCode == StashItem_theta)) THEN
            ! For half-levels just take mid-point
            SPhi(:) = 0.5 * (LocsInfo(iGroup) % SPhi(iLevel,First:Last) + &
                             LocsInfo(iGroup) % SPhi(iLevel + 1,First:Last))
            SLambda(:) = 0.5 * (LocsInfo(iGroup) % SLambda(iLevel,First:Last) + &
                                LocsInfo(iGroup) % SLambda(iLevel + 1,First:Last))
            IF (UseLevelTime) THEN
              STime(:)  = 0.5 * (LocsInfo(iGroup) % STime(iLevel,First:Last) + &
                                 LocsInfo(iGroup) % STime(iLevel + 1,First:Last))
            END IF
            WHERE (ABS (LocsInfo(iGroup) % SLambda(iLevel + 1,First:Last) - &
                   LocsInfo(iGroup) % SLambda(iLevel,First:Last)) > 270.0 * deg2rad)
              SLambda(:) = SLambda(:) - 180.0 * deg2rad
            END WHERE
            WHERE (SLambda(:) < 0.0) SLambda(:) = SLambda(:) + 360.0 * deg2rad
          END IF

          Niter = 1
          IF (Slant .AND. StashCode == StashItem_p .AND. New <= Last .AND. &
              .NOT. Obs(iGroup) % header % ModelLevel) THEN
            Niter = 3
          END IF
          ! If Slant and pressure field then SPhi etc updated below for new obs

          DO iter = 1, Niter

            CALL Ops_HorizontalInterp (domain,                               &
                                       LambdaOrigin,                         &
                                       PhiOrigin,                            &
                                       DLambda,                              &
                                       DPhi,                                 &
                                       SLambda(:),                           &
                                       SPhi(:),                              &
                                       FieldData,                            &
                                       DataOut(:),                           &
                                       StashCode,                            &
                                       interp_type = cx_interp_method_local)

            IF (Slant .AND. StashCode == StashItem_p .AND. New <= Last) THEN
              CALL Ops_CXSlantPressure (LocsInfo(iGroup), &
                                        Obs(iGroup),      &
                                        domain,           &
                                        first,            &
                                        New,              &
                                        Last,             &
                                        DataOut,          &
                                        STime,            &
                                        SPhi,             &
                                        SLambda,          &
                                        iLevel,           &
                                        NumLevels,        &
                                        iter,             &
                                        Niter)
            END IF  ! (Slant .AND. StashCode == StashItem_p)
          END DO  ! iter

          ! Perform time interpolation, putting the observations back in order
          !--------------------------------------------------------------------

          IF (ASSOCIATED (CXArrays(iGroup,iField) % R1)) THEN
            Field => CXArrays(iGroup,iField) % R1(:)
          ELSE
            Field => CXArrays(iGroup,iField) % R2(:,iLevel)
          END IF

          DO iSorted = First, Last
            iOb = LocsInfo(iGroup) % Index(iSorted)
            IF (Slant .AND. ASSOCIATED (LocsInfo(iGroup) % UseLevel)) THEN
              ! Single level fields before pressure don't have UseLevel set up
              IF (STime(iSorted) <= obTime1 .OR. STime(iSorted) >= obTime2) THEN
                CYCLE ! This level already completed
              END IF
              IF (.NOT. LocsInfo(iGroup) % UseLevel(iLevel,iSorted)) THEN
                Field(iOb) = RMDI
                CYCLE ! This level is out of area
              END IF
            END IF
            IF (STime(iSorted) < FieldTime) THEN
              !  Obs in the time window before this forecast time.
              IF (Field(iOb) == RMDI) THEN
                Field(iOb) = DataOut(iSorted)
              ELSE IF (DataOut(iSorted) /= RMDI) THEN
                Weight = (FieldTime - STime(iSorted)) * Denom
                IF (ANY (cx_no_interpolation == StashCode)) THEN
                  IF (Weight < 0.5) THEN
                    Field(iOb) = DataOut(iSorted)
                  END IF
                ELSE
                  Field(iOb) = Field(iOb) * Weight + &
                               DataOut(iSorted) * (1.0 - Weight)
                END IF
              END IF
            ELSE
              !  Obs at exactly this forecast time or later
              Field(iOb) = DataOut(iSorted)
            END IF
          END DO

          DEALLOCATE (DataOut)
          DEALLOCATE (STime)
          DEALLOCATE (SLambda)
          DEALLOCATE (SPhi)

        END IF ! Last>First

      END DO

      IF (ALLOCATED (DumpFieldData)) DEALLOCATE (DumpFieldData)

    END DO VertLevels

  END DO ! iField

  IF (ALLOCATED (ParamA)) THEN  ! All three arrays (de)allocated together
    DEALLOCATE (ParamA)
    DEALLOCATE (ParamB)
    DEALLOCATE (LapseRate)
  END IF

END DO ! iTime

CALL Ops_Timer (RoutineName)
IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CXPerformInterp
