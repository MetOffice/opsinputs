!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Read the data into the shared memory.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ReadModelShared (FieldDataShared, &
                                domain,          &
                                FieldsInfo,      &
                                CXArrays,        &
                                ForecastTimes,   &
                                ModelTimeUTC,    &
                                ncfields,        &
                                iTime)

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

USE OpsMod_FieldRead, ONLY: &
  Ops_ReadUMField

USE OpsMod_ModelIO, ONLY: &
  DomainInfo_type,        &
  NetCDFFields_type,      &
  Ops_GetNetCDFField

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
  StashItem_v,               &
  StashItem_v_unslanted

USE OpsMod_SharedMemory, ONLY: &
  SharedMemory_type

USE OpsMod_Kinds, ONLY: &
  real32

IMPLICIT NONE

! Subroutine arguments:
TYPE (SharedMemory_type), INTENT(INOUT) :: FieldDataShared
TYPE (DomainInfo_type), INTENT(IN)      :: domain
TYPE (FieldInfo_type), INTENT(INOUT)    :: FieldsInfo(:)
TYPE (CXArray_type), INTENT(IN)         :: CXArrays(:,:)
INTEGER, INTENT(IN)                     :: ForecastTimes(:)
INTEGER, INTENT(IN)                     :: ModelTimeUTC(:)
TYPE (NetCDFFields_type), INTENT(INOUT) :: ncfields
INTEGER, INTENT(IN)                     :: iTime

! Local declarations:
INTEGER                                 :: iReadStartEnd(2)
INTEGER                                 :: NumForecastTimes
INTEGER                                 :: NumFields
INTEGER                                 :: iField
INTEGER                                 :: iLevel
INTEGER                                 :: iLevelCounter
INTEGER                                 :: StashCode
INTEGER                                 :: ForecastTime
LOGICAL                                 :: ForecastMissing
REAL(kind=real32), POINTER              :: FieldData(:,:)
REAL(kind=real32), ALLOCATABLE          :: ParamA(:,:)
REAL(kind=real32), ALLOCATABLE          :: ParamB(:,:)
REAL(kind=real32), ALLOCATABLE          :: LapseRate(:,:) ! near surface lapse rate
CHARACTER(len=*), PARAMETER             :: RoutineName = 'Ops_ReadModelShared'
INTEGER                                 :: i
INTEGER                                 :: j

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! Get the dimensions
NumForecastTimes = COUNT (ForecastTimes(:) /= IMDI)
NumFields = SIZE (FieldsInfo)
iReadStartEnd = FieldDataShared % GetReadStartEnd()

iLevelCounter = 0
DO iField = 1, NumFields

  StashCode = FieldsInfo(iField) % StashCode

  ! If the field for this forecast time is not available in the dump,
  ! find the field at the next available forecast time.
  CALL Ops_GetUMFieldAvailable (ForecastTime,     &
                                ForecastMissing,  &
                                FieldsInfo,       &
                                iTime,            &
                                iField,           &
                                NumForecastTimes)
  IF (ForecastMissing) CYCLE

  VertLevels: DO iLevel = 1, MAXVAL (CXArrays(:,iField) % NumLevels)

    iLevelCounter = iLevelCounter + 1

    ! Only read on PE's between start and end - otherwise cycle
    IF (iLevelCounter < iReadStartEnd(1) .OR. iLevelCounter > iReadStartEnd(2)) CYCLE

    ! Read in the present UM field for this level and time
    !------------------------------------------------------

    IF (GeneralMode >= VerboseMode .AND. mype == 0) THEN
      WRITE (MessageOut, '(A,L1,A,I0)') 'FieldInfo % DerivedField is ', &
        FieldsInfo(iField) % DerivedField, ' for stash code ', StashCode
    END IF

    FieldData => FieldDataShared % GetData(iField,iLevelCounter,iTime)

    IF (FieldsInfo(iField) % DerivedField) THEN

      IF (.NOT. ALLOCATED (ParamA)) THEN
        CALL Ops_DeriveField (domain % header(1),    &
                              ForecastTimes(iTime),  &
                              ModelTimeUTC,          &
                              ParamA,                &
                              ParamB,                &
                              LapseRate,             &
                              ReadOnAllPEs = .TRUE.)       
      END IF
      SELECT CASE (StashCode)
        CASE (StashCode_PSurfParamA)
          FieldData(:,:) = ParamA(:,:)
        CASE (StashCode_PSurfParamB)
          FieldData(:,:) = ParamB(:,:)
        CASE (StashCode_LapseRate)
          FieldData(:,:) = LapseRate(:,:)
      END SELECT

    ELSE

      IF (StashCode < 100000) THEN
        IF (StashCode == StashItem_theta_unslanted) THEN
          CALL Ops_ReadUMField (domain % header(1),                        &
                                StashItem_theta,                           &
                                FieldData,                                 &
                                level = iLevel,                            &
                                Time = ForecastTimes(ForecastTime),        &
                                ModelTimeUTC = ModelTimeUTC,               &
                                Vector = FieldsInfo(iField) % VectorField, &
                                IntField = FieldsInfo(iField) % IntField,  &
                                ReadOnAllPEs = .TRUE.)
        ELSE IF (StashCode == StashItem_u_unslanted) THEN
          CALL Ops_ReadUMField (domain % header(1),                        &
                                StashItem_u,                               &
                                FieldData,                                 &
                                level = iLevel,                            &
                                Time = ForecastTimes(ForecastTime),        &
                                ModelTimeUTC = ModelTimeUTC,               &
                                Vector = FieldsInfo(iField) % VectorField, &
                                IntField = FieldsInfo(iField) % IntField,  &
                                ReadOnAllPEs = .TRUE.)
        ELSE IF (StashCode == StashItem_v_unslanted) THEN
          CALL Ops_ReadUMField (domain % header(1),                        &
                                StashItem_v,                               &
                                FieldData,                                 &
                                level = iLevel,                            &
                                Time = ForecastTimes(ForecastTime),        &
                                ModelTimeUTC = ModelTimeUTC,               &
                                Vector = FieldsInfo(iField) % VectorField, &
                                IntField = FieldsInfo(iField) % IntField,  &
                                ReadOnAllPEs = .TRUE.)
        ELSE IF (StashCode == StashItem_q_unslanted) THEN
          CALL Ops_ReadUMField (domain % header(1),                        &
                                StashItem_q,                               &
                                FieldData,                                 &
                                level = iLevel,                            &
                                Time = ForecastTimes(ForecastTime),        &
                                ModelTimeUTC = ModelTimeUTC,               &
                                Vector = FieldsInfo(iField) % VectorField, &
                                IntField = FieldsInfo(iField) % IntField,  &
                                ReadOnAllPEs = .TRUE.)
        ELSE IF (StashCode == StashItem_p_unslanted) THEN
          CALL Ops_ReadUMField (domain % header(1),                        &
                                StashItem_p,                               &
                                FieldData,                                 &
                                level = iLevel,                            &
                                Time = ForecastTimes(ForecastTime),        &
                                ModelTimeUTC = ModelTimeUTC,               &
                                Vector = FieldsInfo(iField) % VectorField, &
                                IntField = FieldsInfo(iField) % IntField,  &
                                ReadOnAllPEs = .TRUE.)
        ELSE
          CALL Ops_ReadUMField (domain % header(1),                        &
                                StashCode,                                 &
                                FieldData,                                 &
                                level = iLevel,                            &
                                Time = ForecastTimes(ForecastTime),        &
                                ModelTimeUTC = ModelTimeUTC,               &
                                Vector = FieldsInfo(iField) % VectorField, &
                                IntField = FieldsInfo(iField) % IntField,  &
                                ReadOnAllPEs = .TRUE.)
        END IF
      ELSE
        IF (StashCode == 999998) THEN
          CALL Ops_GetNetCDFField (ncfields, &
                                   "cha")
          DO i = 1, SIZE (ncfields % netcdf_cha, DIM = 1)
            DO j = 1, SIZE (ncfields % netcdf_cha, DIM = 2)
              FieldData(j,i) = ncfields % netcdf_cha(i,j,iTime)
            END DO
          END DO
        ELSE IF (StashCode == 999999) THEN
          CALL Ops_GetNetCDFField (ncfields, &
                                   "mss")
          DO i = 1, SIZE (ncfields % netcdf_mss, DIM = 1)
            DO j = 1, SIZE (ncfields % netcdf_mss, DIM = 2)
              FieldData(j,i) = ncfields % netcdf_mss(i,j,iTime)
            END DO
          END DO
        ELSE
          FieldData = RMDI
        END IF
      END IF
    END IF

  END DO VertLevels

END DO ! iField

IF (ALLOCATED (ParamA)) THEN
  DEALLOCATE (ParamA)
  DEALLOCATE (ParamB)
  DEALLOCATE (LapseRate)
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ReadModelShared
