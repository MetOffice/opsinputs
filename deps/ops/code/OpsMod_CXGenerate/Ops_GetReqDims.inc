!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Get the dimensions of the shared memory.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_GetReqDims (FieldDims,     &
                           domain,        &
                           FieldsInfo,    &
                           CXArrays,      &
                           ForecastTimes, &
                           ncfields,      &
                           ModelTimeUTC)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_ModelIO, ONLY: &
  DomainInfo_type,        &
  NetCDFFields_type

USE OpsMod_FieldRead, ONLY: &
  Ops_GetUMFieldDims

USE OpsMod_Stash, ONLY:      &
  StashItem_orog,            &
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

IMPLICIT NONE

! Subroutine arguments:
INTEGER, ALLOCATABLE, INTENT(INOUT)     :: FieldDims(:,:,:)
TYPE (DomainInfo_Type), INTENT(IN)      :: domain
TYPE (FieldInfo_type), INTENT(INOUT)    :: FieldsInfo(:)
TYPE (CXArray_type), INTENT(IN)         :: CXArrays(:,:)
INTEGER, INTENT(IN)                     :: ForecastTimes(:)
TYPE (NetCDFFields_type), INTENT(INOUT) :: ncfields
INTEGER, INTENT(IN)                     :: ModelTimeUTC(:)

! Local declarations:
INTEGER                                 :: NumForecastTimes
INTEGER                                 :: NumFields
INTEGER                                 :: StashCode
INTEGER                                 :: iField
INTEGER                                 :: iTime
INTEGER                                 :: Level
INTEGER                                 :: AddBLField_local   ! is THIS 2nd boundary
LOGICAL                                 :: extend_local
INTEGER                                 :: ForecastTime
LOGICAL                                 :: ForecastMissing
CHARACTER(len=*), PARAMETER             :: RoutineName = 'Ops_GetReqDims'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! There should be an option default for ModelTimeUTC
AddBLField_local = 0

extend_local = .TRUE.

! Assume the number of points is the same on all levels
Level = 1

! Get the time dimension
NumForecastTimes = COUNT (ForecastTimes(:) /= IMDI)

! Get the number of fields
NumFields = SIZE (FieldsInfo)

! Get the dimensions of field (1 and 2) and the number of levels (3)
ALLOCATE (FieldDims(3,NumFields,NumForecastTimes))

DO iTime = 1, NumForecastTimes

  DO iField = 1, NumFields 

    ! Set equal to zero in case there are missing forecasts
    FieldDims(:,iField,iTime) = 0
 
    ! If the field for this forecast time is not available in the dump,
    ! find the field at the next available forecast time.
    CALL Ops_GetUMFieldAvailable (ForecastTime,     &
                                  ForecastMissing,  &
                                  FieldsInfo,       &
                                  iTime,            &
                                  iField,           &
                                  NumForecastTimes)
    IF (ForecastMissing) CYCLE

    IF (FieldsInfo(iField) % DerivedField) THEN
      ! StashCode is invalid for DerivedField - use StashCode from derived field routine 
      StashCode = StashItem_Orog
    ELSE
      StashCode = FieldsInfo(iField) % StashCode
    ENDIF

    ! Get the dimensions of the field
    IF (StashCode < 100000) THEN
      IF (StashCode == StashItem_theta_unslanted) THEN
        CALL Ops_GetUMFieldDims (FieldDims(1:2,iField,iTime), &
                                 domain % header(1),          &
                                 StashItem_theta,             &
                                 Level,                       &
                                 ForecastTimes(ForecastTime), &
                                 ModelTimeUTC,                &
                                 AddBLField_local,            &
                                 extend_local)
      ELSE IF (StashCode == StashItem_u_unslanted) THEN
        CALL Ops_GetUMFieldDims (FieldDims(1:2,iField,iTime), &
                                 domain % header(1),          &
                                 StashItem_u,                 &
                                 Level,                       &
                                 ForecastTimes(ForecastTime), &
                                 ModelTimeUTC,                &
                                 AddBLField_local,            &
                                 extend_local)
      ELSE IF (StashCode == StashItem_v_unslanted) THEN
        CALL Ops_GetUMFieldDims (FieldDims(1:2,iField,iTime), &
                                 domain % header(1),          &
                                 StashItem_v,                 &
                                 Level,                       &
                                 ForecastTimes(ForecastTime), &
                                 ModelTimeUTC,                &
                                 AddBLField_local,            &
                                 extend_local)
      ELSE IF (StashCode == StashItem_q_unslanted) THEN
        CALL Ops_GetUMFieldDims (FieldDims(1:2,iField,iTime), &
                                 domain % header(1),          &
                                 StashItem_q,                 &
                                 Level,                       &
                                 ForecastTimes(ForecastTime), &
                                 ModelTimeUTC,                &
                                 AddBLField_local,            &
                                 extend_local)
      ELSE IF (StashCode == StashItem_p_unslanted) THEN
        CALL Ops_GetUMFieldDims (FieldDims(1:2,iField,iTime), &
                                 domain % header(1),          &
                                 StashItem_p,                 &
                                 Level,                       &
                                 ForecastTimes(ForecastTime), &
                                 ModelTimeUTC,                &
                                 AddBLField_local,            &
                                 extend_local)
      ELSE
        CALL Ops_GetUMFieldDims (FieldDims(1:2,iField,iTime), &
                                 domain % header(1),          &
                                 StashCode,                   &
                                 Level,                       &
                                 ForecastTimes(ForecastTime), &
                                 ModelTimeUTC,                &
                                 AddBLField_local,            &
                                 extend_local)
      END IF
    ELSE
      FieldDims(1,iField,iTime) = ncfields % netcdf_num_lats
      FieldDims(2,iField,iTime) = ncfields % netcdf_num_lons
    END IF

    ! Get the max value for this field
    FieldDims(3,iField,iTime) = MAXVAL (CXArrays(:,iField) % NumLevels)

  END DO

END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_GetReqDims
