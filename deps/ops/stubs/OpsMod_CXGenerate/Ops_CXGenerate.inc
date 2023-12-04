!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Perform horizontal and time interpolation of a UM dump to fill an array of CX
! structures.
!
! Method:
!   Three routines are called to do the work:
!   1: Ops_CXSetupArrays (which includes routine Ops_CXAddField)
!     The positions and number of levels of each field within the UM dump
!     is determined. The arrays to be filled within the CX structure are
!     allocated, and an array of structures (CXArray_Type) is prepared
!     which points to each field in the cx structure.
!   2: Ops_CXPreproLocations
!     Preprocessing is performed of the observation locations and times.
!     A structure (LocInfo_Type) is prepared for each observation group,
!     which contains the latitude and longitude in radians, an index to the
!     observations in time order, together with the integers indicating the
!     ranges of observations in each time window, and the position of each
!     observation within its time window.
!   3: Ops_CXPerformInterp
!     The horizontal and time interpolations are performed.
!     Each level of each field is read in from the dump in turn,
!     at all forecast periods. The observations in each group which
!     require each forecast time are interpolated. The two values
!     for each observation are weighted and summed.
!
! Inputs:
!   Obs: Array of structures containing observations for each obsgroup,
!     required for observation locations.
!   ForecastTimes: The forecast times to be interpolated between.
!   RequiredFields: A 2-d array specifying the stashcodes required
!     for each obsgroup.
!   NumLevelsRequired: A 2-d array specifying the number of levels required
!     for each required field.
!   ModelTimeUTC: Validity time UTC in the UM dump.
!
! Outputs:
!   CXs: Array of cx structures for each obsgroup, filled with interpolated
!     background field data.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CXGenerate (Obs,               &
                           domain,            &
                           ForecastTimes,     &
                           RequiredFields,    &
                           NumLevelsRequired, &
                           CXs,               &
                           BGECall,           &
                           ModelTimeUTC,      &
                           obs_from_varobs)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE GenMod_UMHeaderConstants, ONLY: &
  FH_Dataset,                       &
  FH_Dataset_Ancil,                 &
  FH_Dataset_InstDump

USE OpsMod_CXInfo, ONLY: &
  CX_Type

USE OpsMod_ModelIO, ONLY: &
  DomainInfo_type

USE OpsMod_ObsInfo, ONLY: &
  Ob_Type

IMPLICIT NONE

! Subroutine arguments:
TYPE (Ob_Type), INTENT(IN)         :: Obs(:)
TYPE (DomainInfo_type), INTENT(IN) :: domain
INTEGER, INTENT(IN)                :: ForecastTimes(:)
INTEGER, INTENT(IN)                :: RequiredFields(:,:) ! The fields required for each group (iField)
INTEGER, INTENT(IN)                :: NumLevelsRequired(:) ! The number of levels required for each field
TYPE (Cx_Type), INTENT(INOUT)      :: Cxs(:)
LOGICAL, INTENT(IN)                :: BGECall
INTEGER, INTENT(IN)                :: ModelTimeUTC(:)
LOGICAL, INTENT(IN)                :: obs_from_varobs

! Local declarations:
TYPE (FieldInfo_type), ALLOCATABLE :: FieldsInfo(:)
INTEGER                            :: iField
INTEGER                            :: iField2
INTEGER                            :: iGroup
INTEGER                            :: field_counter
INTEGER                            :: AllFieldsList(MaxModelCodes)
TYPE (CXArray_type), ALLOCATABLE   :: CXArrays(:,:)
TYPE (LocInfo_type), ALLOCATABLE   :: LocsInfo(:)
CHARACTER(len=*), PARAMETER        :: RoutineName = 'Ops_CXGenerate'
CHARACTER(len=80)                  :: ErrorMessage(2)

IF (UseTrace) CALL gen_trace_entry (RoutineName)

ALLOCATE (LocsInfo(SIZE (Obs)))

AllFieldsList = IMDI
field_counter = 0

DO iGroup = 1, SIZE (Obs)
  DO iField = 1, SIZE (RequiredFields, DIM = 2)
    IF (ALL (AllFieldsList /= RequiredFields(iGroup,iField))) THEN
      field_counter = field_counter + 1
      AllFieldsList(field_counter) = RequiredFields(iGroup,iField)
    END IF
  END DO
END DO

ALLOCATE (FieldsInfo(field_counter))
ALLOCATE (CXArrays(SIZE (Obs),field_counter))
CxArrays(:,:) % NumLevels = 0
DO iField = 1, field_counter
  DO iGroup = 1, SIZE (Obs)
    NULLIFY (CxArrays(iGroup,iField) % R1)
    NULLIFY (CxArrays(iGroup,iField) % R2)
  END DO
  FieldsInfo(iField) % FieldSetup = .FALSE.
END DO

!---------------------------------------------------------
! 1. Prepare CX arrays and corresponding info in CXArrays
!---------------------------------------------------------

IF (domain % header(1) % FixHd(FH_Dataset) == FH_Dataset_Ancil .OR. &
    domain % header(1) % FixHd(FH_Dataset) == FH_Dataset_InstDump) THEN
  CALL Ops_CXSetupArraysAncil (domain % header(1), &
                               ForecastTimes,      &
                               Obs,                &
                               RequiredFields,     &
                               NumLevelsRequired,  &
                               CXs,                &
                               CXArrays,           &
                               FieldsInfo,         &
                               ModelTimeUTC)
ELSE
  CALL Ops_CXSetupArrays (domain % header(1), &
                          ForecastTimes,      &
                          Obs,                &
                          RequiredFields,     &
                          NumLevelsRequired,  &
                          CXs,                &
                          CXArrays,           &
                          FieldsInfo,         &
                          BGECall,            &
                          ModelTimeUTC)
END IF

DO iField = 1, field_counter
  IF (.NOT. FieldsInfo(iField) % FieldSetup) THEN
    IF (BGECall) THEN
      WRITE (ErrorMessage(1), '(A)') "Unknown stash code specified for background error reading:"
    ELSE
      WRITE (ErrorMessage(1), '(A)') "Unknown stash code specified for background reading:"
    END IF
    DO iField2 = 1, field_counter
      IF (ALL (FieldsInfo(1:iField - 1) % StashCode /= AllFieldsList(iField2))) THEN
        WRITE (ErrorMessage(2), '(A,I0)') "StashCode = ", AllFieldsList(iField2)
        EXIT
      END IF
    END DO
    CALL gen_fail (RoutineName,       &
                   ErrorMessage(1:2))
  END IF
END DO

!---------------------------------------------------
! 2. Preprocess the observation locations and times
!---------------------------------------------------

CALL Ops_CXPreproLocations (Obs,             &
                            ForecastTimes,   &
                            domain,          &
                            LocsInfo,        &
                            obs_from_varobs)

!---------------------------
! 3. Perform interpolations
!---------------------------

CALL Ops_CXPerformInterp (Obs,            &
                          domain,         &
                          FieldsInfo,     &
                          CXArrays,       &
                          LocsInfo,       &
                          ForecastTimes,  &
                          RequiredFields, &
                          BGECall,        &
                          ModelTimeUTC)

! Deallocations
!---------------
DO iField = 1, SIZE (FieldsInfo)
  DEALLOCATE (FieldsInfo(iField) % StartPositions)
END DO

DO iGroup = 1, SIZE (Obs)
  IF (ASSOCIATED (LocsInfo(iGroup) % Phi)) DEALLOCATE (LocsInfo(iGroup) % Phi)
  IF (ASSOCIATED (LocsInfo(iGroup) % Lambda)) DEALLOCATE (LocsInfo(iGroup) % Lambda)
  IF (ASSOCIATED (LocsInfo(iGroup) % SPhi)) DEALLOCATE (LocsInfo(iGroup) % SPhi)
  IF (ASSOCIATED (LocsInfo(iGroup) % SLambda)) DEALLOCATE (LocsInfo(iGroup) % SLambda)
  IF (ASSOCIATED (LocsInfo(iGroup) % STime)) DEALLOCATE (LocsInfo(iGroup) % STime)
  IF (ASSOCIATED (LocsInfo(iGroup) % Index)) DEALLOCATE (LocsInfo(iGroup) % Index)
  IF (ASSOCIATED (LocsInfo(iGroup) % FirstInWindow)) DEALLOCATE (LocsInfo(iGroup) % FirstInWindow)
  IF (ASSOCIATED (LocsInfo(iGroup) % LastInWindow)) DEALLOCATE (LocsInfo(iGroup) % LastInWindow)
END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CXGenerate
