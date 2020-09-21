!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Sets up fields information and cx arrays information in parent subroutine
! (Ops_CXSetupArrays)
!
! Method:
!     The routine is called from Ops_CXSetupArrays, for each element in the
!     cx structure. Depending on the value of the parent subroutines iGroup
!     variable, different actions are performed:
!       iGroup  Action
!         -1    Simply count the number of calls, ie increment iField.
!          0    Record details of field in FieldsInfo, including position
!                 and number of levels in dump, and values of optional
!                 logical arguments.
!          1+   If the stashcode is required for this group, work out the
!                 number of levels required, allocate the array in the cx
!                 structure and set up its header. Then point to the array
!                 from the CxArrays structure so that the interpolation routine
!                 can fill it with data.
!
!     (Presently it is assumed that derived fields are present and that they
!     are single level. If any derived fields are multiple level, a DerivedFrom
!     argument could be added. This would be an array of stashcodes, whose
!     presence and number of levels could be used to determine the 'presence'
!     and number of levels of the derived field.)
!
! Inputs:
!     StashCode: the stashcode of the field
!     ElHdr: A pointer to the header of the element in the cx header.
!     R1: An optional pointer to the single level array in the cx structure.
!     R2: An optional pointer to the multiple level array in the cx structure.
!     VectorField: An optional logical indicating a vector field, default false.
!     IntField: An optional logical indicating an integer field, default false.
!     DerivedField: An optional logical indicating a derived field, default false.
!
! Also uses information from parent subroutine:
!     RequiredFields
!     NumLevelsRequired
!     iGroup
!     iField
!
! Outputs:
!     Variables owned by parent subroutine are modified:
!       Element iField of array FieldsInfo is set up when called for first pass.
!       Element (iGroup,iField) of array CxArrays is set up on subsequent passes.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CXAddField (StashCode,    &
                           ElHdr,        &
                           R1,           &
                           R2,           &
                           VectorField,  &
                           IntField,     &
                           DerivedField)

USE GenMod_Control, ONLY: &
  GeneralMode,            &
  DebugMode,              &
  MyPE

USE GenMod_Core, ONLY: &
  gen_message,         &
  gen_trace_entry,     &
  gen_trace_exit,      &
  MessageOut,          &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI,                                  &
  RMDI

USE OpsMod_MiscTypes, ONLY: &
  ElementHeader_Type

USE OpsMod_Stash, ONLY:      &
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
INTEGER, INTENT(IN)                      :: StashCode
TYPE (ElementHeader_Type), INTENT(INOUT) :: ElHdr
REAL, OPTIONAL, POINTER                  :: R1(:)
REAL, OPTIONAL, POINTER                  :: R2(:,:)
LOGICAL, OPTIONAL, INTENT(IN)            :: VectorField  ! Whether this is a component of a vector field
LOGICAL, OPTIONAL, INTENT(IN)            :: IntField     ! Whether this is stored as an integer field in the UM Dump
LOGICAL, OPTIONAL, INTENT(IN)            :: DerivedField ! Whether this is derived from other fields in the dump

! Local declarations:
INTEGER                                  :: NumObs
INTEGER                                  :: NumLevels
INTEGER                                  :: iTime
CHARACTER(len=80)                        :: ErrMess(2)
LOGICAL                                  :: FieldRequired
INTEGER                                  :: FieldCounter
CHARACTER(len=*), PARAMETER              :: RoutineName = 'Ops_CXAddField'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (ANY (RequiredFields == StashCode)) THEN

  iField = iField + 1

  IF (iGroup == 0) THEN

    ! First time around, locate this field in the dump.
    !---------------------------------------------------

    FieldsInfo(iField) % VectorField = .FALSE.
    IF (PRESENT (VectorField)) FieldsInfo(iField) % VectorField = VectorField

    FieldsInfo(iField) % IntField = .FALSE.
    IF (PRESENT (IntField)) FieldsInfo(iField) % IntField = IntField

    FieldsInfo(iField) % DerivedField = .FALSE.
    IF (PRESENT (DerivedField)) FieldsInfo(iField) % DerivedField = DerivedField

    FieldsInfo(iField) % StashCode = StashCode
    FieldsInfo(iField) % FieldSetup = .TRUE.

    ALLOCATE (FieldsInfo(iField) % StartPositions(NumForecastTimes))

    IF (StashCode < 100000) THEN
      DO iTime = 1, NumForecastTimes
        IF (StashCode == StashItem_theta_unslanted) THEN
          CALL Ops_LocateUMField (UMHdr,                                      &
                                  StashItem_theta,                            &
                                  ForecastTimes(iTime),                       &
                                  FieldsInfo(iField) % StartPositions(iTime), &
                                  FieldsInfo(iField) % NumLevelsAvailable,    &
                                  ModelTimeUTC)
        ELSE IF (StashCode == StashItem_u_unslanted) THEN
          CALL Ops_LocateUMField (UMHdr,                                      &
                                  StashItem_u,                                &
                                  ForecastTimes(iTime),                       &
                                  FieldsInfo(iField) % StartPositions(iTime), &
                                  FieldsInfo(iField) % NumLevelsAvailable,    &
                                  ModelTimeUTC)
        ELSE IF (StashCode == StashItem_v_unslanted) THEN
          CALL Ops_LocateUMField (UMHdr,                                      &
                                  StashItem_v,                                &
                                  ForecastTimes(iTime),                       &
                                  FieldsInfo(iField) % StartPositions(iTime), &
                                  FieldsInfo(iField) % NumLevelsAvailable,    &
                                  ModelTimeUTC)
        ELSE IF (StashCode == StashItem_q_unslanted) THEN
          CALL Ops_LocateUMField (UMHdr,                                      &
                                  StashItem_q,                                &
                                  ForecastTimes(iTime),                       &
                                  FieldsInfo(iField) % StartPositions(iTime), &
                                  FieldsInfo(iField) % NumLevelsAvailable,    &
                                  ModelTimeUTC)
        ELSE IF (StashCode == StashItem_p_unslanted) THEN
          CALL Ops_LocateUMField (UMHdr,                                      &
                                  StashItem_p,                                &
                                  ForecastTimes(iTime),                       &
                                  FieldsInfo(iField) % StartPositions(iTime), &
                                  FieldsInfo(iField) % NumLevelsAvailable,    &
                                  ModelTimeUTC)
        ELSE
          CALL Ops_LocateUMField (UMHdr,                                      &
                                  StashCode,                                  &
                                  ForecastTimes(iTime),                       &
                                  FieldsInfo(iField) % StartPositions(iTime), &
                                  FieldsInfo(iField) % NumLevelsAvailable,    &
                                  ModelTimeUTC)
        END IF
        IF (GeneralMode >= DebugMode .AND. MyPE == 0 .AND. &
            FieldsInfo(iField) % StartPositions(iTime) /= IMDI) THEN
          WRITE (MessageOut, '(A,I0,A,I0)')' Found field ', StashCode, ' at forecast time ',&
                  ForecastTimes(iTime)
        END IF
      END DO
    ELSE
      FieldsInfo(iField) % StartPositions(:) = -1
      FieldsInfo(iField) % NumLevelsAvailable = 1
    END IF

  ELSE IF (iGroup > 0) THEN

    ! Search for the stashcode in the required fields for this group
    !----------------------------------------------------------------
    FieldRequired = .FALSE.
    DO FieldCounter = 1, SIZE (RequiredFields, DIM = 2)
      IF (RequiredFields(iGroup,FieldCounter) == StashCode) THEN
        FieldRequired = .TRUE.
      END IF
    END DO
    IF (FieldRequired) THEN
      ! Check that this field is available in the dump, for all forecast times.
      ! Let derived fields pass.
      IF (ANY (FieldsInfo(iField) % StartPositions(:) /= IMDI) .OR. &
          FieldsInfo(iField) % DerivedField) THEN
        NumObs = Obs(iGroup) % Header % NumObsLocal
        IF (PRESENT (R1)) THEN

          ALLOCATE (R1(NumObs))
          NumLevels = 1
          R1(:) = RMDI
          CxArrays(iGroup,iField) % R1 => R1

          IF (GeneralMode >= DebugMode .AND. MyPE == 0) THEN
            WRITE (MessageOut,'(A, I0, 3A, L1, A, I0)')'Allocated single level field ', &
              Stashcode, ' for group ', TRIM (Obs(iGroup) % Header % ObsGroupName),&
              ' FieldInfo % DerivedField is ', FieldsInfo(iField) % DerivedField, &
              ' NumObs = ',NumObs
          END IF

        ELSE IF (PRESENT (R2)) THEN
          NumLevels = NumLevelsRequired(iGroup)

          !  Don't do 2nd half of this here- it messes up the stats - put below
          ! instead
          IF (NumLevels == IMDI .OR. &
              (NumLevels > FieldsInfo(iField) % NumLevelsAvailable .AND. FieldsInfo(iField) % NumLevelsAvailable > 0)) THEN
            NumLevels = FieldsInfo(iField) % NumLevelsAvailable
          END IF
          IF (StashCode == StashItem_p .OR. StashCode == StashItem_p_unslanted) THEN
            NumLevels = NumLevels + 1 ! extra Exner PlevelA required
          END IF
          ALLOCATE (R2(NumObs,NumLevels))
          R2(:,:) = RMDI
          CxArrays(iGroup,iField) % R2 => R2

          IF (GeneralMode >= DebugMode .AND. MyPE == 0) THEN
            WRITE (MessageOut,'(A,I0,3A,I0,A)') 'Allocated multiple level field ', &
              Stashcode, ' for group ', &
              TRIM (Obs(iGroup) % Header % ObsGroupName), ', ', &
              NumLevels, ' levels.'
          END IF

        END IF

        ElHdr % Present = .TRUE.
        ElHDr % NumLev = NumLevels
        ElHDr % ZCode  = ZCode
        CxArrays(iGroup,iField) % NumLevels = NumLevels

      END IF ! Field available

      IF (.NOT. FieldsInfo(iField) % DerivedField) THEN
        DO iTime = 1, NumForecastTimes
          IF (FieldsInfo(iField) % StartPositions(iTime) == IMDI) THEN
            WRITE (ErrMess(1), '(A,I0,A,I0)') 'Field with stashcode ', &
              Stashcode,' not available at forecast time ',ForecastTimes(iTime)
            WRITE (ErrMess(2), '(2A)') 'for interpolation for obsgroup ', &
              TRIM (Obs(iGroup) % Header % ObsGroupName)
            CALL gen_message (RoutineName, &
                              ErrMess(:))
          END IF
        END DO

      END IF ! Derived field

    END IF ! Field required

  END IF ! First pass

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CXAddField
