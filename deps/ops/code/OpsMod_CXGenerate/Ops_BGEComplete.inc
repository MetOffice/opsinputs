!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! This routine completes the back ground error set up code left outstanding by
! the routines belonging to the generic UM reading module OpsMod_CXGenerate.
!
! Inputs : See argument list above
!
! Outputs: Completed structure of background error columns
!-------------------------------------------------------------------------------

SUBROUTINE Ops_BGEComplete (Observations, & ! Observation structure report levels
                            BackGrErrCX,  & ! structure of background error columns
                            BGEHeader,    & ! BGE UM file header
                            LevelsOfCx)     ! Number of levels of Cx data

USE GenMod_Control, ONLY: &
    GeneralMode,          &
    VerboseMode,          &
    mype

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    gen_warn,          &
    MessageOut,        &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI,                                &
    RMDI

USE GenMod_ModelIO, ONLY: &
    UM_header_type

USE GenMod_UMHeaderConstants, ONLY: &
    FH_LevDepCStart,                &
    FH_LevDepCSize1,                &
    FH_SubModel,                    &
    FH_SubModel_Ocean

USE OpsMod_CXInfo, ONLY: &
    CX_type

USE OpsMod_MiscTypes, ONLY: &
    ZcodeLevelsA

USE OpsMod_ObsInfo, ONLY: &
    OB_type,              &
    Ops_Alloc

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(IN)        :: Observations ! Obs structure report levels
TYPE (Cx_type), INTENT(INOUT)     :: BackGrErrCX  ! structure of backg. error cols
TYPE (UM_Header_Type), INTENT(IN) :: BGEHeader    ! BGE UM file header
INTEGER, INTENT(INOUT)            :: LevelsOfCx

! Local declarations:
INTEGER                           :: NumObsLocal
INTEGER                           :: I                  ! Loop variable
INTEGER                           :: NumCxBackErrLevA   ! Num levels of bgerr used
CHARACTER(len=80)                 :: ErrorMessage(3)
CHARACTER(len=*),  PARAMETER      :: RoutineName = 'Ops_BGEComplete'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

ErrorMessage(:) = ' '
NumObsLocal = Observations % header % NumObsLocal

!  Add pressure and obstore info to Cx background error structure.
BackGrErrCx % header % PoleLat = RMDI
BackGrErrCx % header % PoleLong = RMDI
BackGrErrCx % header % ObsGroup = Observations % header % ObsGroup
BackGrErrCx % header % ValidityTime = Observations % header % ValidityTime
BackGrErrCx % header % ObsGroup = Observations % header % ObsGroup

IF (BGEheader %  FixHd(FH_LevDepCStart) > 0) THEN
  BackGrErrCx % header % PlevelsA % Present = .TRUE.
  BackGrErrCx % header % PlevelsA % Zcode = ZcodeLevelsA
  ! Use the number of levels from the control file unless it is greater than
  ! the number in the file, in which case, use number from file
  IF (LevelsOfCx == IMDI) THEN
    LevelsOfCx = BGEheader % FixHd(FH_LevDepCSize1)
    Errormessage(1) = 'Number of background error levels specified in control'
    Errormessage(2) = 'file is missing so the number in the file header'
    Errormessage(3) = 'is used.'
    CALL gen_warn (RoutineName,       &
                   ErrorMessage(1:3))
  ELSE IF (LevelsOfCx <= BGEheader % FixHd(FH_LevDepCSize1)) THEN
    NumCxBackErrLevA = LevelsOfCx
  ELSE
    NumCxBackErrLevA = BGEheader % FixHd(FH_LevDepCSize1)
    Errormessage(1) = 'Number of background error levels specified in control'
    Errormessage(2) = 'file is  greater than the number in the file header'
    Errormessage(3) = 'so file header value is used.'
    CALL gen_warn (RoutineName,       &
                   ErrorMessage(1:3))
  END IF

  ALLOCATE (BackGrErrCX % PlevelsA(NumObsLocal,NumCxBackErrLevA))
  BackGrErrCx % header % PlevelsA % NumLev = NumCxBackErrLevA
  DO I = 1,NumCxBackErrLevA
    IF (BGEHeader % Fixhd(FH_SubModel) == FH_SubModel_Ocean) THEN
      BackGrErrCX % PlevelsA(:,I) = BGEheader % LevDepC(I)
    ELSE    ! change pressures to Pascals
      BackGrErrCX % PlevelsA(:,I) = BGEheader % LevDepC(I) * 100.0
    END IF
  END DO
END IF

BackGrErrCx % header % PlevelsB % Present = .FALSE.

IF (BackGrErrCx % header % pmsl % Present) THEN
  BackGrErrCX % pmsl(:) = BackGrErrCX % pmsl(:) * 100.0
END IF

!  Copy background errors from u slot into v slot
IF (BackGrErrCx % header % u10 % Present) THEN
  CALL Ops_Alloc (BackGrErrCX % header % v10, &
                  "BGErr % v10",              &
                  NumObsLocal,                &
                  BackGrErrCX % v10)
  BackGrErrCX % v10(:) = BackGrErrCX % u10(:)
END IF

IF (BackGrErrCx % header % u % Present) THEN
  CALL Ops_Alloc (BackGrErrCX % header % v,         &
                  "BGErr % v",                      &
                  NumObsLocal,                      &
                  BackGrErrCX % v,                  &
                  HdrIn = BackGrErrCx % header % u)
  BackGrErrCX % v(:,:) = BackGrErrCX % u(:,:)
END IF

BackGrErrCX % Header % Rotated = .FALSE.

IF (GeneralMode >= VerboseMode) THEN
  IF (mype == 0) THEN
    WRITE (MessageOut, '(4A)') ' For  ', TRIM (Observations % header % ObsGroupName), &
                                                           ' in ', RoutineName
    WRITE (MessageOut, '(A)')  'Background Error CX structure '
  END IF
  CALL BackGrErrCX % print
END IF

BackGrErrCx % header % NumTotal = Observations % header % NumObsTotal

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_BGEComplete
