!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Top level routine to read either the BG or BGE from a UM dump, perform
! horizontal and time interpolation of a UM dump, fill an array of CX structures
! and complete any BG/BGE specific code required.
!
! Method:
!      0. Tracing and Initialisations
!      1. Set up control information for UM Ffile extraction
!      2. Read BGE or UM dump and Generate BGE or Cx files using top level
!         reading routine Ops_CXGenerate
!      3. Complete any file/obgroup specific info
!-------------------------------------------------------------------------------

SUBROUTINE Ops_BGEandCxCreate (Observations,    &
                               CXFiles,         &
                               obs_from_varobs, &
                               BGECall,         &
                               NumCxLevels,     &
                               ops_call)

USE GenMod_Control, ONLY: &
    VerboseMode,          &
    mype,                 &
    GeneralMode

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    StatusOK,          &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI

USE GenMod_ModelIO, ONLY: &
    UM_header_type

USE GenMod_UMHeaderConstants, ONLY: &
    FH_LookupSize2

USE OpsMod_CXInfo, ONLY: &
    CX_Type

USE OpsMod_IOUtils, ONLY: &
    Ops_CloseCFile

USE OpsMod_ModelIO, ONLY:         &
    DomainInfo_type,              &
    Ops_FieldsFile_ReadHeaderEnv

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupSurface

USE OpsMod_ObsInfo, ONLY: &
    OB_type,              &
    Ops_Alloc

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_Type), INTENT(INOUT)  :: Observations(:)
TYPE (CX_type), INTENT(INOUT)  :: CXFiles(:)
LOGICAL, INTENT(IN)            :: obs_from_varobs
LOGICAL, INTENT(IN)            :: BGECall
INTEGER, OPTIONAL, INTENT(OUT) :: NumCxLevels(:)
LOGICAL, OPTIONAL, INTENT(IN)  :: ops_call

! Local declarations:
INTEGER, ALLOCATABLE           :: CxLevels_all(:)
INTEGER                        :: IGroup
INTEGER, ALLOCATABLE           :: ForecastTimes(:)
INTEGER, ALLOCATABLE           :: ModelTimeUTC(:)
TYPE (UM_Header_Type)          :: CXheader
CHARACTER(len=*), PARAMETER    :: RoutineName = 'Ops_BGEandCxCreate'
INTEGER, ALLOCATABLE           :: CxFields(:,:)
TYPE (DomainInfo_type)         :: background

!--------------------------------
! 0. Tracing and Initialisations
!--------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

ALLOCATE (CxLevels_all(SIZE (Observations)))
CxLevels_all(:) = IMDI
ALLOCATE (CxFields(SIZE (Observations),MaxModelCodes))

!--------------------------------
! 1. Set up control information
!--------------------------------

IF (BGECall) THEN
  CALL Ops_FieldsFile_ReadHeaderEnv (background,                       &
                                     "OPS_BACKERR",                    &
                                     status_if_env_not_set = StatusOK)
ELSE
  CALL Ops_FieldsFile_ReadHeaderEnv (background,   &
                                     "OPS_UMBACK")
END IF

IF (ALLOCATED (background % header)) THEN
  CALL Ops_CXSetup (Observations,           &
                    CXFiles,                &
                    background % header(1), &
                    CxFields,               &
                    CxLevels_all,           &
                    ForecastTimes,          &
                    ModelTimeUTC,           &
                    BGECall,                &
                    NumCxLevels,            &
                    ops_call)

  CALL Ops_CXGenerate (Observations,     &
                       background,       &
                       ForecastTimes(:), &
                       CxFields,         &
                       CxLevels_all,     &
                       CXFiles,          &
                       BGECall,          &
                       ModelTimeUTC(:),  &
                       obs_from_varobs)

  ! Close dump file and free unit number
  CALL Ops_CloseCFile (background % header(1) % UnitNum)

  !-------------------------------------------
  ! 3. Finish with file/obgroup specific info
  !-------------------------------------------

  IF (BGECall) THEN
    DO IGroup = 1, SIZE (Observations)
      CALL Ops_BGEComplete (Observations(IGroup),   &
                            CXFiles(IGroup),        &
                            background % header(1), &
                            CxLevels_all(IGroup))
    END DO
  ELSE IF (.NOT. obs_from_varobs) THEN
    CALL Ops_CreateCxUMHdr (background % header(1),                         &
                            background % header(1) % FixHd(FH_LookupSize2), &
                            CXheader)
    DO IGroup = 1, SIZE (Observations)
      CALL Ops_CxComplete (Observations(IGroup), &
                           CXFiles(IGroup),      &
                           CXHeader,             &
                           CxLevels_all(IGroup))
    END DO
  ELSE
    DO IGroup = 1, SIZE (Observations)
      IF (GeneralMode >= VerboseMode .AND. mype == 0) THEN
        WRITE (MessageOut, '(4A)') ' For  ', &
                         TRIM (Observations(IGroup) % Header % ObsGroupName), &
                         ' in ', RoutineName
        WRITE (MessageOut, '(A)') ' CX structure '
        CALL CxFiles(IGroup) % print
      END IF
      IF (Observations(IGroup) % Header % ObsGroup == ObsGroupSurface .AND. &
          CXFiles(IGroup) % Header % aerosol_p % Present) THEN
        CALL Ops_Alloc (CXFiles(IGroup) % Header % Aerosol,  &
                        'Cx % Aerosol',                      &
                        CXFiles(IGroup) % Header % NumLocal, &
                        CXFiles(IGroup) % Aerosol)
        CXFiles(IGroup) % Aerosol(:) = CXFiles(IGroup) % Aerosol_p(:,1)
      END IF
    END DO
  END IF

  !------------------
  ! 4. Tidy and Exit
  !------------------

  CALL background % header(1) % dealloc
  IF (.NOT. (obs_from_varobs .OR. BGECall)) THEN
    CALL CXHeader % dealloc
  END IF
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_BGEandCxCreate
