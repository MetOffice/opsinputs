!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Creates a varobs file from "OpsMod_ObsInfo" structure information.
!
! Inputs: A Fortran 90 structure containing header information from the Cx
!         header, and a Fortran 90 structure, output from the ob processing
!         program, which contains the observational data
!
! Outputs: A "varobs" file, the structure of which is described in OTDP 16
!
! Method: Extract information from the input structures, and use to construct
!         a data array and the varobs file header
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CreateVarobs (Observations,    & ! Structure containing observation info.
                             CxHeader,        & ! Structure containing Cx header info.
                             AssimDataFormat, &
                             NumVarobsTotal,  &
                             PGEBd,           & ! Structure containing Cx header info.
                             RepObs)

USE GenMod_Control, ONLY: &
    GeneralMode,          &
    DebugMode,            &
    nproc,                &
    mype

USE GenMod_Core, ONLY:    &
    gen_close_stats_file, &
    gen_fail,             &
    gen_open_stats_file,  &
    gen_trace_entry,      &
    gen_trace_exit,       &
    gen_warn,             &
    MaxFileNameLen,       &
    MessageOut,           &
    StatsOut,             &
    StatusOK,             &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI

USE GenMod_ModelIO, ONLY: &
    UM_header_type

USE GenMod_UMHeaderConstants, ONLY: &
    FH_IntCSize,                    &
    FH_RealCSize,                   &
    FH_ColDepCSize1,                &
    FH_ColDepCSize2,                &
    FH_LevDepCSize1,                &
    FH_LevDepCSize2,                &
    FH_LookupSize1,                 &
    FH_LookupSize2

USE OpsMod_Control, ONLY: &
    mpi_group

USE OpsMod_EnvUtils, ONLY: &
    ops_env_is_true,       &
    ops_get_env

USE OpsMod_Gcom, ONLY: &
    gcg_isum

USE OpsMod_GPSRO, ONLY: &
    GPSRO_TPD

USE OpsMod_IOUtils, ONLY: &
    Ops_TouchFile

USE OpsMod_ModelObInfo, ONLY: &
    ModelOB_type

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupGeoCloud,          &
    ObsGroupGPSRO,             &
    OpsFn_IsSatRadGroup

USE OpsMod_ObsInfo, ONLY: &
    OB_type,              &
    LenCallSign

USE OpsMod_Radar, ONLY: &
    RadFamily

USE OpsMod_Varfields, ONLY: &
    ActualMaxVarfield,      &
    NumObVarMax

USE OpsMod_VarobsLib, ONLY: &
    varobs_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(INOUT)             :: Observations
TYPE (UM_header_type), INTENT(IN)         :: CxHeader
INTEGER, INTENT(IN)                       :: AssimDataFormat
INTEGER, INTENT(OUT)                      :: NumVarObsTotal
TYPE (ModelOB_type), INTENT(IN), OPTIONAL :: PGEBd
TYPE (OB_type), OPTIONAL, INTENT(IN)      :: RepObs

! Local declarations:
CHARACTER(len=*), PARAMETER               :: RoutineName = "Ops_CreateVarobs"
TYPE (ModelOb_type)                       :: VarobPGEs
CHARACTER(len=16)                         :: ObsGroupName
INTEGER                                   :: NumRejTotal
INTEGER                                   :: TotalObsElement
INTEGER                                   :: TotalLatLonElement
INTEGER                                   :: TotalTimeElement
INTEGER                                   :: TotalFamilyElement
INTEGER                                   :: TotalSiteLatLonElement
INTEGER                                   :: istat
INTEGER                                   :: NumVarObsPE(0:nproc - 1)
INTEGER                                   :: LocalObsElement(0:nproc - 1)
INTEGER                                   :: LocalLatLonElement(0:nproc - 1)
INTEGER                                   :: LocalTimeElement(0:nproc - 1)
INTEGER                                   :: LocalFamilyElement(0:nproc - 1)
INTEGER                                   :: LocalSiteLatLonElement(0:nproc - 1)
INTEGER                                   :: NumRejPE(0:nproc - 1)
INTEGER                                   :: ObsArrayMaxSize
INTEGER                                   :: ObsArraySize
INTEGER                                   :: NumValidVarFields
REAL, ALLOCATABLE                         :: LatLons_temp(:)
REAL, ALLOCATABLE                         :: Times_temp(:)
INTEGER, ALLOCATABLE                      :: Family_temp(:)
REAL, ALLOCATABLE                         :: SiteLatLons_temp(:)
REAL, ALLOCATABLE                         :: ElementsPresent(:)
REAL, ALLOCATABLE                         :: ElementsLevels(:)
REAL, ALLOCATABLE                         :: ObsArray(:)
REAL, ALLOCATABLE                         :: VarArray(:)
REAL, ALLOCATABLE                         :: LatLons(:)
REAL, ALLOCATABLE                         :: SiteLatLons(:)
REAL, ALLOCATABLE                         :: Times(:)
INTEGER, ALLOCATABLE                      :: Family(:)
INTEGER                                   :: VarFields(ActualMaxVarfield)
INTEGER                                   :: varfield_array_size
TYPE (varobs_type)                        :: varobs
CHARACTER(len=MaxFileNameLen)             :: Filename
CHARACTER(len=80)                         :: StatsString
INTEGER                                   :: rc

!-------------------------------------------------------------------------------
! 0. Tracing and initialisation
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

ObsGroupName = Observations % header % ObsGroupName

IF (GeneralMode >= DebugMode .AND. mype == 0) THEN
  WRITE (MessageOut, '(A)') 'Creating VAR data for ' // TRIM (ObsGroupName)
END IF

NumVarObsPE(:) = 0
NumRejPE(:) = 0

!-------------------------------------------------------------------------------
! 1. Determine the type of file to be created and initialise file type
!    specific information.
!-------------------------------------------------------------------------------

varobs % fixhd(:) = CxHeader % FixHd(:)

CALL Ops_ReadVarobsControlNL (Observations % header % ObsGroup, &
                              Varfields)

NumMeta = LenCallSign + 4        ! Number of meta variables for VAR
IF (OpsFn_IsSatRadGroup (Observations % header % ObsGroup) .AND. Observations % header % ObsGroup /= ObsGroupGeoCloud) THEN
  ! Number of observation items for VAR
  NumItem = 1
ELSE
  NumItem = 3
END IF

varobs % FixHd(FH_IntCSize) = 49
varobs % FixHd(FH_RealCSize) = 34
varobs % FixHd(FH_ColDepCSize2) = 8
varobs % FixHd(FH_LookupSize1) = 128
varobs % FixHd(FH_LookupSize2) = nproc + 2
IF (RadFamily) THEN
  varobs % FixHd(FH_LookupSize2) = nproc + 3
END IF
IF (Observations % Header % ObsGroup == ObsGroupGPSRO .AND. GPSRO_TPD) THEN
  varobs % FixHd(FH_LookupSize2) = nproc + 3
END IF

IF (PRESENT (PGEBd)) THEN
  IF (GeneralMode >= DebugMode .AND. mype == 0) THEN
    WRITE (MessageOut, '(A)') 'Using PGEs updated by buddy check'
  END IF
END IF
CALL Ops_VarobPGEs (Observations, &
                    VarobPGEs,    &
                    PGEBd)

IF (GeneralMode >= DebugMode .AND. mype == 0) THEN
  WRITE (MessageOut, '(A)') 'PGE structure after Ops_VarobPGEs'
  CALL VarobPGEs % print
END IF

ALLOCATE (ElementsPresent(ActualMaxVarfield))
ALLOCATE (ElementsLevels(ActualMaxVarfield))
ElementsPresent(:) = IMDI
ElementsLevels(:) = IMDI

CALL Ops_CheckVarFields (VarFields,         & ! in
                         Observations,      & ! in
                         NumValidVarFields, & ! out
                         ObsArrayMaxSize,   & ! out
                         ElementsPresent,   & ! inout
                         ElementsLevels,    & ! inout
                         RepObs = RepObs)

varobs % FixHd(FH_LevDepCSize1) = NumObLev + 2
varobs % FixHd(FH_LevDepCSize2) = 1

IF (ObsArrayMaxSize > 0) THEN

  ALLOCATE (ObsArray(ObsArrayMaxSize))
  ALLOCATE (LatLons_temp(Observations % Header % NumObsLocal * 2))
  ALLOCATE (Times_temp(Observations % Header % NumObsLocal))
  IF (RadFamily) THEN
    ALLOCATE (Family_temp(Observations % Header % NumObsLocal))
  ELSE
    ALLOCATE (Family_temp(0))
  END IF
  IF (Observations % Header % ObsGroup == ObsGroupGPSRO .AND. GPSRO_TPD) THEN
    ALLOCATE (SiteLatLons_temp(Observations % Header % NumObsLocal * 2))
  ELSE
    ALLOCATE (SiteLatLons_temp(0))
  END IF
  CALL Ops_SetupVarArray (ObsArray,          & ! inout
                          LatLons_temp,      & ! inout
                          Times_temp,        & ! inout
                          Family_temp,       & ! inout
                          SiteLatLons_temp,  & ! inout
                          NumValidVarFields, & ! in
                          VarFields,         & ! in
                          Observations,      & ! in
                          VarobPGEs,         & ! in
                          NumVarObsPE(mype), & ! out
                          NumRejPE(mype),    & ! out
                          ObsArraySize,      & ! out
                          RepObs = RepObs)

  IF (ObsArraySize > 0) THEN
    ALLOCATE (LatLons(NumVarObsPE(mype) * 2))
    LatLons(:) = LatLons_temp(1:NumVarObsPE(mype) * 2)

    ALLOCATE (Times(NumVarObsPE(mype)))
    Times(:) = Times_temp(1:NumVarObsPE(mype))

    ALLOCATE (VarArray(ObsArraySize))
    VarArray(:) = ObsArray(1:ObsArraySize)

    IF (RadFamily) THEN
      ALLOCATE (Family(NumVarObsPE(mype)))
      Family(:) = Family_temp(1:NumVarObsPE(mype))
    ELSE
      ALLOCATE (Family(0))
    END IF
    IF (Observations % Header % ObsGroup == ObsGroupGPSRO .AND. GPSRO_TPD) THEN
      ALLOCATE (SiteLatLons(NumVarObsPE(mype) * 2))
      SiteLatLons(:) = SiteLatLons_temp(1:NumVarObsPE(mype) * 2)
    ELSE
      ALLOCATE (SiteLatLons(0))
    END IF

  ELSE
    ObsArraySize = 0
    NumVarObsPE(mype) = 0
    ALLOCATE (VarArray(0)) ! Needed for MPL calls.
    ALLOCATE (Times(0)) ! Needed for MPL calls.
    ALLOCATE (LatLons(0)) ! Needed for MPL calls.
    ALLOCATE (Family(0)) ! Needed for MPL calls.
    ALLOCATE (SiteLatLons(0))
  END IF
  ! PGEs no longer required, so deallocate
  CALL VarobPGEs % deallocate
  DEALLOCATE (LatLons_temp)
  DEALLOCATE (Times_temp)
  DEALLOCATE (ObsArray)
  DEALLOCATE (Family_temp)
  DEALLOCATE (SiteLatLons_temp)
ELSE

  ObsArraySize = 0
  NumVarObsPE(mype) = 0
  ALLOCATE (VarArray(0)) ! Needed for MPL calls.
  ALLOCATE (Times(0)) ! Needed for MPL calls.
  ALLOCATE (LatLons(0)) ! Needed for MPL calls.
  ALLOCATE (Family(0)) ! Needed for MPL calls.
  ALLOCATE (SiteLatLons(0)) ! Needed for MPL calls.

END IF

varfield_array_size = MAX (MAXVAL (VarFields), NumObVarMax)

varobs % FixHd(FH_ColDepCSize1) = varfield_array_size

!-------------------------------------------------------------------------------
! 2. Check whether requested data can be supplied.
!-------------------------------------------------------------------------------

LocalObsElement(:) = 0
LocalObsElement(mype) = ObsArraySize

CALL gcg_isum (nproc,           &
               mpi_group,       &
               istat,           &
               LocalObsElement)

TotalObsElement = SUM (LocalObsElement)

LocalLatLonElement(:) = 0
LocalLatLonElement(mype) = NumVarobsPE(mype) * 2

CALL gcg_isum (nproc,              &
               mpi_group,          &
               istat,              &
               LocalLatLonElement)

TotalLatLonElement = SUM (LocalLatLonElement)

LocalTimeElement(:) = 0
LocalTimeElement(mype) = NumVarobsPE(mype)

CALL gcg_isum (nproc,            &
               mpi_group,        &
               istat,            &
               LocalTimeElement)

TotalTimeElement = SUM (LocalTimeElement)

IF (RadFamily) THEN
  LocalFamilyElement(:) = 0
  LocalFamilyElement(mype) = NumVarobsPE(mype)

  CALL gcg_isum (nproc,              &
                 mpi_group,          &
                 istat,              &
                 LocalFamilyElement)

  TotalFamilyElement = SUM (LocalFamilyElement)

  TotalObsElement = TotalObsElement + &
                    TotalLatlonElement + &
                    TotalTimeElement + &
                    TotalFamilyElement
ELSE IF (Observations % Header % ObsGroup == ObsGroupGPSRO .AND. GPSRO_TPD) THEN
  LocalSiteLatLonElement(:) = 0
  LocalSiteLatLonElement(mype) = NumVarobsPE(mype) * 2

  CALL gcg_isum (nproc,                  &
                 mpi_group,              &
                 istat,                  &
                 LocalSiteLatLonElement)

  TotalSiteLatLonElement = SUM (LocalSiteLatLonElement)

  TotalObsElement = TotalObsElement + &
                    TotalLatlonElement + &
                    TotalTimeElement + &
                    TotalSiteLatLonElement
ELSE
  TotalObsElement = TotalObsElement + TotalLatlonElement + TotalTimeElement
END IF

CALL gcg_isum (nproc,       &
               mpi_group,   &
               istat,       &
               NumVarObsPE)

NumVarObsTotal = SUM (NumVarObsPE(:))

! Don't continue if there are no obs to write out
IF (NumVarObsTotal == 0) THEN
  CALL gen_warn (RoutineName,                                                                        &
                 "No observations to write out for " // TRIM (Observations % Header % ObsGroupName))
  GOTO 9999
END IF

CALL gcg_isum (nproc,     &
               mpi_group, &
               istat,     &
               NumRejPE)

NumRejTotal = SUM (NumRejPE(:))

IF (GeneralMode >= DebugMode .AND. mype == 0) THEN
  WRITE (MessageOut, '(I0,A)') NumVarObsTotal, ' observations output to varobs file'
  WRITE (MessageOut, '(I0,A)') NumRejTotal, ' observations not output to varobs file'
END IF

!-------------------------------------------------------------------------------
! 3. Construct the Varobs header.
!-------------------------------------------------------------------------------

! Set up the FIXED LENGTH header
CALL Ops_SetupVarobsFixhd (varobs,          &
                           TotalObsElement)

CALL varobs % alloc

! Set up the INTEGER header
CALL Ops_SetupVarobsIntC (Observations,        &  ! in - structure containing ob info
                          CxHeader,            &  ! in - Cx header structure
                          varobs,              &  ! inout - varobs header structure
                          NumVarObsTotal,      &  ! in - number of varobs to be written
                          varfield_array_size)

! Set up the REAL header
CALL Ops_SetupVarobsRealC (CxHeader, &
                           varobs)

! Set up the LEVEL DEPENDENT CONSTANTS header
CALL Ops_SetupVarobsLevDepC (Observations, &  ! in - structure containing ob info
                             varobs)          ! inout - varobs header structure

! Set up the LOOKUP header
CALL Ops_SetupVarobsLookup (Observations,    &
                            NumVarObsPE(:),  &
                            LocalObsElement, &   ! in - sizes of data on each PE
                            NumVarObsTotal,  &   ! in - total number of observations
                            varobs)              ! inout - varobs header structure

! Set up the COLUMN DEPENDENT CONSTANTS header
CALL Ops_SetupVarobsColDepC (Observations % header % ObsGroup,       &
                             varobs,                                 &
                             ElementsPresent(1:varfield_array_size), &
                             ElementsLevels(1:varfield_array_size))
DEALLOCATE (ElementsPresent)
DEALLOCATE (ElementsLevels)

!-------------------------------------------------------------------------------
! 4. Write varobs header and data to a file.
!-------------------------------------------------------------------------------

CALL Ops_WriteVarobs (varobs,          &
                      ObsGroupName,    &
                      NumVarObsPE(:),  &
                      VarArray,        &
                      LatLons,         &
                      Times,           &
                      Family,          &
                      SiteLatLons,     &
                      AssimDataFormat)

!-------------------------------------------------------------------------------
! 5. Exit.
!-------------------------------------------------------------------------------

9999 CONTINUE               ! Skip to here if there's an error

IF (mype == 0) THEN
  IF (ops_env_is_true ("OPS_CREATE_VAROBS_DONE_FILE")) THEN
    CALL ops_get_env ("OPS_VAROB_OUTPUT_DIR", &
                      Filename)
    IF (AssimDataFormat == AssimDataFormat_ModelOb) THEN
      Filename = TRIM (Filename) // "/" // TRIM (ObsGroupName) // ".modelobs.done"
    ELSE
      Filename = TRIM (Filename) // "/" // TRIM (ObsGroupName) // ".varobs.done"
    END IF
    CALL Ops_TouchFile (Filename)
  END IF

  CALL gen_open_stats_file ("VarObsWriter statistics", &
                            rc)
  IF (rc /= StatusOK) THEN
    CALL gen_fail (RoutineName,                    &
                   "Error in gen_open_stats_file")
  END IF
  WRITE (StatsOut, '(A)') &
          ' --------------------------------------------------------------------'
  StatsString = ' Total Varobs ' // TRIM (Observations % Header % ObsGroupName) // ' obs '
  WRITE (StatsString(35:44),'(A2,I8)') '= ', NumVarObsTotal

  WRITE (StatsOut, '(A)') TRIM (StatsString)
  WRITE (StatsOut, '(A)') &
        ' --------------------------------------------------------------------'
  WRITE (StatsOut, '(A)') ''

  CALL gen_close_stats_file (rc)
  IF (rc /= StatusOK) THEN
    CALL gen_fail (RoutineName,               &
                   "Cannot close stats file")
  END IF

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CreateVarobs
