!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Write varobs header and data to an external file.  All output is done on PE 0.
! The varobs file is opened then the Varobs header (argument VarobsHeader) is
! written.
!
! The data is gathered via MPL_GATHERV on PE 0 and written batch-by-batch, each
! PE's obs corresponding to one batch.  Then the observation latitudes and
! longitudes are written as another batch followed by the observation times
! written as a further batch.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_WriteVarobs (Varobs,          & ! Structure containing varobs header info.
                            FilenamePrefix,  & ! Prefix for assimilation data file name
                            NumVarObsPE,     & ! Array of number of varobs to be written
                            VarArray,        &
                            LatLons,         &
                            Times,           &
                            Family,          &
                            SiteLatLons,     &
                            AssimDataFormat)

USE GenMod_Control, ONLY: &
    DebugMode,            &
    GeneralMode,          &
    GenFn_UMSectorLen,    &
    mype,                 &
    nproc

USE GenMod_Core, ONLY: &
    gen_fail,          &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE mpl, ONLY:      &
    mpl_comm_world, &
    mpl_real,       &
    mpl_success

USE OpsMod_Control, ONLY: &
    mpi_group

USE OpsMod_Gcom, ONLY: &
    gcg_isum,          &
    gcg_rsum

USE OpsMod_GPSRO, ONLY: &
    GPSRO_TPD

USE OpsMod_IOUtils, ONLY: &
    Ops_CloseCFile,       &
    Ops_OpenCEnvNew

USE OpsMod_MPLInterface, ONLY: &
    ops_mpl_gatherv

USE OpsMod_Radar, ONLY: &
    RadFamily

USE OpsMod_VarobsLib, ONLY: &
    varobs_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (varobs_type), INTENT(INOUT) :: Varobs
CHARACTER(len=*), INTENT(IN)      :: FilenamePrefix
INTEGER, INTENT(IN)               :: NumVarObsPE(0:nproc - 1)
REAL, INTENT(IN)                  :: VarArray(:)
REAL, INTENT(IN)                  :: LatLons(:)
REAL, INTENT(IN)                  :: Times(:)
INTEGER, INTENT(IN)               :: Family(:)
REAL, INTENT(IN)                  :: SiteLatLons(:)
INTEGER, INTENT(IN)               :: AssimDataFormat

! Local declarations:
INTEGER                           :: istat
INTEGER                           :: j
INTEGER                           :: i
INTEGER                           :: ObsPointer
INTEGER                           :: VarArraySize(0:nproc - 1)
INTEGER                           :: NumVarObsTotal
CHARACTER(len=9)                  :: FilenameSuffix   ! Suffix for file name
CHARACTER(len=80)                 :: ErrMess          ! Message for gen_fail
REAL, ALLOCATABLE                 :: LatLons_temp(:)
REAL, ALLOCATABLE                 :: Times_temp(:)
INTEGER, ALLOCATABLE              :: Family_temp(:)
REAL, ALLOCATABLE                 :: SiteLatLons_temp(:)
INTEGER                           :: displacements(0:nproc - 1)
REAL, ALLOCATABLE                 :: VarArray_temp(:)
REAL, ALLOCATABLE                 :: LatLons_temp2(:)
REAL, ALLOCATABLE                 :: Times_temp2(:)
INTEGER, ALLOCATABLE              :: Family_temp2(:)
REAL, ALLOCATABLE                 :: SiteLatLons_temp2(:)
CHARACTER(len=*), PARAMETER       :: RoutineName = "Ops_WriteVarobs"

!-------------------------------------------------------------------------------
!  Section 0. Tracing
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
!  Section 1. Assign unit number to Varobs file
!-------------------------------------------------------------------------------

IF (mype == 0) THEN

  IF (AssimDataFormat == AssimDataFormat_ModelOb) THEN
    FilenameSuffix = ".modelobs"
  ELSE
    FilenameSuffix = ".varobs"
  END IF

  CALL Ops_OpenCEnvNew ("OPS_VAROB_OUTPUT_DIR",                                    &
                        varobs % unit_num,                                         &
                        filename = TRIM (FilenamePrefix) // TRIM (FilenameSuffix))

  !-----------------------------------------------------------------------------
  !  Section 4.  Write out header to the varobs file.
  !-----------------------------------------------------------------------------

  CALL varobs % write_head

END IF ! mype==0

!-------------------------------------------------------------------------------
!  Section 5.  Write out the Varobs data from each PE in turn
!-------------------------------------------------------------------------------

VarArraySize(:) = 0
VarArraySize(mype) = SIZE (VarArray)

CALL gcg_isum (nproc,           &
               mpi_group,       &
               istat,           &
               VarArraySize(:))

! handle each PE's data in turn

displacements(0) = 0
DO j = 1, nproc - 1
  displacements(j) = displacements(j - 1) + GenFn_UMSectorLen (VarArraySize(j - 1) - 1)
END DO
IF (mype == 0) THEN
  ALLOCATE (VarArray_temp(displacements(nproc - 1) + GenFn_UMSectorLen (VarArraySize(nproc - 1) - 1)))
ELSE
  ALLOCATE (VarArray_temp(1))
END IF

VarArray_temp = RMDI

CALL ops_mpl_gatherv (VarArray,           &
                      VarArraySize(mype), &
                      mpl_real,           &
                      VarArray_temp,      &
                      VarArraySize,       &
                      displacements,      &
                      mpl_real,           &
                      0,                  &
                      mpl_comm_world,     &
                      istat)
IF (istat /= mpl_success) THEN
  WRITE (ErrMess, '(A,I0)') "Error in mpl_gatherv, istat = ", istat
  CALL gen_fail (RoutineName, &
                 ErrMess)
END IF

IF (mype == 0) THEN
  DO j = 0, nproc - 1
    IF (VarArraySize(j) == 0) CYCLE

    CALL varobs % write_batch (j + 1,                                                                  &
                               VarArray_temp(displacements(j) + 1:displacements(J) + VarArraySize(j)))

  END DO
  DEALLOCATE (VarArray_temp)
END IF

! Collect observation positions together for and write to last field

! Temporary workspace

NumVarObsTotal = SUM (NumVarObsPE(:))

ALLOCATE (LatLons_temp(2 * NumVarObsTotal))
ALLOCATE (Times_temp(NumVarObsTotal))
LatLons_temp(:) = 0.0
Times_temp(:) = 0.0

IF (RadFamily) THEN
  ALLOCATE (Family_temp(1:NumVarObsTotal))
  Family_temp(:) = 0
END IF

IF (FilenamePrefix == "GPSRO" .AND. GPSRO_TPD) THEN
  ALLOCATE (SiteLatLons_temp(2 * NumVarObsTotal))
  SiteLatLons_temp(:) = 0
END IF

ObsPointer = 1
DO i = 0, nproc - 1
  IF (mype == i .AND. NumVarObsPE(i) > 0) THEN
    LatLons_temp(ObsPointer:ObsPointer + NumVarObsPE(i) * 2 - 1) = LatLons(1:NumVarObsPE(i) * 2)
  END IF
  ObsPointer = ObsPointer + NumVarObsPE(i) * 2
END DO

CALL gcg_rsum (NumVarObsTotal * 2, &
               mpi_group,          &
               istat,              &
               LatLons_temp(:))

ObsPointer = 1
DO i = 0, nproc - 1
  IF (mype == i .AND. NumVarObsPE(i) > 0) THEN
    Times_temp(ObsPointer:ObsPointer + NumVarObsPE(i) - 1) = Times(1:NumVarObsPE(i))
  END IF
  ObsPointer = ObsPointer + NumVarObsPE(i)
END DO

CALL gcg_rsum (NumVarObsTotal, &
               mpi_group,      &
               istat,          &
               Times_temp(:))


IF (RadFamily) THEN
  ObsPointer = 1
  DO i = 0, nproc - 1
    IF (mype == i .AND. NumVarObsPE(i) > 0) THEN
      Family_temp(ObsPointer:ObsPointer + NumVarObsPE(i) - 1) = Family(1:NumVarObsPE(i))
    END IF
    ObsPointer = ObsPointer + NumVarObsPE(i)
  END DO

  CALL gcg_isum (NumVarObsTotal, &
                 mpi_group,      &
                 istat,          &
                 Family_temp(:))
END IF

IF (FilenamePrefix == "GPSRO" .AND. GPSRO_TPD) THEN
  ObsPointer = 1
  DO i = 0, nproc - 1
    IF (mype == i .AND. NumVarObsPE(i) > 0) THEN
      SiteLatLons_temp(ObsPointer:ObsPointer + NumVarObsPE(i) * 2 - 1) = SiteLatLons(1:NumVarObsPE(i) * 2)
    END IF
    ObsPointer = ObsPointer + NumVarObsPE(i) * 2
  END DO

  CALL gcg_rsum (NumVarObsTotal * 2,  &
                 mpi_group,           &
                 istat,               &
                 SiteLatLons_temp(:))
END IF

IF (mype == 0) THEN

  ALLOCATE (LatLons_temp2(GenFn_UMSectorLen (SIZE (LatLons_temp) - 1)))

  IF (GeneralMode >= DebugMode) THEN
    WRITE (MessageOut, '(A,I0,A,I0)') 'LatLon array resized from ', &
                                 SIZE (LatLons_temp), ' to ', SIZE (LatLons_temp2)
  END IF

  ALLOCATE (Times_temp2(GenFn_UMSectorLen (SIZE (Times_temp) - 1)))

  IF (GeneralMode >= DebugMode) THEN
    WRITE(MessageOut, '(A, I0, A, I0)') 'Times array resized from ', &
                                     SIZE (Times_temp), ' to ', SIZE (Times_temp2)
  END IF

  Latlons_temp2(:) = 0.0
  LatLons_temp2(1:SIZE (LatLons_temp)) = LatLons_temp(1:SIZE (LatLons_temp))

  Times_temp2(:) = 0.0
  Times_temp2(1:SIZE (Times_temp)) = Times_temp(1:SIZE (Times_temp))

  IF (RadFamily) THEN
    ALLOCATE (Family_temp2(GenFn_UMSectorLen (SIZE (Family_temp) - 1)))
    Family_temp2(:) = 0.0
    Family_temp2(1:SIZE (Family_temp)) = Family_temp(1:SIZE (Family_temp))
  END IF

  IF (FilenamePrefix == "GPSRO" .AND. GPSRO_TPD) THEN
    ALLOCATE (SiteLatLons_temp2(GenFn_UMSectorLen (SIZE (SiteLatLons_temp) - 1)))
    SiteLatLons_temp2(:) = 0.0
    SiteLatLons_temp2(1:SIZE (SiteLatLons_temp)) = SiteLatLons_temp(1:SIZE (SiteLatLons_temp))
  END IF

  IF (SIZE (LatLons_temp2) > 0) THEN
    CALL varobs % write_batch (nproc + 1,     &  ! in  Field number
                               LatLons_temp2)    ! in  1D varobs data array
  END IF

  IF (SIZE (Times_temp2) > 0) THEN
    CALL varobs % write_batch (nproc + 2,   &  ! in  Field number
                               Times_temp2)    ! in  1D varobs data array
  END IF

  IF (RadFamily) THEN
    IF (SIZE (Family_temp2) > 0) THEN
      CALL varobs % write_batch (nproc + 3,           &  ! in  Field number
                                 REAL (Family_temp2))    ! in  1D varobs data array
    END IF
  END IF

  IF (FilenamePrefix == "GPSRO" .AND. GPSRO_TPD) THEN
    IF (SIZE (SiteLatLons_temp2) > 0) THEN
      CALL varobs % write_batch (nproc + 3,         &  ! in  Field number
                                 SiteLatLons_temp2)    ! in  1D varobs data array
    END IF
  END IF

END IF

!-------------------------------------------------------------------------------
!  Section 6.  Close external file
!-------------------------------------------------------------------------------

IF (mype == 0) THEN
  CALL Ops_CloseCFile (varobs % unit_num)
END IF

!-------------------------------------------------------------------------------
! Section 10. Tracing and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_WriteVarobs
