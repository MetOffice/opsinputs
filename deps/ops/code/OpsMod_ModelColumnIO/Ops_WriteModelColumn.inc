!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Writes columns of CX data to a .cx file.  The Cx data arrays are assumed to be
! present and required to be output, if within the Cx structure's header they
! have a present = .true.  The order in which the fields are stored in the
! output array is the same order as their index pointers, defined in
! OpsMod_CXIndexes i.e. for surface fields :
!         1. Orography (if present)
!         2. pstar (if present)
!         3. theta2 (if present)
!         4. etc.....
!
!  The data to be written is copied into "CxData" which is allocated and set up
!  in Ops_SetupModelColumn.  Each processor sends its data to PE 0 which then
!  writes each column in turn.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_WriteModelColumn (NumCx,      &
                                 Dump_UMhdr, &
                                 Cx,         &
                                 Cx_UMhdr,   &
                                 mask)

USE GenMod_ClookAdd, ONLY: &
    LBYR,                  &
    LBFT,                  &
    LBLREC,                &
    LBROW,                 &
    LBNPT,                 &
    LBEXT,                 &
    LBNREC,                &
    DATA_TYPE,             &
    ITEM_CODE,             &
    LBPACK,                &
    VarobsLookupNumObs

USE GenMod_Control, ONLY: &
    mype,                 &
    nproc

USE GenMod_Core, ONLY: &
    gen_fail,          &
    gen_trace_entry,   &
    gen_trace_exit,    &
    StatusOK,          &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI

USE GenMod_ModelIO, ONLY:   &
    LenFixHd,               &
    Ops_SetDumpfileAddress, &
    UM_header_type

USE GenMod_UMHeaderConstants, ONLY: &
    FH_DataSize,                    &
    FH_LookupSize1,                 &
    FH_LookupSize2,                 &
    IC_LenCxCol

USE mpl, ONLY:      &
    mpl_comm_world, &
    mpl_integer,    &
    mpl_real,       &
    mpl_success

USE OpsMod_Control, ONLY: &
    mpi_group

USE OpsMod_CXInfo, ONLY: &
    Cx_type

USE OpsMod_Gcom, ONLY: &
    gcg_isum

USE OpsMod_MPLInterface, ONLY: &
    ops_mpl_gatherv

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)                  :: NumCx(:)       ! Number of Cx columns
TYPE (UM_header_type), INTENT(IN)    :: Dump_UMhdr     ! Model UM header
TYPE (Cx_type), INTENT(IN)           :: Cx
TYPE (UM_header_type), INTENT(INOUT) :: Cx_UMhdr       ! Cx UM header
LOGICAL, OPTIONAL, INTENT(IN)        :: mask(:)

! Local declarations:
INTEGER                              :: ErrStat        ! Error status from subr calls
INTEGER                              :: len_data
INTEGER                              :: PeLoop
REAL, ALLOCATABLE                    :: CxData_temp(:)
INTEGER                              :: mpl_len(0:nproc - 1)
INTEGER                              :: data_len(0:nproc - 1)
INTEGER                              :: mpl_displacements(0:nproc - 1)
INTEGER                              :: i
INTEGER                              :: MaxFldSizes(0:nproc - 1)
CHARACTER(len=80)                    :: ErrMess       ! Message for gen_fail
REAL, ALLOCATABLE                    :: CxData(:)     ! 1D array for output of data
CHARACTER(len=*), PARAMETER          :: RoutineName = "Ops_WriteModelColumn"
CHARACTER(len=*), PARAMETER          :: TypeOfDump = "Cx "
INTEGER                              :: NewDataSize
INTEGER                              :: num_cxes_all(0:nproc - 1)

!-------------------------------------------------------------------------------
!  Section 0.1:  Tracing and Initialisation
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

ErrStat = StatusOK

!-------------------------------------------------------------------------------
!  Section 1:  Initial setup of model column
!-------------------------------------------------------------------------------

CALL Ops_SetupModelColumn (NumCx,        &
                           Cx,           &
                           Cx_UMhdr,     &
                           CxData,       &
                           num_cxes_all, &
                           mask = mask)

!-------------------------------------------------------------------------------
!  Section 2.  Set up lookup headers
!-------------------------------------------------------------------------------

Cx_UMhdr % Lookup(:,:) = IMDI
Cx_UMhdr % Lookup(DATA_TYPE,:) = 1
Cx_UMhdr % Lookup(LBLREC,:) = 0
Cx_UMhdr % Lookup(LBPACK,:) = 0
Cx_UMhdr % Lookup(LBROW,:) = Cx_UMhdr % IntC(IC_LenCxCol)
Cx_UMhdr % Lookup(LBEXT,:) = 0
Cx_UMhdr % Lookup(LBNREC,:) = 0
Cx_UMhdr % Lookup(DATA_TYPE,:) = 1
Cx_UMhdr % Lookup(ITEM_CODE,:) = 1

DO i = 1, nproc
  Cx_UMhdr % Lookup(LBYR:LBFT,i) = Dump_UMhdr % Lookup(LBYR:LBFT,1)
  Cx_UMhdr % Lookup(LBLREC,i) = num_cxes_all(i - 1) * Cx_UMhdr % IntC(IC_LenCxCol)
  Cx_UMhdr % Lookup(LBNPT,i) = num_cxes_all(i - 1)
  Cx_UMhdr % Lookup(VarobsLookupNumObs,i) = num_cxes_all(i - 1)
END DO

!-------------------------------------------------------------------------------
!  Section 3.  Write out Cx header to the Cx file.
!-------------------------------------------------------------------------------

IF (mype == 0) CALL Cx_UMhdr % write (TypeOfDump,            &
                                      CheckLookup = .FALSE.)

!-------------------------------------------------------------------------------
!  Section 4.  Send data lengths to pe 0
!-------------------------------------------------------------------------------

len_data = SIZE (CxData)
mpl_len = 1
DO i = 0, nproc - 1
  mpl_displacements(i) = i
END DO

CALL ops_mpl_gatherv ((/len_data/),      &
                      1,                 &
                      mpl_integer,       &
                      data_len,          &
                      mpl_len,           &
                      mpl_displacements, &
                      mpl_integer,       &
                      0,                 &
                      mpl_comm_world,    &
                      Errstat)
IF (Errstat /= mpl_success) THEN
  WRITE (ErrMess, '(A,I0)') "Error in mpl_gatherv (CX lengths), Errstat = ", Errstat
  CALL gen_fail (RoutineName, &
                 ErrMess)
END IF

!-------------------------------------------------------------------------------
!  Section 5.  Send CX data to pe 0
!-------------------------------------------------------------------------------

IF (mype == 0) THEN
  ALLOCATE (CxData_temp(SUM (data_len)))
  mpl_displacements(0) = 0
  DO i = 1, nproc - 1
    mpl_displacements(i) = mpl_displacements(i - 1) + data_len(i - 1)
  END DO
ELSE
  ALLOCATE (CxData_temp(1))
  mpl_displacements = 0
END IF

CALL ops_mpl_gatherv (CxData,            &
                      len_data,          &
                      mpl_real,          &
                      CxData_temp,       &
                      data_len,          &
                      mpl_displacements, &
                      mpl_real,          &
                      0,                 &
                      mpl_comm_world,    &
                      Errstat)
IF (Errstat /= mpl_success) THEN
  WRITE (ErrMess, '(A,I0)') "Error in mpl_gatherv (CX data), Errstat = ", Errstat
  CALL gen_fail (RoutineName, &
                 ErrMess)
END IF

MaxFldSizes = 0
MaxFldSizes(mype) = Cx_UMHdr % MaxFldSize
CALL gcg_isum (nproc,       &
               mpi_group,   &
               ErrStat,     &
               MaxFldSizes)

!-------------------------------------------------------------------------------
!  Section 6.  Write batches of CX data.
!-------------------------------------------------------------------------------

IF (mype == 0) THEN
  NewDataSize = 0 ! initialize FH_DataSize parameter
  DO PeLoop = 0, nproc - 1

    ! Now that length of field is known on Pe 0, set disk address and length
    ! to anable well formed I/O
    NewDataSize = NewDataSize + NumCx(PeLoop + 1) * Cx_UMhdr % IntC(IC_LenCxCol)
    Cx_UMhdr % FixHd(FH_DataSize) = NewDataSize
    CALL Ops_SetDumpfileAddress (Cx_UMhdr % FixHd,                 &
                                 LenFixHd,                         &
                                 Cx_UMhdr % Lookup,                &
                                 Cx_UMhdr % FixHd(FH_LookupSize1), &
                                 Cx_UMhdr % FixHd(FH_LookupSize2))

    IF (data_len(PeLoop) > 0) THEN
      CALL Cx_UMhdr % write_field (PeLoop + 1,                                  &  ! in  Field number to start at
                                   CxData_temp(mpl_displacements(PeLoop) + 1:))    ! in

    END IF

  END DO
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_WriteModelColumn
