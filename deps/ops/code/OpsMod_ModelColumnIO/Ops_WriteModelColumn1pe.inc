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

SUBROUTINE Ops_WriteModelColumn1pe (NumCx,      &
                                    Dump_UMhdr, &
                                    Cx,         &
                                    Cx_UMhdr)

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

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI,                                &
    RMDI

USE GenMod_ModelIO, ONLY: &
    UM_header_type

USE GenMod_UMHeaderConstants, ONLY: &
    FH_DataSize,                    &
    IC_LenCxCol,                    &
    IC_NumObsTotal

USE OpsMod_CXInfo, ONLY: &
    Cx_type

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)                  :: NumCx(:)       ! Number of Cx columns
TYPE (UM_header_type), INTENT(IN)    :: Dump_UMhdr     ! Model UM header
TYPE (Cx_type), INTENT(IN)           :: Cx
TYPE (UM_header_type), INTENT(INOUT) :: Cx_UMhdr       ! Cx UM header

! Local declarations:
INTEGER                              :: i
REAL, ALLOCATABLE                    :: CxData(:,:)   ! 1D array for output of data
CHARACTER(len=*), PARAMETER          :: RoutineName = "Ops_WriteModelColumn1pe"
CHARACTER(len=*), PARAMETER          :: TypeOfDump = "Cx "
INTEGER                              :: NewDataSize

!-------------------------------------------------------------------------------
!  Section 0.1:  Tracing and Initialisation
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
!  Section 1:  Initial setup of model column
!-------------------------------------------------------------------------------

ALLOCATE (CxData(SIZE (NumCx),Cx_UMhdr % IntC(IC_LenCxCol) * MAXVAL (NumCx)))
CxData = RMDI

DO i = 1, SIZE (NumCx)
  CALL Ops_SetupModelColumn1pe (NumCx,    &
                                Cx,       &
                                Cx_UMhdr, &
                                CxData,   &
                                i)
END DO

Cx_UMhdr % IntC(IC_NumObsTotal) = SUM (NumCx(:))

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

DO i = 1, SIZE (NumCx)
  Cx_UMhdr % Lookup(LBYR:LBFT,i) = Dump_UMhdr % Lookup(LBYR:LBFT,1)
  Cx_UMhdr % Lookup(LBLREC,i) = NumCx(i) * Cx_UMhdr % IntC(IC_LenCxCol)
  Cx_UMhdr % Lookup(LBNPT,i) = NumCx(i)
  Cx_UMhdr % Lookup(VarobsLookupNumObs,i) = NumCx(i)
END DO

!-------------------------------------------------------------------------------
!  Section 3.  Write out Cx header to the Cx file.
!-------------------------------------------------------------------------------

CALL Cx_UMhdr % write (TypeOfDump,            &
                       CheckLookup = .FALSE.)

!-------------------------------------------------------------------------------
!  Section 6.  Write batches of CX data.
!-------------------------------------------------------------------------------

NewDataSize = 0 ! initialize FH_DaraSize parameter
DO i = 1, SIZE (NumCx)

  ! Now that length of field is known on Pe 0, set disk address and length
  ! to anable well formed I/O
  NewDataSize = NewDataSize + NumCx(i) * Cx_UMhdr % IntC(IC_LenCxCol)
  Cx_UMhdr % FixHd(FH_DataSize) = NewDataSize

  IF (NumCx(i) > 0) THEN

    CALL Cx_UMhdr % write_field (i,           &  ! in  Field number to start at
                                 CxData(i,:))    ! in

  END IF

END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_WriteModelColumn1pe
