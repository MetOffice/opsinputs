!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Creates a basic UM header to the start of each Cx file.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CreateCxUMHdr (Dump_UMhdr, &
                              MaxBatches, &
                              Cx_UMhdr)

USE GenMod_ClookAdd, ONLY: &
    BLEV,                  &
    LBLREC,                &
    LBPACK,                &
    DATA_TYPE

USE GenMod_Core, ONLY: &
    gen_fail,          &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI

USE GenMod_ModelIO, ONLY: &
    UM_header_type

USE GenMod_UMHeaderConstants, ONLY: &
    FH_Dataset,                     &
    FH_Dataset_CX,                  &
    FH_VTYear,                      &
    FH_VTMonth,                     &
    FH_VTDay,                       &
    FH_VTHour,                      &
    FH_VTMinute,                    &
    FH_VTDayNo,                     &
    FH_VTSecond,                    &
    FH_DTYear,                      &
    FH_DTMonth,                     &
    FH_DTDay,                       &
    FH_DTHour,                      &
    FH_DTMinute,                    &
    FH_DTDayNo,                     &
    FH_DTSecond,                    &
    FH_SubModel_Atmos,              &
    FH_Dataset_Ancil,               &
    IC_NumObsTotal

USE GenMod_UMHeaderConstants, ONLY: &
    FH_SubModel,                    &
    FH_SubModel_Ocean,              &
    IC_PLevels,                     &
    FH_RealCStart,                  &
    FH_RealCSize,                   &
    FH_IntCStart,                   &
    FH_IntCSize,                    &
    FH_LevDepCStart,                &
    FH_LevDepCSize1,                &
    FH_LevDepCSize2,                &
    FH_RowDepCStart,                &
    FH_RowDepCSize1,                &
    FH_RowDepCSize2,                &
    FH_ColDepCStart,                &
    FH_ColDepCSize1,                &
    FH_ColDepCSize2,                &
    FH_LookupStart,                 &
    FH_LookupSize1,                 &
    FH_LookupSize2,                 &
    FH_DataStart,                   &
    FH_DataSize

USE OpsMod_Control, ONLY: &
    CycleTime,            &
    cycle_year,           &
    cycle_month,          &
    cycle_day,            &
    cycle_hour

USE OpsMod_CXIndexes, ONLY: &
    IndexCXSFSize,          &
    IndexCXUASize

IMPLICIT NONE

! Subroutine arguments:
TYPE (UM_header_type), INTENT(INOUT) :: Dump_UMhdr
INTEGER, INTENT(IN)                  :: MaxBatches
TYPE (UM_header_type), INTENT(INOUT) :: Cx_UMhdr

! Local declarations:
CHARACTER(len=*), PARAMETER          :: RoutineName = "Ops_CreateCxUMHdr"
INTEGER                              :: Len1             ! Length 1 of Lev-Dep-Consts
INTEGER                              :: NumDumpLevs      ! Number of Model Levels in dump
INTEGER                              :: Position         ! locating components of COLDEPC
CHARACTER(len=80)                    :: ErrMess(2)

!-------------------------------------------------------------------------------
!  Section 0:  Tracing and Initialisation
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
!  Section 1.  Set up Cx file UM header.
!-------------------------------------------------------------------------------

!  Section 1.1  Copy Dump_UMhdr's fixed-length-header into Cx_UMhdr.
!--------------------------------------------------------------------
Cx_UMhdr % FixHd(:) = Dump_UMhdr % FixHd(:)
Cx_UMhdr % UnitNum = IMDI

NumDumpLevs = Dump_UMhdr % IntC(IC_PLevels)   ! No. of model levels
IF (NumDumpLevs <= 0) THEN
  ErrMess(1) = 'Invalid value of number of plevels set in inthd(8)'
  ErrMess(2) = 'Correct header in background fieldsfile'
  CALL gen_fail (RoutineName,  &
                 ErrMess(1:2))
END IF


!  Section 1.2  Amend Cx_UMhdr % FixHd (revised lengths/pointers)
!------------------------------------------------------------------
Cx_UMhdr % FixHd(FH_Dataset) = FH_Dataset_CX          ! Implies Cx file
Cx_UMhdr % FixHd(FH_DTYear) = CycleTime(cycle_year)
Cx_UMhdr % FixHd(FH_DTMonth) = CycleTime(cycle_month)
Cx_UMhdr % FixHd(FH_DTDay) = CycleTime(cycle_day)
Cx_UMhdr % FixHd(FH_DTHour) = CycleTime(cycle_hour)
Cx_UMhdr % FixHd(FH_DTMinute) = 0                ! Minute of data
Cx_UMhdr % FixHd(FH_DTSecond) = 0                ! Second of data
Cx_UMhdr % FixHd(FH_DTDayNo) = IMDI
Cx_UMhdr % FixHd(FH_VTYear) = CycleTime(cycle_year)
Cx_UMhdr % FixHd(FH_VTMonth) = CycleTime(cycle_month)
Cx_UMhdr % FixHd(FH_VTDay) = CycleTime(cycle_day)
Cx_UMhdr % FixHd(FH_VTHour) = CycleTime(cycle_hour)
Cx_UMhdr % FixHd(FH_VTMinute) = 0                ! Minute of validity
Cx_UMhdr % FixHd(FH_VTSecond) = 0                ! Second of validity
Cx_UMhdr % FixHd(FH_VTDayNo) = IMDI

! integer constants'
Cx_UMhdr % FixHd(FH_IntCSize) =  49

! real constants'
Cx_UMhdr % FixHd(FH_RealCStart) = Cx_UMhdr % FixHd(FH_IntCStart) + Cx_UMhdr % FixHd(FH_IntCSize)
Cx_UMhdr % FixHd(FH_RealCSize) = Dump_UMhdr % FixHd(FH_RealCSize)

! 'level dependant constants'
Cx_UMhdr % FixHd(FH_LevDepCStart) = Cx_UMhdr % FixHd(FH_RealCStart) + Cx_UMhdr % FixHd(FH_RealCSize)

Cx_UMhdr % FixHd(FH_LevDepCSize1) = NumDumpLevs + 2
Cx_UMhdr % FixHd(FH_LevDepCSize2) = 3

Cx_UMhdr % FixHd(FH_RowDepCStart) = IMDI
Cx_UMhdr % FixHd(FH_RowDepCSize1) = IMDI
Cx_UMhdr % FixHd(FH_RowDepCSize2) = IMDI

! 'column dependant constants'
Cx_UMhdr % FixHd(FH_ColDepCStart) = Cx_UMhdr % FixHd(FH_LevDepCStart) + &
        Cx_UMhdr % FixHd(FH_LevDepCSize1) * Cx_UMhdr % FixHd(FH_LevDepCSize2)

Cx_UMhdr % FixHd(FH_ColDepCSize1) = MAX (IndexCXSFSize,IndexCXUASize)
Cx_UMhdr % FixHd(FH_ColDepCSize2) = 8
Cx_UMhdr % FixHd(123:149) = IMDI

! lookup table
Cx_UMhdr % FixHd(FH_LookupStart) = Cx_UMhdr % FixHd(FH_ColDepCStart) + &
        Cx_UMhdr % FixHd(FH_ColDepCSize1) * Cx_UMhdr % FixHd(FH_ColDepCSize2)
Cx_UMhdr % FixHd(FH_LookupSize1) = 128
Cx_UMhdr % FixHd(FH_LookupSize2) = MaxBatches + 1

! data
Cx_UMhdr % FixHd(FH_DataStart) = Cx_UMhdr % FixHd(FH_LookupStart) + &
          Cx_UMhdr % FixHd(FH_LookupSize1) * Cx_UMhdr % FixHd(FH_LookupSize2)
Cx_UMhdr % FixHd(FH_DataSize) = 0

!  Section 1.3  Set up lengths, allocate arrays within Cx_UMhdr and set defaults
!-------------------------------------------------------------------------------

CALL Cx_UMhdr % alloc       ! inout

! Set default UMhdr, copy Dump_UMhdr arrays into Cx_UMhdr arrays.

Cx_UMhdr % IntC(:) = IMDI

Cx_UMhdr % IntC(1:Dump_UMhdr % FixHd(FH_IntCSize)) = Dump_UMhdr % IntC(1:Dump_UMhdr % FixHd(FH_IntCSize))
Cx_UMhdr % IntC(IC_NumObsTotal) = 0

Cx_UMhdr % RealC(1:Dump_UMhdr % FixHd(FH_RealCSize)) = Dump_UMhdr % RealC (1:Dump_UMhdr % FixHd(FH_RealCSize))

!  Section 1.4  Amend Cx_UMhdr % LevDepC
!------------------------------------------
Len1 = Dump_UMhdr % FixHd(FH_LevDepCSize1) !Number of levels
Cx_UMhdr % LevDepC(:) = 0.0 ! Obs levels not set

IF (Dump_UMhdr % FixHd(FH_SubModel) == FH_SubModel_Atmos .AND. & !Atmos dump
    Dump_UMhdr % FixHd(FH_Dataset) /= FH_Dataset_Ancil) THEN  !but not SST

  Len1 = Len1 - 1  ! Reduced by one as for ND the levDepConstants are
                   ! dimensioned 0:NumLevels

  ! Eta_Theta in dump is dimensioned 0:NumLevels, i.e. there are
  ! NumLevels+1 values. 1st value is for orography, i.e. 0, so miss
  ! this out.
  Position = Len1 + 2
  Cx_UMhdr % LevDepC(Position + 1) = 15         ! indicator for Eta_Theta
  Cx_UMhdr % LevDepC(Position + 2) = Len1
  Cx_UMhdr % LevDepC(Position + 3:Position + 2 + Len1) = Dump_UMhdr % LevDepC(2:Len1 + 1)

  ! Eta_Rho transferred as normal
  Position = 2 * (Len1 + 2)
  Cx_UMhdr % LevDepC(Position + 1) = 16         ! indicator for Eta_Rho
  Cx_UMhdr % LevDepC(Position + 2) = Len1
  Cx_UMhdr % LevDepC(Position + 3:Position + 2 + Len1) = Dump_UMhdr % LevDepC(Len1 + 2:(2 * Len1) + 1)

ELSE IF (Dump_UMhdr % FixHd(2) == FH_Submodel_Ocean) THEN         ! Ocean dump

  Position = Len1 + 2
  Cx_UMhdr % LevDepC(Position + 1) = 14         ! indicator for ocean
                                                  ! depth
  Cx_UMhdr % LevDepC(Position + 2) = Len1
  Cx_UMhdr % LevDepC(Position + 3:Position + 2 + Len1) = TRANSFER (Dump_UMhdr % Lookup(BLEV,1:Len1),0.0,Len1)
END IF

!  Section 1.5  Set up Cx_UMhdr % ColDepC (with pointers to Cx data)
!-----------------------------------------------------------------------
Cx_UMhdr % ColDepC(:) = IMDI   ! Initialise to missing data

!  Section 1.6  Set up LOOKUP etc and exit
!-------------------------------------------
Cx_UMhdr % NumFlds = 0
Cx_UMhdr % MaxFldSize = 0
Cx_UMhdr % Lookup(:,:) = IMDI
Cx_UMhdr % Lookup(DATA_TYPE,:) = 1
Cx_UMhdr % Lookup(LBLREC,:) = 0
Cx_UMhdr % Lookup(LBPACK,:) = 0

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CreateCxUMHdr
