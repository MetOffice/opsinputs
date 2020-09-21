!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! A CX file is opened and parts of the CX header structure are set up.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_OpenModelColumn (Obs,             & ! Observations
                                Dump_UMhdr,      & ! UM header from model dump
                                Cx_UMhdr,        & ! UM header for obs file
                                MaxSurfaceIndex, &
                                MaxUairIndex,    &
                                batches)

USE GenMod_Control, ONLY: &
    mype,                 &
    nproc

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_ModelIO, ONLY: &
    UM_header_type

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI,                                &
    RMDI

USE GenMod_UMHeaderConstants, ONLY: &
    FH_ColDepCSize1,                &
    FH_ColDepCSize2,                &
    FH_ColDepCStart,                &
    FH_Dataset,                     &
    FH_Dataset_Ancil,               &
    FH_Dataset_CX,                  &
    FH_DataSize,                    &
    FH_DataStart,                   &
    FH_DTDay,                       &
    FH_DTDayNo,                     &
    FH_DTHour,                      &
    FH_DTMinute,                    &
    FH_DTMonth,                     &
    FH_DTSecond,                    &
    FH_DTYear,                      &
    FH_IntCSize,                    &
    FH_IntCStart,                   &
    FH_LevDepCSize1,                &
    FH_LevDepCSize2,                &
    FH_LevDepCStart,                &
    FH_LookupSize1,                 &
    FH_LookupSize2,                 &
    FH_LookupStart,                 &
    FH_RealCSize,                   &
    FH_RealCStart,                  &
    FH_RowDepCSize1,                &
    FH_RowDepCSize2,                &
    FH_RowDepCStart,                &
    FH_SubModel,                    &
    FH_SubModel_Atmos,              &
    FH_VTDay,                       &
    FH_VTDayNo,                     &
    FH_VTHour,                      &
    FH_VTMinute,                    &
    FH_VTMonth,                     &
    FH_VTSecond,                    &
    FH_VTYear,                      &
    IC_NumObsTotal,                 &
    IC_PLevels

USE OpsMod_IOUtils, ONLY: &
    Ops_OpenCEnvNew

USE OpsMod_ObsGroupInfo, ONLY: &
    OpsFn_ObsGroupNumToName

USE OpsMod_ObsInfo, ONLY: &
    Ob_type

IMPLICIT NONE

! Subroutine arguments
TYPE (Ob_type), INTENT(IN)            :: Obs
TYPE (UM_header_type), INTENT (INOUT) :: Dump_UMhdr
TYPE (UM_header_type), INTENT (INOUT) :: Cx_UMhdr
INTEGER, INTENT(IN)                   :: MaxSurfaceIndex
INTEGER, INTENT(IN)                   :: MaxUairIndex
INTEGER, OPTIONAL, INTENT(IN)         :: batches

! Local declarations:
INTEGER                               :: Len1         ! Length 1 of Lev-Dep-Consts
INTEGER                               :: i
INTEGER                               :: NumDumpLevs      ! Number of Model Levels in dump
INTEGER                               :: Position         ! locating components of COLDEPC
CHARACTER(len=*), PARAMETER           :: RoutineName = "Ops_OpenModelColumn"
INTEGER                               :: local_batches

!-------------------------------------------------------------------------------
!  Section 0.1:  Tracing and Initialisation
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (PRESENT (batches)) THEN
  local_batches = batches + 1
ELSE
  local_batches = nproc + 1
END IF

!-------------------------------------------------------------------------------
!  Section 1.  Assign unit number to, and then open, Cx file.
!-------------------------------------------------------------------------------

IF (mype == 0) THEN
  CALL Ops_OpenCEnvNew ("OPS_CX_DIR_LIST",                                                            &
                        Cx_UMhdr % UnitNum,                                                           &
                        filename = TRIM (OpsFn_ObsGroupNumToName (Obs % Header % ObsGroup)) // ".cx")
END IF

!-------------------------------------------------------------------------------
!  Section 2  Copy Dump_UMhdr's fixed-length-header into Cx_UMhdr.
!-------------------------------------------------------------------------------

Cx_UMhdr % FixHd(:) = Dump_UMhdr % FixHd(:)
NumDumpLevs = Dump_UMhdr % IntC(IC_PLevels)   ! No. of model levels

!-------------------------------------------------------------------------------
!  Section 3  Amend Cx_UMhdr % FixHd (revised lengths/pointers)
!-------------------------------------------------------------------------------

Cx_UMhdr % FixHd(FH_Dataset) = FH_Dataset_CX          ! Implies Cx file
Cx_UMhdr % FixHd(FH_DTYear) = Obs % header % ValidityTime % year
Cx_UMhdr % FixHd(FH_DTMonth) = Obs % header % ValidityTime % month
Cx_UMhdr % FixHd(FH_DTDay) = Obs % header % ValidityTime % day
Cx_UMhdr % FixHd(FH_DTHour) = Obs % header % ValidityTime % hour
Cx_UMhdr % FixHd(FH_DTMinute) = 0                ! Minute of data
Cx_UMhdr % FixHd(FH_DTSecond) = 0                ! Second of data
Cx_UMhdr % FixHd(FH_DTDayNo) = IMDI
Cx_UMhdr % FixHd(FH_VTYear) = Obs % header % ValidityTime % year
Cx_UMhdr % FixHd(FH_VTMonth) = Obs % header % ValidityTime % month
Cx_UMhdr % FixHd(FH_VTDay) = Obs % header % ValidityTime % day
Cx_UMhdr % FixHd(FH_VTHour) = Obs % header % ValidityTime % hour
Cx_UMhdr % FixHd(FH_VTMinute) = 0                ! Minute of validity
Cx_UMhdr % FixHd(FH_VTSecond) = 0                ! Second of validity
Cx_UMhdr % FixHd(FH_VTDayNo) = IMDI
Cx_UMhdr % FixHd(FH_IntCSize) = 49
Cx_UMhdr % FixHd(FH_RealCStart) = Cx_UMhdr % FixHd(FH_IntCStart) + &
                                       Cx_UMhdr % FixHd(FH_IntCSize)
Cx_UMhdr % FixHd(FH_RealCSize) =  Dump_UMhdr % FixHd(FH_RealCSize)
Cx_UMhdr % FixHd(FH_LevDepCStart) = Cx_UMhdr % FixHd(FH_RealCStart) + &
                                         Cx_UMhdr % FixHd(FH_RealCSize)
Cx_UMhdr % FixHd(FH_LevDepCSize1) = NumDumpLevs + 2
Cx_UMhdr % FixHd(FH_LevDepCSize2) = 3
Cx_UMhdr % FixHd(FH_RowDepCStart) = IMDI
Cx_UMhdr % FixHd(FH_RowDepCSize1) = IMDI
Cx_UMhdr % FixHd(FH_RowDepCSize2) = IMDI
Cx_UMhdr % FixHd(FH_ColDepCStart) = Cx_UMhdr % FixHd(FH_LevDepCStart) + &
                                         Cx_UMhdr % FixHd(FH_LevDepCSize1) * &
                                         Cx_UMhdr % FixHd(FH_LevDepCSize2)
Cx_UMhdr % FixHd(FH_ColDepCSize1) = MAX (MaxSurfaceIndex, MaxUairIndex)
Cx_UMhdr % FixHd(FH_ColDepCSize2) = 8
Cx_UMhdr % FixHd(123:149) = IMDI
Cx_UMhdr % FixHd(FH_LookupStart) = Cx_UMhdr % FixHd(FH_ColDepCStart) + &
                                        Cx_UMhdr % FixHd(FH_ColDepCSize1) * &
                                        Cx_UMhdr % FixHd(FH_ColDepCSize2)
Cx_UMhdr % FixHd(FH_LookupSize1) = 128
Cx_UMhdr % FixHd(FH_LookupSize2) = local_batches
Cx_UMhdr % FixHd(FH_DataStart) = Cx_UMhdr % FixHd(FH_LookupStart) + &
                                 Cx_UMhdr % FixHd(FH_LookupSize1) * &
                                 Cx_UMhdr % FixHd(FH_LookupSize2)
Cx_UMhdr % FixHd(FH_DataSize) = 0

!-------------------------------------------------------------------------------
!  Section 4  Set up lengths, allocate arrays within Cx_UMhdr and set defaults
!-------------------------------------------------------------------------------

CALL Cx_UMhdr % alloc

! Set default UMhdr, copy Dump_UMhdr arrays into Cx_UMhdr arrays.

Cx_UMhdr % IntC(:) = IMDI

Cx_UMhdr % IntC(1:Dump_UMhdr % FixHd(FH_IntCSize))  =  &
                    Dump_UMhdr % IntC  (1:Dump_UMhdr % FixHd(FH_IntCSize))
Cx_UMhdr % IntC(IC_NumObsTotal)  =  0
Cx_UMhdr % RealC(1:Dump_UMhdr % FixHd(FH_RealCSize)) =  &
                    Dump_UMhdr % RealC (1:Dump_UMhdr % FixHd(FH_RealCSize))

!-------------------------------------------------------------------------------
!  Section 5  Amend Cx_UMhdr % LevDepC
!-------------------------------------------------------------------------------

Cx_UMhdr % LevDepC(:) = RMDI
Len1 = Dump_UMhdr % FixHd(FH_LevDepCSize1) ! Number of levels
Cx_UMhdr % LevDepC(1:Len1 + 2) = 0.0       ! Obs levels not set

IF (Dump_UMhdr % FixHd(FH_SubModel) == FH_SubModel_Atmos .AND. & ! Atmos dump
    Dump_UMhdr % FixHd(FH_Dataset) /= FH_Dataset_Ancil) THEN     ! but not SST

  Len1 = Len1 - 1  ! Reduced by one as for ND the levDepConstants are
                   ! dimensioned 0:NumLevels

  ! Eta_Theta in dump is dimensioned 0:NumLevels, i.e. there are
  ! NumLevels+1 values. 1st value is for orography, i.e. 0, so miss
  ! this out.
  Position = Len1 + 2
  Cx_UMhdr % LevDepC(Position + 1) = 15         ! indicator for Eta_Theta
  Cx_UMhdr % LevDepC(Position + 2) = Len1
  DO i = Position + 3, Position + 2 + Len1
    Cx_UMhdr % LevDepC(i) = Dump_UMhdr % LevDepC(i - (Position + 1))
  END DO

    ! Eta_Rho transferred as normal
    Position = 2 * (Len1 + 2)
    Cx_UMhdr % LevDepC(Position + 1) = 16         ! indicator for Eta_Rho
    Cx_UMhdr % LevDepC(Position + 2) = Len1
    Cx_UMhdr % LevDepC(Position + 3:Position + 2 + Len1) = &
      Dump_UMhdr % LevDepC(Len1 + 2:(2 * Len1) + 1)

ELSE IF (Dump_UMhdr % FixHd(2) == 2) THEN         ! Ocean dump

  Position = Len1 + 2
  Cx_UMhdr % LevDepC(Position + 1) = 14           ! indicator for ocean
                                                  ! depth
  Cx_UMhdr % LevDepC(Position + 2) = Len1
  Cx_UMhdr % LevDepC(Position + 3:Position + 2 + Len1) = &
    TRANSFER (Dump_UMhdr % Lookup(52,1:Len1), 0.0, Len1)

END IF

!-------------------------------------------------------------------------------
!  Section 6  Set up Cx_UMhdr % ColDepC (with pointers to Cx data), NumFlds and
!             MaxFldSize
!-------------------------------------------------------------------------------

Cx_UMhdr % ColDepC(:) = IMDI   ! Initialise to missing data
Cx_UMhdr % NumFlds = 0
Cx_UMhdr % MaxFldSize = 0

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_OpenModelColumn
