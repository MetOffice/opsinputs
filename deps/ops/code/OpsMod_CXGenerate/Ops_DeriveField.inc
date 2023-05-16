!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Calculates derived fields from other UM dump fields.
!
! Method:
!     The fields required to derive the required field are located and read in.
!     An error is raised if they cannot be found. Space is allocated if required,
!     and the derived field is calculated. Fields read in are deallocated.
!
! Inputs:
!     UMHdr: The UM dump header.
!     Time:  The forecast time at which the derived field is required
!     ModelTimeUTC: Validity time UTC in the UM dump.
!
! Outputs:
!     ParamA:     ParameterA for Pstn to Pstar calculation
!     ParamB:     ParameterB for Pstn to Pstar calculation
!     VLapseRate: Near surface lapse rate
!-------------------------------------------------------------------------------

SUBROUTINE Ops_DeriveField (UMHdr,        &
                            Time,         &
                            ModelTimeUTC, &
                            ParamA,       &
                            ParamB,       &
                            VLapseRate,   &
                            ReadOnAllPEs)

USE GenMod_Control, ONLY: &
  GeneralMode,            &
  DebugMode,              &
  MyPE

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  MessageOut,          &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  RMDI,                                  &
  g,                                     &
  R,                                     &
  Pref,                                  &
  Kappa,                                 &
  C_Virtual

USE GenMod_ModelIO, ONLY: &
  UM_header_type

USE GenMod_UMHeaderConstants, ONLY: &
  FH_LevDepCSize1,                  &
  IC_Plevels,                       &
  RC_z_ModelTop,                    &
  IC_FirstConstantRhoLevel

USE OpsMod_Constants, ONLY: &
  Lapse_RateL

USE OpsMod_FieldRead, ONLY: &
  Ops_GetUMFieldDims,       &
  Ops_ReadUMField

USE OpsMod_Stash, ONLY:  &
  StashItem_p,           &
  StashItem_orog,        &
  StashItem_theta,       &
  StashItem_q

USE OpsMod_Kinds, ONLY : &
  real32

IMPLICIT NONE

! Subroutine arguments:
TYPE (UM_header_type),INTENT(IN) :: UMHdr ! Local UM header
INTEGER, INTENT(IN)              :: Time
INTEGER, INTENT(IN)              :: ModelTimeUTC(:)
REAL(real32), ALLOCATABLE        :: ParamA(:,:)
REAL(real32), ALLOCATABLE        :: ParamB(:,:)
REAL(real32), ALLOCATABLE        :: VLapseRate(:,:)
LOGICAL, OPTIONAL, INTENT(IN)    :: ReadOnAllPEs

! Local declarations:
REAL(real32), ALLOCATABLE        :: Press1(:,:)
REAL(real32), ALLOCATABLE        :: PressBL1(:,:)
REAL(real32), ALLOCATABLE        :: PressBL2(:,:)
REAL(real32), ALLOCATABLE        :: Orog(:,:)
REAL(real32), ALLOCATABLE        :: QBL(:,:)
REAL(real32), ALLOCATABLE        :: TempVLapseRate(:,:)
REAL, POINTER                    :: EtaTheta(:)
REAL, POINTER                    :: EtaRho(:)
REAL                             :: Zexp1
REAL                             :: tbl
REAL                             :: Z1
REAL                             :: ZA1
REAL                             :: ZA2
REAL                             :: ZB
REAL                             :: T1
REAL                             :: dZ
REAL                             :: ZModelTop
INTEGER                          :: i
INTEGER                          :: j
INTEGER                          :: NumCxLev   ! Number of Cx levels
INTEGER                          :: Pnt        ! Pointer to Level Dependent Constants
INTEGER                          :: Level      ! Loop variable over levels
INTEGER                          :: LevelAboveBL  ! First level above boundary layer
INTEGER                          :: FirstConstantRhoLevel
REAL, PARAMETER                  :: UpperHeight = 2000.0 ! Height of upper level above ground
CHARACTER(len=*), PARAMETER      :: RoutineName = "Ops_DeriveField"
INTEGER                          :: FieldDims(2)
LOGICAL                          :: ReadOnAllPEs_local

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (PRESENT (ReadOnAllPEs)) THEN
  ReadOnAllPEs_local = ReadOnAllPEs
ELSE
  ReadOnAllPEs_local = .FALSE.
END IF

NumCxLev = UMHdr % IntC(IC_PLevels)
Zexp1 = Lapse_RateL * R / g

ZModelTop = UMHdr % RealC(RC_z_ModelTop)

Pnt = UMHdr % FixHd(FH_LevDepCSize1)   !Number of levels
EtaTheta => UMHdr % LevDepC(2:Pnt)
EtaRho => UMHdr % LevDepC(Pnt + 1: 2 * Pnt - 1)

DO Level = 1, NumCxLev
  LevelAboveBL = Level
  IF (EtaTheta(LevelAboveBL) * ZModelTop > UpperHeight) EXIT
END DO

! Required so can pass in memory that is allready allocated
CALL Ops_GetUMFieldDims (FieldDims,                   &
                         UMHdr,                       &
                         StashItem_Orog,              &
                         level = 1,                   &
                         Time = Time,                 &
                         ModelTimeUTC = ModelTimeUTC)
ALLOCATE (Orog(FieldDims(1), FieldDims(2)))

! Read the required fields
CALL Ops_ReadUMField (UMHdr,                             &
                      StashItem_Orog,                    &
                      Orog,                              &
                      level = 1,                         &
                      Time = Time,                       &
                      ModelTimeUTC = ModelTimeUTC,       &
                      ReadOnAllPEs = ReadOnAllPEs_local)

IF (.NOT. ALLOCATED (ParamA)) THEN  ! All three arrays (de)allocated together
  ALLOCATE (VLapseRate(FieldDims(1),FieldDims(2)))
  ALLOCATE (ParamA(FieldDims(1),FieldDims(2)))
  ALLOCATE (ParamB(FieldDims(1),FieldDims(2)))
END IF

IF (LevelLapse > 0) THEN

  ! Required so can pass in memory that is allready allocated
  CALL Ops_GetUMFieldDims (FieldDims,                   &
                           UMHdr,                       &
                           StashItem_P,                 &
                           level = LevelLapse,          &
                           Time = Time,                 &
                           ModelTimeUTC = ModelTimeUTC)
  ALLOCATE(Press1(FieldDims(1), FieldDims(2)))
  CALL Ops_ReadUMField (UMHdr,                             &
                        StashItem_P,                       &
                        Press1,                            &
                        level = LevelLapse,                &
                        Time = Time,                       &
                        ModelTimeUTC = ModelTimeUTC,       &
                        ReadOnAllPEs = ReadOnAllPEs_local)

  CALL Ops_GetUMFieldDims (FieldDims,                   &
                           UMHdr,                       &
                           StashItem_Theta,             &
                           level = LevelLapse,          &
                           Time = Time,                 &
                           ModelTimeUTC = ModelTimeUTC)
  ALLOCATE (TempVLapseRate(FieldDims(1), FieldDims(2)))
  CALL Ops_ReadUMField (UMHdr,                             &
                        StashItem_Theta,                   &
                        TempVLapseRate,                    &
                        level = LevelLapse,                &
                        Time = Time,                       &
                        ModelTimeUTC = ModelTimeUTC,       &
                        ReadOnAllPEs = ReadOnAllPEs_local)

  ! Convert potential temperature to temperature (reuse array)
  TempVLapseRate(:,:) = TempVLapseRate(:,:) * (Press1(:,:) / Pref) ** Kappa

  CALL Ops_CalcTLapse (Orog,           &
                       TempVLapseRate, &
                       Orog /= 0.0,    &  ! in
                       VLapseRate)        ! out
  DEALLOCATE (TempVLapseRate)
ELSE
  VLapseRate(:,:) = RMDI
END IF

IF (LevelLapse /= 1) THEN

  IF (ALLOCATED (Press1)) DEALLOCATE (Press1)
  ! I dont think its necassry to deallocate
  CALL Ops_GetUMFieldDims (FieldDims,                   &
                           UMHdr,                       &
                           StashItem_P,                 &
                           level = 1,                   &
                           Time = Time,                 &
                           ModelTimeUTC = ModelTimeUTC)
  ALLOCATE(Press1(FieldDims(1), FieldDims(2)))
  CALL Ops_ReadUMField (UMHdr,                             &
                        StashItem_P,                       &
                        Press1,                            &
                        level = 1,                         &
                        Time = Time,                       &
                        ModelTimeUTC = ModelTimeUTC,       &
                        ReadOnAllPEs = ReadOnAllPEs_local)
END IF

CALL Ops_GetUMFieldDims (FieldDims,                   &
                         UMHdr,                       &
                         StashItem_Q,                 &
                         level = LevelAboveBL,        &
                         Time = Time,                 &
                         ModelTimeUTC = ModelTimeUTC)
ALLOCATE(QBL(FieldDims(1), FieldDims(2)))
CALL Ops_ReadUMField (UMHdr,                             &
                      StashItem_Q,                       &
                      QBL,                               &
                      level = LevelAboveBL,              &
                      Time = Time,                       &
                      ModelTimeUTC = ModelTimeUTC,       &
                      ReadOnAllPEs = ReadOnAllPEs_local)

CALL Ops_GetUMFieldDims (FieldDims,                   &
                         UMHdr,                       &
                         StashItem_P,                 &
                         level = LevelAboveBL,        &
                         Time = Time,                 &
                         ModelTimeUTC = ModelTimeUTC)
ALLOCATE(PressBL1(FieldDims(1), FieldDims(2)))
CALL Ops_ReadUMField (UMHdr,                             &
                      StashItem_P,                       &
                      PressBL1,                          &
                      level = LevelAboveBL,              &
                      Time = Time,                       &
                      ModelTimeUTC = ModelTimeUTC,       &
                      ReadOnAllPEs = ReadOnAllPEs_local)

CALL Ops_GetUMFieldDims (FieldDims,                   &
                         UMHdr,                       &
                         StashItem_P,                 &
                         level = LevelAboveBL,        &
                         Time = Time,                 &
                         ModelTimeUTC = ModelTimeUTC, &
                         AddBLField = 1)
ALLOCATE(PressBL2(FieldDims(1), FieldDims(2)))
CALL Ops_ReadUMField (UMHdr,                             &
                      StashItem_P,                       &
                      PressBL2,                          &
                      level = LevelAboveBL,              &
                      Time = Time,                       &
                      ModelTimeUTC = ModelTimeUTC,       &
                      AddBLField = 1,                    &
                      ReadOnAllPEs = ReadOnAllPEs_local)

DZ = (EtaTheta(LevelAboveBL) - EtaRho(1)) * ZModelTop
FirstConstantRhoLevel = UMHdr % IntC(IC_FirstConstantRhoLevel)
DO i = 1, SIZE (ParamA, DIM = 1)
  DO j = 1, SIZE (ParamA, DIM = 2)
    ZA1 = EtaRho(LevelAboveBL) * ZModelTop + Orog(i,j) * &
        (1.0 - EtaRho(LevelAboveBL) / EtaRho(FirstConstantRhoLevel)) ** 2
    ZA2 = EtaRho(LevelAboveBL + 1) * ZModelTop + Orog(i,j) * &
        (1.0 - EtaRho(LevelAboveBL + 1) / EtaRho(FirstConstantRhoLevel)) ** 2

    Tbl = -G / R * (ZA2 - ZA1) / LOG (PressBL2(i,j) / PressBL1(i,j))
    ! Convert from virtual temperature to temperature using qbl
    Tbl = Tbl / (1.0 + C_Virtual * qbl(i,j))

    Z1 = EtaRho(1) * ZModelTop + Orog(i,j) * (1.0 - EtaRho(1) / &
           EtaRho(FirstConstantRhoLevel)) ** 2
    ZB = EtaTheta(LevelAboveBL) * ZModelTop + Orog(i,j) * &
        (1.0 - EtaTheta(LevelAboveBL) / EtaRho(FirstConstantRhoLevel)) ** 2
    dZ = ZB - Z1
    T1 = Tbl + Lapse_RateL * dZ

    ParamA(i,j) = Z1 + T1 / Lapse_RateL
    ParamB(i,j) = T1 / ((Press1(i,j) ** ZExp1) * Lapse_RateL)
  END DO
END DO

IF (GeneralMode >= DebugMode .AND. MyPE == 0) THEN
  WRITE (MessageOut, '(A)') 'Derived fields calculated'
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_DeriveField
