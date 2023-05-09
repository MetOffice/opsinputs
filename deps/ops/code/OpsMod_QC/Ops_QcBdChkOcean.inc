!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Buddy check of scalar quantities for ocean profile data, allows for separate
! mesoscale and synoptic scales
!
! Method:  see OSDP 2, sections 3.6 and 3.7
!
! Inputs:  see argument list
!
! Outputs: PGE/flags - see argument list
!-------------------------------------------------------------------------------

SUBROUTINE Ops_QcBdChkOcean (BdListA,    & ! list buddy checks for these obs?
                             CallSignA,  & ! Observation callsigns (for listing)
                             TimeA,      & ! Seconds after ValidityTime
                             NumLevA,    & ! Number of levels per observation
                             ValueA,     & ! Observed value (including any corrections)
                             ObErrA,     & ! Estimated error of observed value
                             BackgrA,    & ! Background value
                             Syn_VarA,   & ! Estimated variance of synoptic bk error
                             Syn_HcorA,  & ! Estimated scale of synoptic bk error (km)
                             Syn_AnisoA, & ! Estimated anisotropy of synoptic bk error (EW/NS
                             Mes_VarA,   & ! Estimated variance of mesoscale bk error
                             Mes_HcorA,  & ! Estimated scale of mesoscale bk error (km)
                             Mes_AnisoA, & ! Estimated anisotropy of mesoscale bk error (EW/NS
                             PGEA,       & ! Probability of Gross Error before/after check
                             NumPairs,   & ! Number of pairs
                             PairObA,    & ! Ob from set A in pair
                             PairObB,    & ! Ob from set B in pair
                             PairDist,   & ! horizontal distance (km) between obs
                             PairRotA,   & ! direction of ob B from ob A
                             PairRotB)     ! reciprocal direction of ob A from B

USE GenMod_Control, ONLY: &
    DiagnosticMode,       & ! (PARAMETER) normal program development
    mype

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    UseTrace

USE OpsMod_QC, ONLY: &
    ExpArgMax,       &
    QcMode

IMPLICIT NONE

! Subroutine arguments:
LOGICAL, INTENT(IN)          :: BdListA(:)      ! list buddy checks for these obs?
INTEGER, INTENT(IN)          :: NumLevA         ! Number of levels per observation
CHARACTER(len=*), INTENT(IN) :: CallSignA(:)    ! Observation callsigns (for listing)
REAL, INTENT(IN)             :: TimeA(:)        ! Seconds after ValidityTime
REAL, INTENT(IN)             :: ValueA(:,:)     ! Observed value (inc. any correction)
REAL, INTENT(IN)             :: ObErrA(:,:)     ! Estimated error of observed value
REAL, INTENT(IN)             :: BackgrA(:,:)    ! Background value
REAL, INTENT(IN)             :: Syn_VarA(:,:)   ! Variance   ) Estimated
REAL, INTENT(IN)             :: Syn_HcorA(:,:)  ! Scale (km) ) synoptic
REAL, INTENT(IN)             :: Syn_AnisoA(:)   ! Anisotropy ) bk error
REAL, INTENT(IN)             :: Mes_VarA(:,:)   ! Variance   ) Estimated
REAL, INTENT(IN)             :: Mes_HcorA(:,:)  ! Scale (km) ) mesoscale
REAL, INTENT(IN)             :: Mes_AnisoA(:)   ! Anisotropy ) bk error
REAL, INTENT(INOUT)          :: PGEA(:,:)       ! PGE before/after background check
INTEGER, INTENT(IN)          :: NumPairs        ! number of Pairs found overall
INTEGER, INTENT(IN)          :: PairObA(:)      ! ob from set A in pair
INTEGER, INTENT(IN)          :: PairObB(:)      ! ob from set B in pair
REAL, INTENT(IN)             :: PairDist(:)     ! horizontal Distance (km) between obs
REAL, INTENT(IN)             :: PairRotA(:)     ! direction of ob B from ob A
REAL, INTENT(IN)             :: PairRotB(:)     ! reciprocal direction of ob A from B

! Local declarations:
INTEGER                      :: JPair   ! loops over pairs
INTEGER                      :: JLev    ! loops over levels
INTEGER                      :: JLevA   ! level in set A
INTEGER                      :: JLevB   ! level in set B
INTEGER                      :: JA      ! current observation in set A
INTEGER                      :: JB      ! current observation in set B
INTEGER                      :: Lev1    ! start of loop over levels
INTEGER                      :: Lev2    ! end of loop over levels
REAL                         :: MesScale   ! Mesoscale error scale for the pair of obs
REAL                         :: MesSDist   ! Distance between observations / MesScale
REAL                         :: SynScale   ! Synoptic error scale for the pair of obs
REAL                         :: SynSDist   ! Distance between observations / SynScale
REAL                         :: TimeDiff2  ! (Time difference / CorScaleT) **2
REAL                         :: Sin2       ! sin**2 of angle between AB and an EW parallel
REAL                         :: Aniso      ! Anisotropy = EW/NS scale
REAL                         :: SynFact    ! Anisotropic distance factor for synoptic errors
REAL                         :: MesFact    ! Anisotropic distance factor for mesoscale errors
REAL                         :: Corr     ! background error correlation between ob positions
REAL                         :: Covar    ! background error covariance between ob positions
REAL                         :: Rho2     ! (total error correlation between ob positions)**2
REAL                         :: ExpArg   ! argument for exponents
REAL                         :: PGEMax  = 1.0 ! maximum allowed value for PGE
REAL                         :: DiffA    ! difference from background
REAL                         :: DiffB    ! difference from background
REAL                         :: ErrVarA  ! estimated error variance (ob+bk) for ob A
REAL                         :: ErrVarB  ! estimated error variance (ob+bk) for ob B
REAL                         :: Z        ! P(OA)*P(OB)/P(OA and OB)
CHARACTER(len=*), PARAMETER  :: RoutineName = "Ops_QcBdChkOcean"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (QcMode >= DiagnosticMode .AND. mype == 0) THEN
  WRITE (MessageOut,'(/A,A,F7.2,A,F7.2)') RoutineName, &
    '   BdDamp1 = ',BdDamp1,'  BdDamp2 = ',BdDamp2
  WRITE (MessageOut,'(/A)') '  ob nos    callsign  callsign lev DiffA DiffB'// &
    ' dist  Corr  agree  pge_a  pge_b   mult '
END IF

! 2.  Loop through observation pairs
DO JPair = 1,NumPairs

  JA = PairObA(JPair)
  JB = PairObB(JPair)
  Lev1 = 1
  Lev2 = MIN (NumLevA,NumLevA)

  Sin2 = SIN (0.5 * (PairRotA(JPair) + PairRotB(JPair))) ** 2
  ! = sin**2 of angle of line joining A and B with EW
  Aniso = 0.5 * (Syn_AnisoA(JA) + Syn_AnisoA(JB)) ! EW/NS scale
  SynFact = SQRT (1.0 + (Aniso ** 2 - 1.0) * Sin2)
  Aniso = 0.5 * (Mes_AnisoA(JA) + Mes_AnisoA(JB)) ! EW/NS scale
  MesFact = SQRT (1.0 + (Aniso ** 2 - 1.0) * Sin2)

  ! 2.1  Scalar buddy check
  DO JLev = Lev1,Lev2
    !  Check that values are present and buddy check is required
    JLevA = MIN (JLev,NumLevA)
    JLevB = MIN (JLev,NumLevA)
    IF (PGEA(JA,JLevA) < PGEMax .AND. PGEA(JB,JLevB) < PGEMax) THEN
      MesScale = 0.5 * (Mes_HcorA(JA,JLevA) + Mes_HcorA(JB,JLevB)) ! eqn 3.9
      SynScale = 0.5 * (Syn_HcorA(JA,JLevA) + Syn_HcorA(JB,JLevB)) ! eqn 3.9
      MesSDist = MesFact * PairDist(JPair) / MesScale
      SynSDist = SynFact * PairDist(JPair) / SynScale
      TimeDiff2 = ((TimeA(JA) - TimeA(JB)) / CorScaleT) ** 2
      ! Multi-level data, treat vertical correlation as 1.0
       ! eqns 3.10, 3.11
      Covar   = SQRT (Mes_VarA(JA,JLevA) * Mes_VarA(JB,JLevB)) &
                * (1.0 + MesSDist) * EXP (-MesSDist - TimeDiff2) &
              + SQRT (Syn_VarA(JA,JLevA) * Syn_VarA(JB,JLevB)) &
                * (1.0 + SynSDist) * EXP (-SynSDist - TimeDiff2)

      DiffA  = ValueA(JA,JLevA) - BackgrA(JA,JLevA)
      DiffB  = ValueA(JB,JLevB) - BackgrA(JB,JLevB)
      !  Calculate error variance - eqn 2.5 ?
      ErrVarA = ObErrA(JA,JLevA) ** 2 + Mes_VarA(JA,JLevA) + Syn_VarA(JA,JLevA)
      ErrVarB = ObErrA(JB,JLevB) ** 2 + Mes_VarA(JB,JLevB) + Syn_VarA(JB,JLevB)
      Rho2    = Covar ** 2 / (ErrVarA * ErrVarB)                       ! eqn 3.14
      ExpArg  = -(0.5 * Rho2 / (1.0 - Rho2)) * &
        (DiffA ** 2 / ErrVarA + DiffB ** 2 / ErrVarB - 2.0 * DiffA * DiffB / Covar)
      ExpArg  = BdDamp1 * (-0.5 * LOG (1.0 - Rho2) + ExpArg)         ! exponent of
      ExpArg  = MIN (ExpArgMax, MAX (-ExpArgMax, ExpArg))       ! eqn 3.18
      Z = 1.0 / (1.0 - (1.0 - PGEA(JA,JLevA)) * (1.0 - PGEA(JB,JLevB)) * (1.0 - EXP (ExpArg)))
      IF (Z <= 0.0) Z = 1.0  ! rounding error control
      Z = Z ** BdDamp2                                          ! eqn 3.16
      PGEA(JA,JLevA) = PGEA(JA,JLevA) * Z                     ! eqn 3.17
      PGEA(JB,JLevB) = PGEA(JB,JLevB) * Z                     ! eqn 3.17
      IF ((BdListA(JA) .OR. BdListA(JB)) .AND. mype == 0) THEN
        Corr = Covar / SQRT ((Mes_VarA(JA,JLevA) + Syn_VarA(JA,JLevA)) &
                            * (Mes_VarA(JB,JLevB) + Syn_VarA(JB,JLevB)))
        WRITE (MessageOut,'(2I5,2(2X,A8),I3,3F6.1,F6.3,4F7.3)') &
          JA, JB, CallSignA(JA), CallsignA(JB), JLev, &
          DiffA, DiffB, PairDist(JPair), Corr, &
          EXP (ExpArg), PGEA(JA,JLevA), PGEA(JB,JLevB), Z
      END IF
    END IF

  END DO  ! JLev loop
END DO  ! JPair loop

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_QcBdChkOcean
