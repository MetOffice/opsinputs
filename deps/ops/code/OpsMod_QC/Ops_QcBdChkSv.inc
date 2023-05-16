!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Buddy check of vector quantities for surface data.
!
! Method:  see OSDP 2, sections 3.6 and 3.7
!
! Inputs:  see argument list
!
! Outputs: PGE/flags - see argument list
!-------------------------------------------------------------------------------

SUBROUTINE Ops_QcBdChkSv (BdListA,    & ! list buddy checks for these obs?
                          BdHCscaleA, & ! background error corrn scales
                          CallSignA,  & ! Observation callsigns (for listing)
                          TimeA,      & ! Seconds after ValidityTime
                          uValueA,    & ! Observed value of u (including any corrections)
                          vValueA,    & ! Observed value of v (including any corrections)
                          ObErrA,     & ! Estimated error of observed value (u or v)
                          uBackgrA,   & ! Background value of u
                          vBackgrA,   & ! Background value of v
                          BackgrErrA, & ! Estimated error of background value (u or v)
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
REAL, INTENT(IN)             :: BdHCscaleA(:)   ! background error corrn scales
CHARACTER(len=*), INTENT(IN) :: CallSignA(:)    ! Observation callsigns (for listing)
REAL, INTENT(IN)             :: TimeA(:)        ! Seconds after ValidityTime
REAL, INTENT(IN)             :: uValueA(:)      ! Observed value of u
REAL, INTENT(IN)             :: vValueA(:)      ! Observed value of v
REAL, INTENT(IN)             :: ObErrA(:)       ! Estimated error of observed value
REAL, INTENT(IN)             :: uBackgrA(:)     ! Background value of u
REAL, INTENT(IN)             :: vBackgrA(:)     ! Background value of v
REAL, INTENT(IN)             :: BackgrErrA(:)   ! Estimated error of background value
REAL, INTENT(INOUT)          :: PGEA(:)         ! PGE before/after background check
INTEGER, INTENT(IN)          :: NumPairs        ! number of Pairs found overall
INTEGER, INTENT(IN)          :: PairObA(:)      ! ob from set A in pair
INTEGER, INTENT(IN)          :: PairObB(:)      ! ob from set B in pair
REAL, INTENT(IN)             :: PairDist(:)     ! horizontal Distance (km) between obs
REAL, INTENT(IN)             :: PairRotA(:)     ! direction of ob B from ob A
REAL, INTENT(IN)             :: PairRotB(:)     ! reciprocal direction of ob A from B

! Local declarations:
INTEGER                      :: JPair   ! loops over pairs
INTEGER                      :: JA      ! current observation in set A
INTEGER                      :: JB      ! current observation in set B
REAL                         :: HCscale  ! horizontal error scale for the pair of obs
REAL                         :: SDist    ! scaled Distance between observations
REAL                         :: lCorr    ! background error correlation between ob positions
REAL                         :: lCovar   ! background error covariance between ob positions
REAL                         :: tCovar   ! background error covariance between ob positions
REAL                         :: lRho2    ! (total error correlation between ob positions)**2
REAL                         :: tRho2    ! (total error correlation between ob positions)**2
REAL                         :: ExpArg   ! argument for exponents
REAL                         :: PGEMax = 1.0 ! maximum allowed value for PGE
REAL                         :: SinRot   ! Sine of rotation angle
REAL                         :: CosRot   ! Cosine of rotation angle
REAL                         :: lDiffA   ! difference from background - longitudinal wind
REAL                         :: tDiffA   ! difference from background - transverse wind
REAL                         :: lDiffB   ! difference from background - longitudinal wind
REAL                         :: tDiffB   ! difference from background - transverse wind
REAL                         :: ErrVarA  ! estimated error variance (ob+bk) for ob A
REAL                         :: ErrVarB  ! estimated error variance (ob+bk) for ob B
REAL                         :: Z        ! P(OA)*P(OB)/P(OA and OB)
CHARACTER(len=*), PARAMETER  :: RoutineName = "Ops_QcBdChkSv"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (QcMode >= DiagnosticMode .AND. mype == 0) THEN
  WRITE (MessageOut, '(/A,A,F7.2,A,F7.2)') RoutineName, &
    '   BdDamp1 = ', BdDamp1, '  BdDamp2 = ', BdDamp2
  WRITE (MessageOut, '(/A)') '  ob nos    callsign  callsign    lA    lB    tA    tB ' // &
                                       ' dist  Corr  agree  pge_a  pge_b   mult'
END IF

! 2.  Loop through observation pairs
DO JPair = 1, NumPairs
  JA = PairObA(JPair)
  JB = PairObB(JPair)

  HCscale = 0.5 * (BdHCscaleA(JA) + BdHCscaleA(JB))          ! eqn 3.9
  SDist = PairDist(JPair) / HCscale
  ! Surface data, treat vertical correlation as 1.0
  lCorr = EXP (-SDist - ((TimeA(JA) - TimeA(JB)) / CorScaleT) ** 2)  ! eqns 3.10, 3.11

  ! 2.1  Vector buddy check
  IF ((1.0 + SDist) * lCorr < 0.1) CYCLE  ! skip to next pair
  !  Check that values are present and buddy check is required
  IF (PGEA(JA) < PGEMax .AND. PGEA(JB) < PGEMax) THEN

    ! Calculate longitudinal and transverse wind components
    SinRot = SIN (PairRotA(JPair))
    CosRot = COS (PairRotA(JPair))
    lDiffA = CosRot * (uValueA(JA) - uBackgrA(JA)) &
               + SinRot * (vValueA(JA) - vBackgrA(JA))             ! eqn 3.19
    tDiffA = - SinRot * (uValueA(JA) - uBackgrA(JA)) &
               + CosRot * (vValueA(JA) - vBackgrA(JA))             ! eqn 3.20
    SinRot = SIN (PairRotB(JPair))
    CosRot = COS (PairRotB(JPair))
    lDiffB = CosRot * (uValueA(JB) - uBackgrA(JB)) &
               + SinRot * (vValueA(JB) - vBackgrA(JB))             ! eqn 3.19
    tDiffB = - SinRot * (uValueA(JB) - uBackgrA(JB)) &
               + CosRot * (vValueA(JB) - vBackgrA(JB))             ! eqn 3.20

    !  Calculate error variance (component wind variance)
    ErrVarA = ObErrA(JA) ** 2 + BackgrErrA(JA) ** 2                  ! eqn 2.5
    ErrVarB = ObErrA(JB) ** 2 + BackgrErrA(JB) ** 2                  ! eqn 2.5

    ! Calculate covariances and probabilities
    lCovar = lCorr * BackgrErrA(JA) * BackgrErrA(JB)  ! eqn 3.13
    tCovar = (1.0 - BdGamma * SDist) * lCovar                   ! eqn 3.12,13
    lRho2 = lCovar ** 2 / (ErrVarA * ErrVarB)                        ! eqn 3.14
    tRho2 = tCovar ** 2 / (ErrVarA * ErrVarB)                        ! eqn 3.14
    IF (ABS (tRho2) <= 0.00001) THEN
      ExpArg = 0.0     ! prevent division by tCovar=0.0
    ELSE
      ExpArg = -(0.5 * tRho2 / (1.0 - tRho2)) * &
          (tDiffA ** 2 / ErrVarA + tDiffB ** 2 / ErrVarB - 2.0 * tDiffA * tDiffB / tCovar)
    END IF
    ExpArg = ExpArg - (0.5 * lRho2 / (1.0 - lRho2)) * &
          (lDiffA ** 2 / ErrVarA + lDiffB ** 2 / ErrVarB - 2.0 * lDiffA * lDiffB / lCovar)
    ExpArg = BdDamp1 * (-0.5 * LOG ((1.0 - lRho2) * (1.0 - lRho2)) + ExpArg)
    ExpArg = MIN (ExpArgMax, MAX (-ExpArgMax, ExpArg))            ! eqn 3.22
    Z = 1.0 / (1.0 - (1.0 - PGEA(JA)) * (1.0 - PGEA(JB)) * (1.0 - EXP (ExpArg)))
    IF (Z <= 0.0) Z = 1.0  ! rounding error control
    Z = Z ** BdDamp2                                               ! eqn 3.16
    PGEA(JA) = PGEA(JA) * Z                                      ! eqn 3.17
    PGEA(JB) = PGEA(JB) * Z                                      ! eqn 3.17
    IF ((BdListA(JA) .OR. BdListA(JB)) .AND. mype == 0) THEN
      WRITE (MessageOut, '(2I5,2(2X,A8),5F6.1,F6.3,4F7.3)') &
          JA, JB, CallSignA(JA), CallSignA(JB), lDiffA, lDiffB, tDiffA, tDiffB, &
          PairDist(JPair), lCorr, EXP (ExpArg), PGEA(JA), PGEA(JB), Z
    END IF
  END IF

END DO  ! JPair loop

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_QcBdChkSv
