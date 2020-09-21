!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Buddy check for scalar quantities for surface data
!
! Method:  see OSDP 2, sections 3.6 and 3.7
!
! Inputs:  see argument list
!
! Outputs: PGE/flags - see argument list
!-------------------------------------------------------------------------------

SUBROUTINE Ops_QcBdChkSs (BdListA,    & ! list buddy checks for these obs?
                          BdHCscaleA, & ! background error corrn scales
                          CallSignA,  & ! Observation callsigns (for listing)
                          TimeA,      & ! Seconds after ValidityTime
                          ValueA,     & ! Observed value (including any corrections)
                          ObErrA,     & ! Estimated error of observed value
                          BackgrA,    & ! Background value
                          BackgrErrA, & ! Estimated error of background value
                          PGEA,       & ! Probability of Gross Error before/after check
                          NumPairs,   & ! Number of pairs
                          PairObA,    & ! Ob from set A in pair
                          PairObB,    & ! Ob from set B in pair
                          PairDist)     ! in      horizontal distance (km) between obs

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
REAL, INTENT(IN)             :: ValueA(:)       ! Observed value (inc. any correction)
REAL, INTENT(IN)             :: ObErrA(:)       ! Estimated error of observed value
REAL, INTENT(IN)             :: BackgrA(:)      ! Background value
REAL, INTENT(IN)             :: BackgrErrA(:)   ! Estimated error of background value
REAL, INTENT(INOUT)          :: PGEA(:)         ! PGE before/after background check
INTEGER, INTENT(IN)          :: NumPairs        ! number of Pairs found overall
INTEGER, INTENT(IN)          :: PairObA(:)      ! ob from set A in pair
INTEGER, INTENT(IN)          :: PairObB(:)      ! ob from set B in pair
REAL, INTENT(IN)             :: PairDist(:)     ! horizontal Distance (km) between obs

! Local declarations:
INTEGER                      :: JPair   ! loops over pairs
INTEGER                      :: JA      ! current observation in set A
INTEGER                      :: JB      ! current observation in set B
REAL                         :: HCscale  ! horizontal error scale for the pair of obs
REAL                         :: SDist    ! scaled Distance between observations
REAL                         :: Corr     ! background error correlation between ob positions
REAL                         :: Covar    ! background error covariance between ob positions
REAL                         :: Rho2     ! (total error correlation between ob positions)**2
REAL                         :: ExpArg   ! argument for exponents
REAL                         :: PGEMax = 1.0 ! maximum allowed value for PGE
REAL                         :: DiffA    ! difference from background
REAL                         :: DiffB    ! difference from background
REAL                         :: ErrVarA  ! estimated error variance (ob+bk) for ob A
REAL                         :: ErrVarB  ! estimated error variance (ob+bk) for ob B
REAL                         :: Z        ! P(OA)*P(OB)/P(OA and OB)
CHARACTER(len=*), PARAMETER  :: RoutineName = "Ops_QcBdChkSs"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (QcMode >= DiagnosticMode .AND. mype == 0) THEN
  WRITE (MessageOut,'(/A,A,F7.2,A,F7.2)') RoutineName, &
    '   BdDamp1 = ', BdDamp1, '  BdDamp2 = ', BdDamp2
  WRITE (MessageOut,'(/A)') '  ob nos    callsign  callsign DiffA DiffB '// &
    ' dist  Corr  agree  pge_a  pge_b   mult '
END IF

! 2.  Loop through observation pairs
DO JPair = 1, NumPairs
  JA = PairObA(JPair)
  JB = PairObB(JPair)

  HCscale = 0.5 * (BdHCscaleA(JA) + BdHCscaleA(JB))      ! eqn 3.9
  SDist = PairDist(JPair) / HCscale
  ! Surface data, treat vertical correlation as 1.0
  Corr = (1.0 + SDist) * EXP (-SDist - ((TimeA(JA) - TimeA(JB)) / CorScaleT) ** 2)  ! eqns 3.10, 3.11

  ! 2.1  Scalar buddy check
  IF (Corr < 0.1) CYCLE  ! skip to next pair
  !  Check that values are present and buddy check is required
  IF (PGEA(JA) < PGEMax .AND. PGEA(JB) < PGEMax) THEN
    DiffA = ValueA(JA) - BackgrA(JA)
    DiffB = ValueA(JB) - BackgrA(JB)
    !    Calculate error variance
    ErrVarA = ObErrA(JA) ** 2 + BackgrErrA(JA) ** 2             ! eqn 2.5
    ErrVarB = ObErrA(JB) ** 2 + BackgrErrA(JB) ** 2             ! eqn 2.5
    Covar = Corr * BackgrErrA(JA) * BackgrErrA(JB)        ! eqn 3.13
    Rho2 = Covar ** 2 / (ErrVarA * ErrVarB)                    ! eqn 3.14
    ExpArg = -(0.5 * Rho2 / (1.0 - Rho2)) * &
        (DiffA ** 2 / ErrVarA + DiffB ** 2 / ErrVarB - 2.0 * DiffA * DiffB / Covar)
    ExpArg = BdDamp1 * (-0.5 * LOG (1.0 - Rho2) + ExpArg)         ! exponent of
    ExpArg = MIN (ExpArgMax, MAX (-ExpArgMax, ExpArg))       ! eqn 3.18
    Z = 1.0 / (1.0 - (1.0 - PGEA(JA)) * (1.0 - PGEA(JB)) * (1.0 - EXP (ExpArg)))
    IF (Z <= 0.0) Z = 1.0  ! rounding error control
    Z = Z ** BdDamp2                                          ! eqn 3.16
    PGEA(JA) = PGEA(JA) * Z                                 ! eqn 3.17
    PGEA(JB) = PGEA(JB) * Z                                 ! eqn 3.17
    IF ((BdListA(JA) .OR. BdListA(JB)) .AND. mype == 0) THEN
      WRITE (MessageOut, '(2I5,2(2X,A8),3F6.1,F6.3,4F7.3)') &
          JA, JB, CallSignA(JA), CallSignA(JB), &
          DiffA, DiffB, PairDist(JPair), Corr, &
          EXP (ExpArg), PGEA(JA), PGEA(JB), Z
     END IF
  END IF

END DO  ! JPair loop

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_QcBdChkSs
