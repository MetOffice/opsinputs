!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Background check for vector quantities
!     - can process single level observations or levels of a profile
!
! Method:  see OSDP 2, section 2
!
! Inputs:  see argument list
!
! Outputs: PGE/flags - see argument list
!
! Note. Wind variance is assumed isotropic, ObErr and BackgrErr give rms error
!       in a single component, units - m/s PdBad and PdTotal are 2D (u,v)
!       densities, units (m/s)**-2
!-------------------------------------------------------------------------------

SUBROUTINE Ops_QcBackgr1v (uElement,  & ! Element type contains u obs info
                           vElement,  & ! Element type contains v obs info
                           uBackgr,   & ! Background u
                           vBackgr,   & ! Background v
                           BackgrErr, & ! Estimated error of background value
                           PdBad,     & ! Probability density of obs with gross errors
                           PdTotal,   & ! Combined probability density (for whole ob check)
                           PGEBd,     & ! PGE for input to buddy check
                           Prelim)      ! Prelim run of background check?

USE GenMod_Constants, ONLY: &
    RMDItol                   ! Tolerance for RMDI

USE GenMod_Control, ONLY: &
    VerboseMode,          & ! (PARAMETER) for verbose output
    mype

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    Pi,                                  & ! Pi
    RMDI                                   ! Missing data indicator value

USE OpsMod_Constants, ONLY: &
    PPF,                    & ! PGE packing factor
    PGEMDI                    ! Missing data value for PGEs

USE OpsMod_MiscTypes, ONLY: &
    Element_type

USE OpsMod_ObsInfo, ONLY: &
    FinalRejectFlag,      & ! => do not use in analysis
    BackRejectFlag,       & ! => PGE>PGEcrit after backgr check
    PermRejectFlag,       & ! => blacklisted data
    BackPerfFlag            ! => Background check performed

USE OpsMod_QC, ONLY: &  ! Parent module
    ExpArgMax,       &  ! Max argument for EXP function
    PGECrit,         &  ! Rejection limit
    QcMode

IMPLICIT NONE

! Subroutine arguments:
TYPE (Element_type), INTENT(INOUT) :: uElement(:)    ! Element type containing obs info
TYPE (Element_type), INTENT(INOUT) :: vElement(:)    ! Element type containing obs info
REAL, INTENT(IN)                   :: uBackgr(:)     ! Background u
REAL, INTENT(IN)                   :: vBackgr(:)     ! Background v
REAL, INTENT(IN)                   :: BackgrErr(:)   ! Estimated error of background value
REAL, INTENT(IN)                   :: PdBad(:)       ! Probability density of 'bad' obs
REAL, INTENT(OUT)                  :: PdTotal(:)     ! Combined probability density
REAL, OPTIONAL, INTENT(OUT)        :: PGEBd(:)       ! PGE for input to buddy check
LOGICAL, OPTIONAL, INTENT(IN)      :: Prelim         ! Prelim run of background check?

! Local declarations:
INTEGER                            :: J              ! Loops over observations
REAL                               :: SDiff          ! Squared difference from background / ErrVar
REAL                               :: ErrVar         ! Combined (ob and background) Error Variance
REAL                               :: PdGood         ! Probability density of good ob - normal distribution
REAL                               :: PGEBk          ! PGE after background check
LOGICAL                            :: PrelimRun      ! Prelim run of background check?
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_QcBackgr1v"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

PrelimRun = .FALSE.
IF (PRESENT (Prelim)) PrelimRun = Prelim
IF (PRESENT (PGEBd)) PGEBd = PGEMDI

DO J = 1,SIZE (uElement)
  IF (uElement(J) % oberr >= 0 .AND. BackgrErr(J) >= 0) THEN
    ErrVar = (ObErrMult * uElement(J) % oberr) ** 2 + &       ! modified version of eqn 2.5
               (BackgrErrMult * BackgrErr(J)) ** 2   ! for checking sensitivity
  ELSE
    ErrVar = RMDI
  END IF

  IF (ABS (uElement(J) % value - RMDI) > RMDItol .AND. &
      ABS (uBackgr(J) - RMDI) > RMDItol .AND. &
      ABS (vElement(J) % value - RMDI) > RMDItol .AND. &
      ABS (vBackgr(J) - RMDI) > RMDItol .AND. &
      ABS (ErrVar - RMDI) > RMDITol) THEN

    SDiff = ((uElement(J) % value - uBackgr(J)) ** 2 + (vElement(J) % value - vBackgr(J)) ** 2) / ErrVar
    PdGood = EXP (-0.5 * MIN (SDiff, 2.0 * ExpArgMax)) / (2.0 * Pi * ErrVar)
                                                                 ! eqn 2.7
    PdTotal(J) = PdBad(J) * uElement(J) % PGEFinal + PdGood * (1.0 - uElement(J) % PGEFinal)           ! eqn 2.8
    PGEBk = PdBad(J) * uElement(J) % PGEFinal / PdTotal(J)                     ! eqn 2.9

    uElement(J) % flags = IBSET (uElement(J) % flags,BackPerfFlag)
    IF (PGEBk >= PGECrit) uElement(J) % flags = IBSET (uElement(J) % flags, BackRejectFlag)

    IF (QcMode >= VerboseMode .AND. J < 21 .AND. mype == 0) THEN
      WRITE (MessageOut, '(I3,7F13.8)') J, ErrVar, SDiff, PdGood, PdBad(J), uElement(J) % PGEFinal, &
                                                                PdTotal(J), PGEBk
    END IF

  ELSE                           ! Missing data values
    PdTotal(J) = PdBad(J)        ! Gives correct ReportPGE
    SDiff = 2.0 * SDiffCrit   ! Set flags below
    PGEBk = PGEMDI          ! Missing data indicator for packed PGEs
  END IF
  uElement(J) % PGEFinal = AINT (PGEBk * PPF) + uElement(J) % PGEFinal   ! Packing of PGEs

  IF (.NOT. PrelimRun .AND. &   ! Not applied to reported level sonde data
      (SDiff >= 2.0 * SDiffCrit .OR. &
       BTEST (uElement(J) % flags, PermRejectFlag) .OR. &
       BTEST (uElement(J) % flags, FinalRejectFlag))) THEN
    PGEBk = PGEMDI                             ! => do not buddy check
    uElement(J) % flags = IBSET (uElement(J) % flags, FinalRejectFlag)
  END IF
  IF (PRESENT (PGEBd)) PGEBd(J) = PGEBk

END DO  ! J: loop over observations

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_QcBackgr1v
