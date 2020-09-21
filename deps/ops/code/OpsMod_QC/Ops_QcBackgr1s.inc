!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Background check for scalar quantities
!     - can process single level observations or levels of a profile
!
! Differences from Ops_QcBackgr2s :-
!     arrays are rank1, NumLev argument not needed
!     PdBad is array - can vary from ob to ob
!     PdTotal is output for use in whole ob check
!     PGEBd is (optionally) output for use in buddy check
!
! Method:  see OSDP 2, section 2
!
! Inputs:  see argument list
!-------------------------------------------------------------------------------

SUBROUTINE Ops_QcBackgr1s (element,   & ! Element type contains obs info
                           Backgr,    & ! Background value
                           BackgrErr, & ! Estimated error of background value
                           PdBad,     & ! Probability density of obs with gross errors
                           PdTotal,   & ! Combined probability density (for whole ob check)
                           PGEBd,     & ! PGE for input to buddy check
                           Prelim,    & ! Prelim run of background check?
                           errors,    &
                           ErrvarMax)   ! Limit for ErrVar

USE GenMod_Constants, ONLY: &
    RMDItol                       ! Tolerance for RMDI

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    Pi,                                  &  ! Pi
    RMDI                                    ! Missing data indicator value

USE OpsMod_Constants, ONLY: &
    PPF,                    &    ! PGE packing factor
    PGEMDI                       ! Missing data value for PGEs

USE OpsMod_MiscTypes, ONLY: &
    Element_type

USE OpsMod_ObsInfo, ONLY: &
    FinalRejectFlag,      &     ! => do not use in analysis
    BackRejectFlag,       &     ! => PGE>PGEcrit after backgr check
    PermRejectFlag,       &     ! => blacklisted data
    BackPerfFlag                ! => Background check performed

USE OpsMod_QC, ONLY: &
    ExpArgMax,       &           ! Max argument for EXP function
    PGECrit                      ! Rejection limit

IMPLICIT NONE

! Subroutine arguments:
TYPE (Element_type), INTENT(INOUT) :: element(:)   ! Element type containing obs info
REAL, INTENT(IN)                   :: Backgr(:)    ! Background value
REAL, INTENT(IN)                   :: BackgrErr(:) ! Estimated error of background value
REAL, INTENT(IN)                   :: PdBad(:)     ! Probability density of 'bad' obs
REAL, INTENT(OUT)                  :: PdTotal(:)   ! Combined probability density
REAL, OPTIONAL, INTENT(OUT)        :: PGEBd(:)     ! PGE for input to buddy check
LOGICAL, OPTIONAL, INTENT(IN)      :: Prelim       ! Prelim run of background check?
REAL, OPTIONAL, INTENT(IN)         :: errors(:)    ! Alternative obs errors to use
REAL, OPTIONAL, INTENT(IN)         :: ErrVarMax    ! Limit for ErrVar (used for sonde RH)

! Local declarations:
INTEGER                            :: J            ! Loops over observations
REAL                               :: SDiff        ! Squared difference from background / ErrVar
REAL                               :: ErrVar       ! Combined (ob and background) Error Variance
REAL                               :: PdGood       ! Probability density of good ob - normal distribution
REAL                               :: PGEBk        ! PGE after background check
LOGICAL                            :: PrelimRun    ! Prelim run of background check?
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_QcBackgr1s"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

PrelimRun = .FALSE.
IF (PRESENT (Prelim)) PrelimRun = Prelim
IF (PRESENT (PGEBd)) PGEBd = PGEMDI

DO J = 1, SIZE (element)
  IF (PRESENT (errors)) THEN
    IF (errors(J) >= 0 .AND. BackgrErr(J) >= 0) THEN
      ErrVar = (ObErrMult * errors(J)) ** 2 + &       ! modified version of eqn 2.5
               (BackgrErrMult * BackgrErr(J)) ** 2   ! for checking sensitivity
    ELSE
      ErrVar = RMDI
    END IF
  ELSE
    IF (element(J) % oberr >= 0 .AND. BackgrErr(J) >= 0) THEN
      ErrVar = (ObErrMult * element(J) % oberr) ** 2 + &   ! modified version of eqn 2.5
               (BackgrErrMult * BackgrErr(J)) ** 2         ! for checking sensitivity
    ELSE
      ErrVar = RMDI
    END IF
  END IF
  IF (PRESENT (ErrVarMax)) THEN
    ErrVar = MIN (ErrVar, ErrVarMax)
  END IF

  IF (ABS (element(J) % value - RMDI) > RMDItol .AND. &
      ABS (Backgr(J) - RMDI) > RMDItol .AND. &
      ABS (ErrVar - RMDI) > RMDItol) THEN

    SDiff = (element(J) % value - Backgr(J)) ** 2 / ErrVar
    PdGood = EXP (-0.5 * MIN (SDiff, 2.0 * ExpArgMax)) / SQRT (2.0 * Pi * ErrVar)
                                                                ! eqn 2.6
    PdTotal(J) = PdBad(J) * element(J) % PGEFinal + PdGood * (1.0 - element(J) % PGEFinal)  ! eqn 2.8
    PGEBk = PdBad(J) * element(J) % PGEFinal / PdTotal(J)                   ! eqn 2.9
    element(J) % flags = IBSET (element(J) % flags, BackPerfFlag)

    IF (PGEBk >= PGECrit) element(J) % flags = IBSET (element(J) % flags, BackRejectFlag)
  ELSE                                ! Missing data values
    PdTotal(J) = PdBad(J)             ! Gives correct ReportPGE
    SDiff = SDiffCrit            ! Set flags below
    PGEBk = PGEMDI               ! Missing data indicator for packed PGEs
  END IF
  element(J) % PGEFinal = AINT (PGEBk * PPF) + element(J) % PGEFinal ! Packing of PGEs

  IF (.NOT. PrelimRun .AND. &         ! Not applied to reported level sonde data
      (SDiff >= SDiffCrit .OR. &
       BTEST (element(J) % flags, PermRejectFlag) .OR. &
       BTEST (element(J) % flags, FinalRejectFlag))) THEN
    PGEBk = PGEMDI                 ! => do not buddy check
    element(J) % flags = IBSET (element(J) % flags, FinalRejectFlag)
  END IF
  IF (PRESENT (PGEBd)) PGEBd(J) = PGEBk

END DO  ! J: loop over observations

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_QcBackgr1s
