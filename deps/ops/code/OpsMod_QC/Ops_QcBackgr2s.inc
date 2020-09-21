!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Background check for scalar quantities for multi-level data (with a constant
! number of levels)
!
! Method:  see OSDP 2, section 2
!
! Inputs:  see argument list
!
! Outputs: PGE/flags - see argument list
!-------------------------------------------------------------------------------

SUBROUTINE Ops_QcBackgr2s (element,   & ! Element type contains obs info
                           Backgr,    & ! Background value
                           BackgrErr, & ! Estimated error of background value
                           PdBad)       ! Probability density of obs with gross errors

USE GenMod_Constants, ONLY: &
    RMDItol                   ! Tolerance for RMDI

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    Pi,                                  &
    RMDI

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

USE OpsMod_QC, ONLY: &
    ExpArgMax,       &  ! Max argument for EXP function
    PGECrit             ! Rejection limit

IMPLICIT NONE

! Subroutine arguments:
TYPE (Element_type), INTENT(INOUT) :: element(:,:)   ! Element type containing obs info
REAL, INTENT(IN)                   :: Backgr(:,:)    ! Background value
REAL, INTENT(IN)                   :: BackgrErr(:,:) ! Estimated error of background value
REAL, INTENT(IN)                   :: PdBad          ! Probability density of 'bad' obs

! Local declarations:
INTEGER                            :: J              ! Loops over observations
INTEGER                            :: Lev            ! Loops over observation levels
REAL                               :: SDiff          ! Squared difference from background / ErrVar
REAL                               :: ErrVar         ! Combined (ob and background) Error Variance
REAL                               :: PdGood         ! Probability density of good ob - normal distribution
REAL                               :: PGEBk          ! PGE after background check
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_QcBackgr2s"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

DO J = 1,SIZE (element, DIM = 1)
  DO Lev = 1,SIZE (element, DIM = 2)

    IF (element(J,Lev) % oberr >= 0 .AND. BackgrErr(J,Lev) >= 0) THEN
      ErrVar = (ObErrMult * element(J,Lev) % ObErr) ** 2 + &      ! modified vn of eqn 2.5
               (BackgrErrMult * BackgrErr(J,Lev)) ** 2  ! for checking sensitivity
    ELSE
      ErrVar = RMDI
    END IF

    IF (ABS (element(J,Lev) % value - RMDI) > RMDItol .AND.  &
        ABS (Backgr(J,Lev) - RMDI) > RMDItol .AND. &
        ABS (ErrVar - RMDI) > RMDITol) THEN

      SDiff = (element(J,Lev) % value - Backgr(J,Lev)) ** 2 / ErrVar
      PdGood = EXP (-MIN (0.5 * SDiff, ExpArgMax)) / SQRT (2.0 * Pi * ErrVar)
                                                               ! eqn 2.6
      PGEBk = PdBad * element(J,Lev) % PGEFinal / (PdBad * element(J,Lev) % PGEFinal + PdGood * (1.0 - element(J,Lev) % PGEFinal))
                                                               ! eqn 2.9

      element(J,Lev) % Flags = IBSET (element(J,Lev) % Flags,BackPerfFlag)
      IF (PGEBk >= PGECrit) element(J,Lev) % Flags = IBSET (element(J,Lev) % Flags,BackRejectFlag)

    ELSE                       ! Missing data values
      SDiff = SDiffCrit   ! Set flags below
      PGEBk = PGEMDI      ! Missing data indicator for packed PGEs
    END IF
    element(J,Lev) % PGEFinal = AINT (PGEBk * PPF) + element(J,Lev) % PGEFinal   ! Packing of PGEs

    IF (BTEST (element(J,Lev) % Flags,PermRejectFlag)) THEN
      element(J,Lev) % Flags = IBSET (element(J,Lev) % Flags,FinalRejectFlag)
    END IF

  END DO  ! Lev: loop over levels
END DO  ! J: loop over observations

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_QcBackgr2s
