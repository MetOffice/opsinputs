!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Converts flight level to pressure.
!
! Calls the routine Ops_HeightToPressure to convert aircraft flight levels to
! pressure. Only overwrites pressure array where flight level is present.
!
! Inputs: Obs % FlightLevel(:) - Obs structure flight level array
!
! Outputs: Obs % PLevelsA(:,1) - Obs structure pressure array
!-------------------------------------------------------------------------------

SUBROUTINE Ops_FlightLevel (Obs)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE OpsMod_MiscTypes, ONLY: &
    ZcodeLevelsA

USE OpsMod_ObsInfo, ONLY:       &
    DerivedFromFlightLevelFlag, &
    OB_type,                    &
    Ops_Alloc

USE OpsMod_Utilities, ONLY: &
    Ops_HeightToPressure

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(INOUT) :: Obs

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = 'Ops_FlightLevel'
INTEGER                       :: NumObsLocal ! Number of observations on this PE
REAL, ALLOCATABLE             :: Pressures(:)

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (Obs % header % FlightLevel % Present) THEN
  NumObsLocal = Obs % header % NumObsLocal

  ! Set up PlevelsA if this has not already been set up

  CALL Ops_Alloc (Obs % header % PlevelsA, &
                  "Obs % PlevelsA",        &
                  NumObsLocal,             &
                  Obs % PLevelsA,          &
                  num_levels = 1,          &
                  zcode = ZCodeLevelsA)

  ! Convert flight level to pressure
  ALLOCATE (Pressures(NumObsLocal))
  Pressures(:) = RMDI

  CALL Ops_HeightToPressure (Obs % FlightLevel, &
                             NumObsLocal,       &
                             Pressures)

  WHERE (Obs % PLevelsA(:,1) % value == RMDI)
    Obs % PLevelsA(:,1) % value = Pressures(:)
    Obs % PLevelsA(:,1) % Flags = IBSET (Obs % PLevelsA(:,1) % Flags, DerivedFromFlightLevelFlag)
  END WHERE

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_FlightLevel
