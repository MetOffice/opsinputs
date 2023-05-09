!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! New dynamics routine.
! Calculates Rho (A) and Theta (B) level heights
!
! Uses ND prescribed method for calculating heights using orography.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CXZND (CX)

USE GenMod_Core, ONLY: &
    gen_fail,          &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE OpsMod_MiscTypes, ONLY: &
    ZCodeLevelsA,           &
    ZCodeLevelsB

IMPLICIT NONE

! Subroutine arguments:
TYPE (CX_Type), INTENT(INOUT) :: CX

! Local declarations:
INTEGER                       :: j
INTEGER                       :: k
INTEGER                       :: NumLocal
INTEGER                       :: NumLevs
INTEGER                       :: NumModLevs
REAL, ALLOCATABLE             :: ZA(:)
REAL, ALLOCATABLE             :: ZB(:)
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_CXZND"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (.NOT. CX % Header % NewDynamics) THEN
  CALL gen_fail (RoutineName,                      &
                 'Call to CXZND for old dynamics')
END IF

IF (CX % Header % Orog % Present .AND. &
    ASSOCIATED (CX % Header % EtaTheta) .AND. &
    ASSOCIATED (CX % Header % EtaRho) .AND. &
    CX % Header % NewDynamics .AND. &
    .NOT. CX % Header % ZB % Present .AND. &
    .NOT. CX % Header % ZA % Present) THEN

  NumLocal = SIZE (CX % Orog(:))
  NumLevs = CX % Header % NumLevels
  NumModLevs = SIZE (CX % Header % EtaTheta(:))

  ALLOCATE (CX % ZB(NumLocal,NumLevs))
  ALLOCATE (CX % ZA(NumLocal,NumLevs + 1))
  ALLOCATE (ZA(NumModLevs + 1))
  ALLOCATE (ZB(NumModLevs))

  CX % Header % ZA % Present = .TRUE.
  CX % Header % ZA % ZCode  = ZCodeLevelsA
  CX % Header % ZA % NumLev = NumLevs + 1
  CX % Header % ZB % Present = .TRUE.
  CX % Header % ZB % ZCode  = ZCodeLevelsB
  CX % Header % ZB % NumLev = NumLevs

  DO j = 1, NumLocal
    ZB(:) = CX % Header % EtaTheta(:) * CX % Header % ZModelTop
    ZA(1:NumModLevs) = CX % Header % EtaRho (:) * CX % Header % ZModelTop

    ! Calculate height of extra rho level
    ZA(NumModLevs+1) = ZB(NumModLevs) * 2.0 - ZA(NumModLevs)

    ! Quadratic method
    DO k = 1, CX % Header % FirstConstantRhoLevel - 1
      ZB(k) = ZB(k) + CX % Orog(j)* (1.0 - CX % Header % EtaTheta(k) / &
                    CX % Header % EtaRho(CX % Header % FirstConstantRhoLevel)) ** 2
      ZA(k) = ZA(k) + CX % Orog(j) *(1.0 - CX % Header % EtaRho(k) / &
                    CX % Header % EtaRho(CX % Header % FirstConstantRhoLevel)) ** 2
    END DO

    CX % ZA(j,:) = ZA(1:NumLevs + 1)
    CX % ZB(j,:) = ZB(1:NumLevs)

  END DO

  DEALLOCATE (ZA)
  DEALLOCATE (ZB)

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CXZND
