!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Outputs a structure (VarobPGEs) which contains the initial and final PGEs
! required for varobs final.
!
! The final PGE is either the PGE after background check, or after the buddy
! check if it is performed. The PGEs stored in the output structure are packed
! as required by VAR.  This has to be done for
! every variable that is present.
!
! Inputs: Structures containing PGEs after background check and (if present)
!         after buddy check
! Outputs: A structure containing packed PGEs required by VAR
!-------------------------------------------------------------------------------

SUBROUTINE Ops_VarobPGEs (Obs,       &
                          VarobPGEs, &
                          PGEBd)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE OpsMod_AODGeneral, ONLY: &
    NAODWaves

USE OpsMod_Constants, ONLY: &
    PPF,                    &
    PGEMDI

USE OpsMod_GPSRO, ONLY: &
    GPSRO_TPD

USE OpsMod_ModelObInfo, ONLY: &
    ModelOb_type,             &
    Ops_ModelOBAllocate

USE OpsMod_ObsInfo, ONLY: &
    FinalRejectFlag,      &
    NoAssimFlag,          &
    Ob_type,              &
    Ops_Alloc

USE OpsMod_Radar, ONLY:        &
    RadWind_SuperOb,           &
    RadPerformSuperObservation

IMPLICIT NONE

! Subroutine arguments:
TYPE (Ob_type), INTENT(IN)                :: Obs          ! Initial and background PGEs
TYPE (ModelOb_type), INTENT(OUT)          :: VarobPGEs    ! PGEs for varobs
TYPE (ModelOb_type), INTENT(IN), OPTIONAL :: PGEBd        ! Buddy check PGEs (if performed)

! Local declarations:
CHARACTER(len=*), PARAMETER               :: RoutineName = "Ops_VarobPGEs"
INTEGER                                   :: Ob
INTEGER                                   :: Lev ! Loop variables
INTEGER                                   :: NumObsLocal
INTEGER                                   :: NumLev
INTEGER                                   :: FlagBits
REAL, ALLOCATABLE                         :: PGEfi(:,:)
REAL, ALLOCATABLE                         :: PGEfi2(:,:)
INTEGER                                   :: Zero
INTEGER                                   :: Iwav

!-------------------------------------------------------------------------------
! 0. Tracing
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

Zero = 0

FlagBits = IBSET (Zero,FinalRejectFlag) + IBSET (Zero,NoAssimFlag)

!-------------------------------------------------------------------------------
! 1. Put PGEs int VarobPGEs structure for elements that are present
!-------------------------------------------------------------------------------

NumObsLocal = Obs % header % NumObsLocal

CALL VarobPGES % init

CALL Ops_ModelOBAllocate (Obs % header, &
                          VarobPGEs)

! Single level data

IF (Obs % header % pstar % Present) THEN
  ALLOCATE (PGEfi(NumObsLocal,1))
  IF (PRESENT (PGEBd)) THEN
    IF (PGEBd % header % pstar % Present) THEN
      WHERE (PGEBd % pstar /= RMDI)
        PGEfi(:,1) = PGEBd % pstar(:)
      ELSEWHERE
        PGEfi(:,1) = AINT (Obs % pstar(:) % PGEFinal) / PPF
      END WHERE
    ELSE
      PGEfi(:,1) = AINT (Obs % pstar(:) % PGEFinal) / PPF
    END IF
  ELSE
    PGEfi(:,1) = AINT (Obs % pstar(:) % PGEFinal) / PPF
  END IF
  DO Ob = 1, NumObsLocal
    IF (IAND (FlagBits,Obs % pstar(Ob) % Flags) /= 0 .AND. &
        PGEfi(Ob,1) < 0.5) PGEfi(Ob,1) = 1.0
  END DO
  VarobPGEs % pstar(:) = REAL (INT (PGEfi(:,1) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

IF (Obs % header % t2 % Present) THEN
  ALLOCATE (PGEfi(NumObsLocal,1))
  IF (PRESENT (PGEBd)) THEN
    IF (PGEBd % header % t2 % Present) THEN
      WHERE (PGEBd % t2 /= RMDI)
        PGEfi(:,1) = PGEBd % t2(:)
      ELSEWHERE
        PGEfi(:,1) = AINT (Obs % t2(:) % PGEFinal) / PPF
      END WHERE
    ELSE
      PGEfi(:,1) = AINT (Obs % t2(:) % PGEFinal) / PPF
    END IF
  ELSE
    PGEfi(:,1) = AINT (Obs % t2(:) % PGEFinal) / PPF
  END IF
  DO Ob = 1, NumObsLocal
    IF (IAND (FlagBits,Obs % t2(Ob) % Flags) /= 0 .AND. &
      PGEfi(Ob,1) < 0.5) PGEfi(Ob,1) = 1.0
  END DO
  VarobPGEs % t2(:) = REAL (INT (PGEfi(:,1) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

IF (Obs % header % rh2 % Present) THEN
  ALLOCATE (PGEfi(NumObsLocal,1))
  IF (PRESENT (PGEBd)) THEN
    IF (PGEBd % header % rh2 % Present) THEN
      WHERE (PGEBd % rh2 /= RMDI)
        PGEfi(:,1) = PGEBd % rh2(:)
      ELSEWHERE
        PGEfi(:,1) = AINT (Obs % rh2(:) % PGEFinal) / PPF
      END WHERE
    ELSE
      PGEfi(:,1) = AINT (Obs % rh2(:) % PGEFinal) / PPF
    END IF
  ELSE
    PGEfi(:,1) = AINT (Obs % rh2(:) % PGEFinal) / PPF
  END IF
  DO Ob = 1, NumObsLocal
    IF (IAND (FlagBits,Obs % rh2(Ob) % Flags) /= 0 .AND. &
        PGEfi(Ob,1) < 0.5) PGEfi(Ob,1) = 1.0
  END DO
  VarobPGEs % rh2(:) = REAL (INT (PGEfi(:,1) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

IF (Obs % header % u10 % Present .AND. &
    Obs % header % v10 % Present) THEN
  ALLOCATE (PGEfi(NumObsLocal,1))
  IF (PRESENT (PGEBd)) THEN
    IF (PGEBd % header % u10 % Present .AND. &
        PGEBd % header % v10 % Present) THEN
      WHERE (PGEBd % u10 /= RMDI)
        PGEfi(:,1) = PGEBd % u10(:)
      ELSEWHERE
        PGEfi(:,1) = AINT (Obs % u10(:) % PGEFinal) / PPF
      END WHERE
    ELSE
      PGEfi(:,1) = AINT (Obs % u10(:) % PGEFinal) / PPF
    END IF
  ELSE
    PGEfi(:,1) = AINT (Obs % u10(:) % PGEFinal) / PPF
  END IF
  DO Ob = 1, NumObsLocal
    IF (IAND (FlagBits,Obs % u10(Ob) % Flags) /= 0 .AND. &
      PGEfi(Ob,1) < 0.5) PGEfi(Ob,1) = 1.0
  END DO
  VarobPGEs % u10(:) = REAL (INT (PGEfi(:,1) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

! For logvis PGE take values from Observations structure even if PGEBd structure
! is present. This is done because logvis is not buddy checked so the PGEBd
! structure contains invalid values.

IF (Obs % header % logvis % Present) THEN
  ALLOCATE (PGEfi(NumObsLocal,1))
  PGEfi(:,1) = AINT (Obs % logvis(:) % PGEFinal) / PPF
  DO Ob = 1, NumObsLocal
    IF (IAND (FlagBits,Obs % logvis(Ob) % Flags) /= 0 .AND. &
        PGEfi(Ob,1) < 0.5) PGEfi(Ob,1) = 1.0
    IF (Obs % logvis(Ob) % Value == RMDI) PGEfi(Ob,1) = PGEMDI
  END DO
  VarobPGEs % logvis(:) = REAL (INT (PGEfi(:,1) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

IF (Obs % header % SST % Present) THEN
  ALLOCATE (PGEfi(NumObsLocal,1))
  IF (PRESENT (PGEBd)) THEN
    IF (PGEBd % header % SST % Present) THEN
      WHERE (PGEBd % SST /= RMDI)
        PGEfi(:,1) = PGEBd % SST(:)
      ELSEWHERE
        PGEfi(:,1) = AINT (Obs % SST(:) % PGEFinal) / PPF
      END WHERE
    ELSE
      PGEfi(:,1) = AINT (Obs % SST(:) % PGEFinal) / PPF
    END IF
  ELSE
    PGEfi(:,1) = AINT (Obs % SST(:) % PGEFinal) / PPF
  END IF
  DO Ob = 1, NumObsLocal
    IF (IAND (FlagBits,Obs % SST(Ob) % Flags) /= 0 .AND. &
        PGEfi(Ob,1) < 0.5) PGEfi(Ob,1) = 1.0
  END DO
  VarobPGEs % SST(:) = REAL (INT (PGEfi(:,1) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

IF (Obs % header % WindSpeed % Present) THEN
  ALLOCATE (PGEfi(NumObsLocal,1))
  IF (PRESENT (PGEBd)) THEN
    IF (PGEBd % header % WindSpeed % Present) THEN
      WHERE (PGEBd % WindSpeed /= RMDI)
        PGEfi(:,1) = PGEBd % WindSpeed(:)
      ELSEWHERE
        PGEfi(:,1) = AINT (Obs % WindSpeed(:) % PGEFinal) / PPF
      END WHERE
    ELSE
      PGEfi(:,1) = AINT (Obs % WindSpeed(:) % PGEFinal) / PPF
    END IF
  ELSE
    PGEfi(:,1) = AINT (Obs % WindSpeed(:) % PGEFinal) / PPF
  END IF
  DO Ob = 1, NumObsLocal
    IF (IAND (FlagBits,Obs % WindSpeed(Ob) % Flags) /= 0 .AND. &
        PGEfi(Ob,1) < 0.5) PGEfi(Ob,1) = 1.0
    IF (Obs % WindSpeed(Ob) % Value == RMDI) PGEfi(Ob,1) = PGEMDI
  END DO
  VarobPGEs % WindSpeed(:) = REAL (INT (PGEfi(:,1) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

IF (Obs % header % TCWV % Present) THEN
  ALLOCATE (PGEfi(NumObsLocal,1))
  IF (PRESENT (PGEBd)) THEN
    IF (PGEBd % header % TCWV % Present) THEN
      WHERE (PGEBd % TCWV /= RMDI)
        PGEfi(:,1) = PGEBd % TCWV(:)
      ELSEWHERE
        PGEfi(:,1) = AINT (Obs % TCWV(:) % PGEFinal) / PPF
      END WHERE
    ELSE
      PGEfi(:,1) = AINT (Obs % TCWV(:) % PGEFinal) / PPF
    END IF
  ELSE
    PGEfi(:,1) = AINT (Obs % TCWV(:) % PGEFinal) / PPF
  END IF
  DO Ob = 1, NumObsLocal
    IF (IAND (FlagBits,Obs % TCWV(Ob) % Flags) /= 0 .AND. &
        PGEfi(Ob,1) < 0.5) PGEfi(Ob,1) = 1.0
    IF (Obs % TCWV(Ob) % Value == RMDI) PGEfi(Ob,1) = PGEMDI
  END DO
  VarobPGEs % TCWV(:) = REAL (INT (PGEfi(:,1) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

! AOD
IF (Obs % header % AOD % Present) THEN
  ALLOCATE (PGEfi(NumObsLocal,NAODWaves))
  IF (PRESENT (PGEBd)) THEN
    IF (PGEBd % header % AOD % Present) THEN
      WHERE (PGEBd % AOD /= RMDI)
        PGEfi(:,:) = PGEBd % AOD(:,:)
      ELSEWHERE
        PGEfi(:,:) = AINT (Obs % AOD(:,:) % PGEFinal) / PPF
      END WHERE
    ELSE
      PGEfi(:,:) = AINT (Obs % AOD(:,:) % PGEFinal) / PPF
    END IF
  ELSE
    PGEfi(:,:) = AINT (Obs % AOD(:,:) % PGEFinal) / PPF
  END IF
  DO Ob = 1, NumObsLocal
    DO Iwav = 1, NAODWaves
      IF (IAND (FlagBits,Obs % AOD(Ob,Iwav) % Flags) /= 0 .AND. &
          PGEfi(Ob,Iwav) < 0.5) PGEfi(Ob,Iwav) = 1.0
      IF (Obs % AOD(Ob,Iwav) % Value == RMDI) PGEfi(Ob,Iwav) = PGEMDI
    END DO
  END DO
  VarobPGEs % AOD(:,:) = REAL (INT (PGEfi(:,:) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

IF (Obs % header % RainAccum % Present) THEN
  ALLOCATE (PGEfi(NumObsLocal,1))
  IF (PRESENT (PGEBd)) THEN
    IF (PGEBd % header % RainAccum % Present) THEN
      WHERE (PGEBd % RainAccum /= RMDI)
        PGEfi(:,1) = PGEBd % RainAccum(:)
      ELSEWHERE
        PGEfi(:,1) = AINT (Obs % RainAccum(:) % PGEFinal) / PPF
      END WHERE
    ELSE
      PGEfi(:,1) = AINT (Obs % RainAccum(:) % PGEFinal) / PPF
    END IF
  ELSE
    PGEfi(:,1) = AINT (Obs % RainAccum(:) % PGEFinal) / PPF
  END IF
  DO Ob = 1, NumObsLocal
    IF (IAND (FlagBits,Obs % RainAccum(Ob) % Flags) /= 0 .AND. &
        PGEfi(Ob,1) < 0.5) PGEfi(Ob,1) = 1.0
    IF (Obs % RainAccum(Ob) % Value == RMDI) PGEfi(Ob,1) = PGEMDI
  END DO
  VarobPGEs % RainAccum(:) = REAL (INT (PGEfi(:,1) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

! LWP
IF (Obs % header % LWP % Present) THEN
  ALLOCATE (PGEfi(NumObsLocal,1))
  IF (PRESENT (PGEBd)) THEN
    IF (PGEBd % header % LWP % Present) THEN
      WHERE (PGEBd % LWP /= RMDI)
        PGEfi(:,1) = PGEBd % LWP(:)
      ELSEWHERE
        PGEfi(:,1) = AINT (Obs % LWP(:) % PGEFinal) / PPF
      END WHERE
    ELSE
      PGEfi(:,1) = AINT (Obs % LWP(:) % PGEFinal) / PPF
    END IF
  ELSE
    PGEfi(:,1) = AINT (Obs % LWP(:) % PGEFinal) / PPF
  END IF
  DO Ob = 1, NumObsLocal
    IF (IAND (FlagBits,Obs % LWP(Ob) % Flags) /= 0 .AND. &
        PGEfi(Ob,1) < 0.5) PGEfi(Ob,1) = 1.0
    IF (Obs % LWP(Ob) % Value == RMDI) PGEfi(Ob,1) = PGEMDI
  END DO
  VarobPGEs % LWP(:) = REAL (INT (PGEfi(:,1) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

IF (Obs % header % GPSTZDelay % Present) THEN
  ALLOCATE (PGEfi(NumObsLocal,1))
  IF (PRESENT (PGEBd)) THEN
    IF (PGEBd % header % GPSTZDelay % Present) THEN
      WHERE (PGEBd % GPSTZDelay /= RMDI)
        PGEfi(:,1) = PGEBd % GPSTZDelay(:)
      ELSEWHERE
        PGEfi(:,1) = AINT (Obs % GPSTZDelay(:) % PGEFinal) / PPF
      END WHERE
    ELSE
      PGEfi(:,1) = AINT (Obs % GPSTZDelay(:) % PGEFinal) / PPF
    END IF
  ELSE
    PGEfi(:,1) = AINT (Obs % GPSTZDelay(:) % PGEFinal) / PPF
  END IF
  DO Ob = 1, NumObsLocal
    IF (IAND (FlagBits,Obs % GPSTZDelay(Ob) % Flags) /= 0 .AND. &
        PGEfi(Ob,1) < 0.5) PGEfi(Ob,1) = 1.0
    IF (Obs % GPSTZDelay(Ob) % Value == RMDI) PGEfi(Ob,1) = PGEMDI
  END DO
  VarobPGEs % GPSTZDelay(:) = REAL (INT (PGEfi(:,1) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

! Mass Density for chemical constituents
IF (Obs % header % MASS_DNSTY % Present) THEN
  ALLOCATE (PGEfi(NumObsLocal,1))
  PGEfi(:,1) = AINT (Obs % MASS_DNSTY(:) % PGEFinal) / PPF
  DO Ob = 1, NumObsLocal
    IF (IAND (FlagBits,Obs % MASS_DNSTY(Ob) % Flags) /= 0 .AND. &
        PGEfi(Ob,1) < 0.5) PGEfi(Ob,1) = 1.0
    IF (Obs % MASS_DNSTY(Ob) % Value == RMDI) PGEfi(Ob,1) = PGEMDI
  END DO
  VarobPGEs % MASS_DNSTY(:) = REAL (INT (PGEfi(:,1) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

! scat ambiguous winds
IF (Obs % header % u10AmbWind % Present .AND. &
    Obs % header % v10AmbWind % Present) THEN
  NumLev = Obs % header % u10AmbWind % NumLev
  ALLOCATE (PGEfi(NumObsLocal,NumLev))
  IF (PRESENT (PGEBd)) THEN
    IF (PGEBd % header % u10AmbWind % Present) THEN
      WHERE (PGEBd % u10AmbWind /= RMDI)
        PGEfi(:,:) = PGEBd % u10AmbWind(:,:)
      ELSEWHERE
        PGEfi(:,:) = AINT (Obs % u10AmbWind(:,:) % PGEFinal) / PPF
      END WHERE
    ELSE
      PGEfi(:,:) = AINT (Obs % u10AmbWind(:,:) % PGEFinal) / PPF
    END IF
  ELSE
    PGEfi(:,:) = AINT (Obs % u10AmbWind(:,:) % PGEFinal) / PPF
  END IF
  DO Ob = 1, NumObsLocal
    DO Lev = 1, NumLev
      IF (PGEfi(Ob,Lev) < 0.5 .AND. &
          IAND (FlagBits, Obs % u10AmbWind(Ob,Lev) % Flags) /= 0) PGEfi(Ob,Lev) = 1.0
    END DO
  END DO
  VarobPGEs % u10AmbWind(:,:) = REAL (INT (PGEfi(:,:) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

! scat prior pge for use in var qc. Note this is directly packed
IF (Obs % header % AWPriorPcorrect % Present) THEN
  VarobPGEs % AWPriorPcorrect(:,:) = &
    REAL (INT (Obs % AWPriorPcorrect(:,:) % PGEFinal * 10000.0))
END IF

! Doppler radar radial velocity
IF (RadWind_SuperOb) THEN
  CALL Ops_Alloc (VarobPGEs % Header % RadialVelocity, &
                  ' VarobPGEs % RadialVelocity',       &
                  NumObsLocal,                         &
                  Real2 =  VarobPGEs % RadialVelocity)
  IF (Obs % header % RadialVelocSO % Present) THEN
    VarobPGEs % RadialVelocity(:,:) =  Obs % RadialVelocSO(:,:) % PGEFinal
  END IF
ELSE
  CALL Ops_Alloc (VarobPGEs % Header % RadialVelocity, &
                  ' VarobPGEs % RadialVelocity',       &
                  NumObsLocal,                         &
                  Real2 =  VarobPGEs % RadialVelocity)
  IF (Obs % header % RadialVelocity % Present) THEN
    VarobPGEs % RadialVelocity(:,:) =  Obs % RadialVelocity(:,:) % PGEFinal
  END IF
END IF

! Radar reflectivity
IF (RadPerformSuperObservation) THEN
  CALL Ops_Alloc (VarobPGEs % Header % Reflectivity, &
                  ' VarobPGEs % Reflectivity',       &
                  NumObsLocal,                       &
                  Real2 =  VarobPGEs % Reflectivity)
  IF (Obs % header % ReflectivitySO % Present) THEN
    VarobPGEs % Reflectivity(:,:) =  Obs % ReflectivitySO(:,:) % PGEFinal
  END IF
ELSE
  IF (Obs % header % Reflectivity % Present) THEN
    VarobPGEs % Reflectivity(:,:) =  Obs % Reflectivity(:,:) % PGEFinal
  END IF
END IF

! Upper air data

IF (Obs % header % t % Present) THEN
  NumLev = Obs % header % t % NumLev
  ALLOCATE (PGEfi(NumObsLocal,NumLev))
  IF (PRESENT (PGEBd)) THEN
    IF (PGEBd % header % t % Present) THEN
      WHERE (PGEBd % t /= RMDI)
        PGEfi(:,:) = PGEBd % t(:,:)
      ELSEWHERE
        PGEfi(:,:) = AINT (Obs % t(:,:) % PGEFinal) / PPF
      END WHERE
    ELSE
      PGEfi(:,:) = AINT (Obs % t(:,:) % PGEFinal) / PPF
    END IF
  ELSE
    PGEfi(:,:) = AINT (Obs % t(:,:) % PGEFinal) / PPF
  END IF
  DO Ob = 1, NumObsLocal
    DO Lev = 1, NumLev
      IF (IAND (FlagBits,Obs % t(Ob,Lev) % Flags) /= 0 .AND. &
          PGEfi(Ob,Lev) < 0.5) PGEfi(Ob,1) = 1.0
    END DO
  END DO
  IF (Obs % header % theta % Present) THEN
    ALLOCATE (PGEfi2(NumObsLocal,NumLev))
    PGEfi2(:,:) = PGEfi(:,:)
    DO Ob = 1, NumObsLocal
      DO Lev = 1, NumLev
        IF (Obs % theta(Ob,Lev) % Value == RMDI) THEN
          PGEfi2(Ob,Lev) = PGEMDI
        END IF
      END DO
    END DO
    VarobPGEs % theta(:,:) = REAL (INT (PGEfi2(:,:) * 10000.0))
    DEALLOCATE (PGEfi2)
  END IF
  VarobPGEs % t(:,:) = REAL (INT (PGEfi(:,:) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

IF (Obs % header % refrac % Present) THEN
  VarobPGEs % Refrac(:,:) = 10000.0 * Obs % Refrac(:,:) % PGEFinal
END IF

IF (Obs % header % z % Present) THEN
  NumLev = Obs % header % z % NumLev
  ALLOCATE (PGEfi(NumObsLocal,NumLev))
  PGEfi(:,:) = AINT (Obs % z(:,:) % PGEFinal) / PPF
  DO Ob = 1, NumObsLocal
    DO Lev = 1, NumLev
      IF (PGEfi(Ob,Lev) < 0.5 .AND. &
          IAND (FlagBits,Obs % z(Ob,Lev) % Flags) /= 0) PGEfi(Ob,Lev) = 1.0
      IF (Obs % z(Ob,Lev) % Value == RMDI) THEN
        PGEfi(Ob,Lev) = PGEMDI
      END IF
    END DO
  END DO
  VarobPGEs % z(:,:) = REAL (INT (PGEfi(:,:) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

IF (GPSRO_TPD) THEN
  IF (Obs % header % BendingAngleAll % Present) THEN
    ! Note this is directly packed
    VarobPGEs % BendingAngleAll(:,:) = 10000.0 * Obs % BendingAngleAll(:,:) % PGEFinal
  END IF

  IF (Obs % header % ImpactParamAll % Present) THEN
    ! Note this is directly packed
    VarobPGEs % ImpactParamAll(:,:) = 10000.0 * Obs % ImpactParamAll(:,:) % PGEFINAL
  END IF
ELSE
  IF (Obs % header % BendingAngle % Present) THEN
    ! Note this is directly packed
    VarobPGEs % BendingAngle(:,:) = 10000.0 * Obs % BendingAngle(:,:) % PGEFinal
  END IF

  IF (Obs % header % ImpactParam % Present) THEN
    ! Note this is directly packed
    VarobPGEs % ImpactParam(:,:) = 10000.0 * Obs % ImpactParam(:,:) % PGEFINAL
  END IF
END IF

IF (Obs % header % RO_Rad_Curv % Present) THEN
  ! Note this is directly packed
  VarobPGEs % RO_Rad_Curv(:) = 10000.0 * Obs % RO_Rad_Curv(:) % PGEFinal
END IF

IF (Obs % header % RO_geoid_und % Present) THEN
  ! Note this is directly packed
  VarobPGEs % RO_geoid_und(:) = 10000.0 * Obs % RO_geoid_und(:) % PGEFinal
END IF

IF (Obs % header % rh % Present) THEN
  NumLev = Obs % header % rh % NumLev
  ALLOCATE (PGEfi(NumObsLocal,NumLev))
  IF (PRESENT (PGEBd)) THEN
    IF (PGEBd % header % rh % Present) THEN
      WHERE (PGEBd % rh /= RMDI)
        PGEfi(:,:) = PGEBd % rh(:,:)
      ELSEWHERE
        PGEfi(:,:) = AINT (Obs % rh(:,:) % PGEFinal) / PPF
      END WHERE
    ELSE
      PGEfi(:,:) = AINT (Obs % rh(:,:) % PGEFinal) / PPF
    END IF
  ELSE
    PGEfi(:,:) = AINT (Obs % rh(:,:) % PGEFinal) / PPF
  END IF
  DO Ob = 1, NumObsLocal
    DO Lev = 1, NumLev
      IF (PGEfi(Ob,Lev) < 0.5 .AND. &
          IAND (FlagBits,Obs % rh(Ob,Lev) % Flags) /= 0) PGEfi(Ob,Lev) = 1.0
      IF (Obs % rh(Ob,Lev) % Value == RMDI) THEN
        PGEfi(Ob,Lev) = PGEMDI
      END IF
    END DO
  END DO
  VarobPGEs % rh(:,:) = REAL (INT (PGEfi(:,:) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

IF (Obs % header % u % Present) THEN
  NumLev = Obs % header % u % NumLev
  ALLOCATE (PGEfi(NumObsLocal,NumLev))
  IF (PRESENT (PGEBd)) THEN
    IF (PGEBd % header % u % Present) THEN
      WHERE (PGEBd % u /= RMDI)
        PGEfi(:,:) = PGEBd % u(:,:)
      ELSEWHERE
        PGEfi(:,:) = AINT (Obs % u(:,:) % PGEFinal) / PPF
      END WHERE
    ELSE
      PGEfi(:,:) = AINT (Obs % u(:,:) % PGEFinal) / PPF
    END IF
  ELSE
    PGEfi(:,:) = AINT (Obs % u(:,:) % PGEFinal) / PPF
  END IF
  DO Ob = 1, NumObsLocal
    DO Lev = 1, NumLev
      IF (PGEfi(Ob,Lev) < 0.5 .AND. &
          IAND (FlagBits,Obs % u(Ob,Lev) % Flags) /= 0) PGEfi(Ob,Lev) = 1.0
    END DO
  END DO
  VarobPGEs % u(:,:) = REAL (INT (PGEfi(:,:) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

IF (Obs % header % Cloud % Present) THEN
  NumLev = Obs % header % Cloud % NumLev
  ALLOCATE (PGEfi(NumObsLocal,NumLev))
  IF (PRESENT (PGEBd)) THEN
    IF (PGEBd % header % Cloud % Present) THEN
      WHERE (PGEBd % Cloud /= RMDI)
        PGEfi(:,:) = PGEBd % Cloud(:,:)
      ELSEWHERE
        PGEfi(:,:) = AINT (Obs % Cloud(:,:) % PGEFinal) / PPF
      END WHERE
    ELSE
      PGEfi(:,:) = AINT (Obs % Cloud(:,:) % PGEFinal) / PPF
    END IF
  ELSE
    PGEfi(:,:) = AINT (Obs % Cloud(:,:) % PGEFinal) / PPF
  END IF
  DO Ob = 1, NumObsLocal
    DO Lev = 1, NumLev
      IF (IAND (FlagBits,Obs % Cloud(Ob,Lev) % Flags) /= 0 .AND. &
          PGEfi(Ob,Lev) < 0.5) PGEfi(Ob,Lev) = 1.0
      IF (Obs % Cloud(Ob,Lev) % Value == RMDI) THEN
        PGEfi(Ob,Lev) = PGEMDI
      END IF
    END DO
  END DO
  VarobPGEs % Cloud(:,:) = REAL (INT (PGEfi(:,:) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

IF (Obs % header % Rainrate % Present) THEN
  NumLev = Obs % header % Rainrate % NumLev
  ALLOCATE (PGEfi(NumObsLocal,NumLev))
  IF (PRESENT (PGEBd)) THEN
    IF (PGEBd % header % Rainrate % Present) THEN
      WHERE (PGEBd % Rainrate /= RMDI)
        PGEfi(:,:) = PGEBd % Rainrate(:,:)
      ELSEWHERE
        PGEfi(:,:) = AINT (Obs % Rainrate(:,:) % PGEFinal) / PPF
      END WHERE
    ELSE
      PGEfi(:,:) = AINT (Obs % Rainrate(:,:) % PGEFinal) / PPF
    END IF
  ELSE
    PGEfi(:,:) = AINT (Obs % Rainrate(:,:) % PGEFinal) / PPF
  END IF
  DO Ob = 1, NumObsLocal
    DO Lev = 1, NumLev
      IF (IAND (FlagBits,Obs % Rainrate(Ob,Lev) % Flags) /= 0 .AND. &
          PGEfi(Ob,Lev) < 0.5) PGEfi(Ob,Lev) = 1.0
      IF (Obs % Rainrate(Ob,Lev) % Value == RMDI) THEN
        PGEfi(Ob,Lev) = PGEMDI
      END IF
    END DO
  END DO
  VarobPGEs % Rainrate(:,:) = REAL (INT (PGEfi(:,:) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

IF (Obs % header % SBUVozone % Present) THEN
  NumLev = Obs % header % SBUVozone % NumLev
  ALLOCATE (PGEfi(NumObsLocal,NumLev))
  IF (PRESENT (PGEBd)) THEN
    IF (PGEBd % header % SBUVOzone % Present) THEN
      WHERE (PGEBd % SBUVOzone /= RMDI)
        PGEfi(:,:) = PGEBd % SBUVOzone(:,:)
      ELSEWHERE
        PGEfi(:,:) = AINT (Obs % SBUVOzone(:,:) % PGEFinal) / PPF
      END WHERE
    ELSE
      PGEfi(:,:) = AINT (Obs % SBUVOzone(:,:) % PGEFinal) / PPF
    END IF
  ELSE
    PGEfi(:,:) = AINT (Obs % SBUVozone(:,:) % PGEFinal) / PPF
  END IF
  DO Ob = 1, NumObsLocal
    DO Lev = 1, NumLev
      IF (PGEfi(Ob,Lev) < 0.5 .AND. &
          IAND (FlagBits,Obs % SBUVozone(Ob,Lev) % Flags) /= 0) PGEfi(Ob,Lev) = 1.0
      IF (Obs % SBUVozone(Ob,Lev) % Value == RMDI) THEN
        PGEfi(Ob,Lev) = PGEMDI
      END IF
    END DO
  END DO
  VarobPGEs % SBUVozone(:,:) = REAL (INT (PGEfi(:,:) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

! HLOSwinds
IF (Obs % header % HLOSwind % Present) THEN
  ALLOCATE (PGEfi(NumObsLocal,1))
  PGEFi(:,1) = Obs % HLOSwind(:) % PGEFinal                          
  DO Ob = 1, NumObsLocal
    IF (IAND (FlagBits,Obs % HLOSwind(Ob) % Flags) /= 0 .AND. &
        PGEfi(Ob,1) < 0.5) PGEfi(Ob,1) = 1.0
    IF (Obs % HLOSwind(Ob) % Value == RMDI) PGEfi(Ob,1) = PGEMDI
  END DO
  VarobPGEs % HLOSwind(:) = REAL (INT (PGEfi(:,1) * 10000.0))
  DEALLOCATE (PGEfi)
END IF

!-------------------------------------------------------------------------------
! 2. Tidy up and exit
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_VarobPGEs
