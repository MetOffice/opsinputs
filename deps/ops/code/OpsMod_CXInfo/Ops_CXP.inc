!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!------------------------------------------------------------------------------
! Calculates CX pressures, for both old and new dynamics.
!
! Method:
!     Old dynamics: uses AK and Bks
!     New dynamics: calculates Theta (B) level pressures by interpolation
!       of Rho (A) level pressures Now consistent with model calculation
!       of calc_p_at theta, where exner pressure at rho levels linearly
!       interpolated with height.
!     Ocean: copies directly from Depths in the CX header into PlevelsA
!------------------------------------------------------------------------------

SUBROUTINE Ops_CXP (CX)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  PRef,                                  &
  Kappa,                                 &
  RMDI

USE GenMod_UMHeaderConstants, ONLY: &
  FH_Submodel_Ocean

USE OpsMod_MiscTypes, ONLY: &
  ZcodeLevelsA,             &
  ZcodeLevelsB

IMPLICIT NONE

! Subroutine arguments:
TYPE (CX_Type), INTENT(INOUT) :: CX

! Local declarations:
INTEGER                       :: NumLocal
INTEGER                       :: NumCXLev
INTEGER                       :: iLev
INTEGER                       :: iob
LOGICAL                       :: AllocZA
LOGICAL                       :: AllocZB
REAL, ALLOCATABLE             :: exnerA(:,:)
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_CXP"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

NumCxLev = CX % Header % NumLevels
NumLocal = CX % Header % NumLocal

IF (CX % Header % SubModel == FH_Submodel_Ocean) THEN  ! Ocean background
  IF (ASSOCIATED (CX % Header % Depths)) THEN

    ALLOCATE (Cx % PLevelsA(NumLocal,NumCxLev))
    CX % Header % PlevelsA % NumLev = NumCxLev
    CX % Header % PlevelsA % ZCode = ZcodeLevelsA
    CX % Header % PlevelsA % Present = .TRUE.

    DO Ilev = 1, NumCxLev
      Cx % PlevelsA (:,ilev) = CX % Header % Depths(ilev)
    END DO

  END IF

ELSE IF (CX % Header % NewDynamics) THEN

  IF (ASSOCIATED (CX % Theta) .AND. ASSOCIATED (CX % PLevelsA)) THEN

    ALLOCATE (Cx % PLevelsB(NumLocal,NumCxLev))
    CX % Header % PlevelsB % NumLev = NumCxLev
    CX % Header % PlevelsB % ZCode = ZcodeLevelsB
    CX % Header % PlevelsB % Present = .TRUE.

    AllocZA = .NOT. ASSOCIATED (CX % ZA)
    AllocZB = .NOT. ASSOCIATED (CX % ZB)
    CALL Ops_CXZND (CX)

    ! Use ND UM method calculating from theta (as in Calc_P_at_theta)
    ! as adapted from UM5.2+ on. Linear interpolation of exner pressure
    ! with height from rho levels to theta levels.

    ALLOCATE (exnerA(NumLocal,NumCxLev + 1))

    DO iLev = 1, NumCxLev + 1
      DO iob = 1, NumLocal
        IF (CX % PLevelsA(iob,iLev) == RMDI) THEN
           exnerA(iob,iLev) = RMDI
        ELSE
           exnerA(iob,iLev) = (CX % PLevelsA(iob,iLev) / pref) ** kappa
        END IF
      END DO
    END DO

    DO iLev = 1, NumCxLev - 1
      DO iob = 1, NumLocal
        IF (CX % PLevelsA(iob,iLev + 1) == RMDI .OR. &
            CX % PLevelsA(iob,iLev) == RMDI) THEN
          CX % PLevelsB(iob,iLev) = RMDI
        ELSE
          CX % PLevelsB(iob,iLev) = pref * &
          ((exnerA(iob,iLev)  * (CX % ZA(iob,iLev + 1) - CX % ZB(iob,iLev)) + &
            exnerA(iob,iLev + 1)*(CX % ZB(iob,iLev) - CX % ZA(iob,iLev))) / &
            (CX % ZA(iob,iLev + 1) - CX % ZA(iob,iLev))) ** (1.0 / kappa)
        END IF
      END DO
    END DO

    iLev = NumCxLev

    DO iob = 1, NumLocal
      IF (CX % PLevelsA(iob,iLev + 1) == RMDI .OR. &
          CX % PLevelsA(iob,iLev) == RMDI) THEN
        CX % PLevelsB(iob,iLev) = RMDI
      ELSE
        CX % PLevelsB(iob,iLev) = pref * &
          (0.5 * (exnerA(iob,iLev) + exnerA(iob,iLev + 1))) ** (1.0 / kappa)
      END IF
    END DO

    DEALLOCATE (exnerA)

    IF (AllocZA .AND. ASSOCIATED (CX % ZA)) THEN
      CX % Header % ZA % Present = .FALSE.
      DEALLOCATE (CX % ZA)
    END IF
    IF (AllocZB .AND. ASSOCIATED (CX % ZB)) THEN
      CX % Header % ZB % Present = .FALSE.
      DEALLOCATE (CX % ZB)
    END IF


  ELSE IF (ASSOCIATED (CX % Theta) .AND. ASSOCIATED (CX % ExnerA)) THEN

    ALLOCATE (Cx % PLevelsA(NumLocal,NumCxLev))
    CX % Header % PlevelsA % NumLev = NumCxLev
    CX % Header % PlevelsA % ZCode = ZcodeLevelsA
    CX % Header % PlevelsA % Present = .TRUE.


    ALLOCATE (Cx % PLevelsB(NumLocal,NumCxLev))
    CX % Header % PlevelsB % NumLev = NumCxLev
    CX % Header % PlevelsB % ZCode = ZcodeLevelsB
    CX % Header % PlevelsB % Present = .TRUE.


    AllocZA = .NOT. ASSOCIATED (CX % ZA)
    AllocZB = .NOT. ASSOCIATED (CX % ZB)
    CALL Ops_CXZND (CX)


    DO iLev = 1, NumCxLev
      DO iob = 1, NumLocal
        IF (CX % ExnerA(iob,iLev) == RMDI) THEN
           CX % PLevelsA(iob,iLev) = RMDI
        ELSE
           IF ( CX % ExnerA(iob,iLev) > 0.0 )  THEN
              CX % PLevelsA(iob,iLev) = PRef * exp( (1.0 / kappa) * LOG (CX % ExnerA(iob,iLev)))
           ELSE
               CX % PLevelsA(iob,iLev) = 0.0
           END IF
        END IF
      END DO
    END DO

    DO iLev = 1, NumCxLev - 1
      DO iob = 1, NumLocal
        IF (CX % PLevelsA(iob,iLev + 1) == RMDI .OR. &
            CX % PLevelsA(iob,iLev) == RMDI) THEN
          CX % PLevelsB(iob,iLev) = RMDI
        ELSE
           IF (CX % ExnerA(iob,iLev) > 0.0)  THEN
              CX % PLevelsB(iob,iLev) = PRef * EXP ((1.0 / kappa) * &
               LOG ((CX % ExnerA(iob,iLev)  * (CX % ZA(iob,iLev + 1) - CX % ZB(iob,iLev)) + &
               CX % ExnerA(iob,iLev + 1) * (CX % ZB(iob,iLev) - CX % ZA(iob,iLev))) / &
               (CX % ZA(iob,iLev + 1) - CX % ZA(iob,iLev))))
           ELSE
              CX % PLevelsA(iob,iLev) = 0.0
           END IF
        END IF
      END DO
    END DO

    iLev = NumCxLev

    DO iob = 1, NumLocal
      IF (CX % PLevelsA(iob,iLev) == RMDI) THEN
        CX % PLevelsB(iob,iLev) = RMDI
      ELSE
        IF (CX % ExnerA(iob,iLev - 1) > 0.0)  THEN
           CX % PLevelsB(iob,iLev) = pref * (0.5 * (CX % ExnerA(iob,iLev - 1) + CX % ExnerA(iob,iLev))) ** (1.0 / kappa)
        ELSE
           CX % PLevelsB(iob,iLev) = 0.0
        END IF
      END IF
    END DO


    IF (AllocZA .AND. ASSOCIATED (CX % ZA)) THEN
      CX % Header % ZA % Present = .FALSE.
      DEALLOCATE (CX % ZA)
    END IF
    IF (AllocZB .AND. ASSOCIATED (CX % ZB)) THEN
      CX % Header % ZB % Present = .FALSE.
      DEALLOCATE (CX % ZB)
    END IF

  END IF


ELSE IF (ASSOCIATED (Cx % Pstar) .AND. ASSOCIATED (CX % Header % AK)) THEN

  ALLOCATE (Cx % PLevelsA(NumLocal,NumCxLev))
  CX % Header % PlevelsA % NumLev = NumCxLev
  CX % Header % PlevelsA % ZCode = ZcodeLevelsA
  CX % Header % PlevelsA % Present = .TRUE.

  ALLOCATE (Cx % PLevelsB(NumLocal,NumCxLev+1))
  CX % Header % PlevelsB % NumLev = NumCxLev+1
  CX % Header % PlevelsB % ZCode = ZcodeLevelsB
  CX % Header % PlevelsB % Present = .TRUE.

  DO Ilev = 1, NumCxLev
   DO iob = 1, NumLocal
     IF (Cx % orog(iob) == RMDI) THEN
       Cx % PlevelsA (IOB, Ilev) = RMDI
       Cx % PlevelsB (IOB, Ilev) = RMDI
     ELSE
       Cx % PlevelsA (IOB, Ilev) = &
        CX % Header % AK (Ilev) + (CX % Header % BK (Ilev) * Cx % pstar(IOB))
       Cx % PlevelsB (iob, Ilev) = &
        CX % Header % AKH(Ilev) + (CX % Header % BKH(Ilev) * Cx % pstar(iob))
     END IF
   END DO
  END DO
  Ilev = NumCxLev + 1  ! Cater for extra half-level
  DO iob = 1, NumLocal
     IF (Cx % orog(iob) == RMDI) THEN
       Cx % PlevelsB (IOB, Ilev) = RMDI
     ELSE
      Cx % PlevelsB (iob, Ilev) = CX % Header % AKH(Ilev) + (CX % Header % BKH(Ilev) * Cx % pstar(iob))
     END IF
  END DO
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CXP
