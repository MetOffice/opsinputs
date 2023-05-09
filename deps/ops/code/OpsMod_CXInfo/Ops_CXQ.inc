!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Calculates CX Q from RH, if necessary and possible.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CXQ (CX,  &
                    alt)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE OpsMod_MiscTypes, ONLY: &
    ZCodeLevelsA

USE OpsMod_ObsInfo, ONLY: &
    Ops_Alloc,            &
    Ops_DeAlloc

USE OpsMod_Utilities, ONLY: &
    Ops_Qsat

IMPLICIT NONE

! Subroutine arguments:
TYPE (CX_Type), INTENT(INOUT) :: CX
LOGICAL, OPTIONAL, INTENT(IN) :: Alt ! Method for calculating pressure at each level

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_CXQ"
INTEGER                       :: NumLocal
INTEGER                       :: Lev
INTEGER                       :: NumLev
REAL, POINTER                 :: Qs(:,:)  ! saturated humidity mixing ratio
REAL, POINTER                 :: Temp(:)  ! temperature
LOGICAL                       :: AllocExnerB
LOGICAL                       :: AllocT

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (.NOT. ASSOCIATED(CX % q) .AND. &
    ASSOCIATED (CX % RH) .AND. &
    ASSOCIATED (CX % Theta) .AND. &
    ASSOCIATED (CX % PLevelsA) .AND. &
    ASSOCIATED (CX % PLevelsB)) THEN

  NumLocal = CX % Header % NumLocal
  NumLev = CX % header % rh % NumLev
  CALL Ops_Alloc (CX % Header % Q,          &
                  "CX % Q",                 &
                  NumLocal,                 &
                  CX % Q,                   &
                  HdrIn = CX % Header % RH)

  IF (NumLocal > 0) THEN

    ALLOCATE (Qs(NumLocal,NumLev))
    CX % q(:,:) = RMDI

    ! Calculate temperatures
    IF (CX % Header % RH % ZCode == ZCodeLevelsA) THEN
      AllocExnerB = .NOT. ASSOCIATED (CX % ExnerB)
      CALL Ops_CXExnerB (CX)

      ALLOCATE (Temp(NumLocal))

      DO Lev = 1, NumLev

        IF (PRESENT (Alt)) THEN
          Temp(:) = CX % Theta(:,Lev) * &
            (CX % ExnerB(:,Lev + 1) - CX % ExnerB(:,Lev)) / &
            (LOG (CX % ExnerB(:,Lev + 1)) - LOG (CX % ExnerB(:,Lev)))
        ELSE
          Temp(:) = CX % Theta(:,Lev) * SQRT (CX % ExnerB(:,Lev) * CX % ExnerB(:,Lev + 1))
        END IF

        ! Calculate saturated specific humidity
        CALL Ops_Qsat (Qs(:,Lev),            & ! OUT
                       Temp,                 & ! IN
                       CX % PlevelsA(:,Lev), & ! IN
                       NumLocal)               ! IN
      END DO

      DEALLOCATE (Temp)
      IF (AllocExnerB .AND. ASSOCIATED (CX % ExnerB)) THEN
        CALL Ops_DeAlloc (CX % Header % ExnerB, &
                          "CX % ExnerB",        &
                          CX % ExnerB)
      END IF
    ELSE
      AllocT = .NOT. ASSOCIATED (CX % T)
      CALL Ops_CXT (CX)

      DO Lev = 1, NumLev
        ! Calculate saturated specific humidity
        CALL Ops_Qsat (Qs(:,Lev),            & ! OUT
                       CX % T,               & ! IN
                       CX % PlevelsB(:,Lev), & ! IN
                       NumLocal)               ! IN
      END DO

      IF (AllocT .AND. ASSOCIATED (CX % T)) THEN
        CALL Ops_DeAlloc (CX % Header % T, &
                          "CX % T",        &
                          CX % T)
      END IF
    END IF

    CX % q(:,:) = 0.0

    WHERE (CX % rh(:,:) /= RMDI)
      CX % q(:,:) = CX % rh(:,:) * Qs(:,:) * 0.01
    END WHERE

    DEALLOCATE (Qs)
  END IF

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CXQ
