!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Calculates CX RH from Q, if necessary and possible
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CXRH (CX,          &
                     CapSupersat)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE OpsMod_MiscTypes, ONLY: &
    ZCodeLevelsA

USE OpsMod_ObsInfo, ONLY: &
    Ops_Alloc

USE OpsMod_Utilities, ONLY: &
    Ops_Qsat

IMPLICIT NONE

! Subroutine arguments:
TYPE (CX_Type), INTENT(INOUT) :: CX
LOGICAL, INTENT(IN)           :: CapSupersat

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_CXRH"
INTEGER                       :: NumLocal
INTEGER                       :: Lev
INTEGER                       :: Col
INTEGER                       :: NumLev
REAL, POINTER                 :: Qs(:,:) ! saturated humidity mixing ratio
REAL, POINTER                 :: Temp(:) ! temperature

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (.NOT. CX % Header % RH % Present .AND. &
    CX % Header % Q % Present .AND. &
    CX % Header % Theta % Present .AND. &
    CX % Header % PLevelsA % Present .AND. &
    CX % Header % PLevelsB % Present) THEN

  NumLocal = CX % Header % NumLocal
  NumLev = CX % header % q % NumLev
  CALL Ops_Alloc (CX % Header % RH,        &
                  "CX % RH",               &
                  NumLocal,                &
                  CX % RH,                 &
                  HdrIn = CX % Header % Q)
  IF (NumLocal > 0) THEN
    ALLOCATE (Qs(NumLocal,NumLev))
    CX % RH(:,:) = RMDI

    ! Calculate temperatures
    IF (CX % Header % RH % ZCode == ZCodeLevelsA) THEN
      CALL Ops_CXExnerB (CX)

      ALLOCATE (Temp(NumLocal))

      DO Lev = 1, NumLev

        Temp(:) = CX % Theta(:,Lev) * SQRT (CX % ExnerB(:,Lev) * CX % ExnerB(:,Lev + 1))
        ! Calculate saturated specific humidity
        CALL Ops_Qsat (Qs(:,Lev),            &
                       Temp,                 &
                       CX % PlevelsA(:,Lev), &
                       NumLocal)
      END DO
      DEALLOCATE (Temp)
    ELSE
      CALL Ops_CXT (CX)

      DO Lev = 1, NumLev
        ! Calculate saturated specific humidity
        CALL Ops_Qsat (Qs(:,Lev),            &
                       CX % T(:,Lev),        &
                       CX % PlevelsB(:,Lev), &
                       NumLocal)
      END DO

    END IF

    ! Calculate relative humidity
    DO Lev = 1, NumLev
      DO Col = 1, NumLocal
        IF (CX % Q(Col,Lev) /= RMDI) THEN
          CX % rh(Col,Lev) = 100.0 * CX % Q(Col,Lev) / Qs(Col,Lev)
          IF (CapSupersat .AND. CX % rh(Col,Lev) > 100.0) CX % rh(Col,Lev) = 100.0
        END IF
      END DO
    END DO

    DEALLOCATE (Qs)

  END IF

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CXRH
