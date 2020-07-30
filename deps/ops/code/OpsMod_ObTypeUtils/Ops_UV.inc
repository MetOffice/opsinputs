!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Calculates u,v from wind speed and direction.  The wind components are
! calculated using the formulaes:
!
!     u = -fa sin(db)            v = -fa cos(db)
!
! where:
!     d is the direction in degrees
!     f is the speed in knots,
!     a converts from knots to m/s
!     b converts from degrees to radians
!-------------------------------------------------------------------------------

SUBROUTINE Ops_UV (Obs)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RECIP_PI_OVER_180,                   &
    RMDI

USE OpsMod_ObsInfo, ONLY: &
    OB_type,              &
    ExtremeValueFlag,     &
    Ops_Alloc

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(INOUT) :: Obs  ! Contains data onobs

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = 'Ops_UV'
INTEGER                       :: NumObsLocal ! Number of observations on this PE
INTEGER                       :: Iobs        ! Loop variable
INTEGER                       :: Lev         ! Loop variable
REAL                          :: Rep         ! Reciprical of RECIP_PI_OVER_180
REAL                          :: Zddd        ! Wind direction
REAL                          :: Zfff        ! Wind speed

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!------------------------
! 1.   Set Initial values
!------------------------

Rep = 1.0 / RECIP_PI_OVER_180
NumObsLocal = Obs % header % NumObsLocal

!---------------------------------
! 2. Calculate 10m wind components
!---------------------------------

IF (Obs % header % ddd10 % Present .AND. &
    Obs % header % fff10 % Present) THEN
  IF (.NOT. Obs % header % u10 % Present .AND. &
      .NOT. Obs % header % v10 % Present) THEN
    CALL Ops_Alloc (Obs % header % u10,           &
                    "Obs % u10",                  &
                    NumObsLocal,                  &
                    Obs % u10,                    &
                    HdrIn = Obs % Header % ddd10)
    CALL Ops_Alloc (Obs % header % v10,           &
                    "Obs % v10",                  &
                    NumObsLocal,                  &
                    Obs % v10,                    &
                    HdrIn = Obs % Header % ddd10)
    IF (NumObsLocal > 0) THEN
      Obs % u10(:) = Obs % ddd10(:)
      Obs % u10(:) % Value = RMDI
      Obs % v10(:) = Obs % u10(:)
    END IF
  END IF

  DO Iobs = 1, NumObsLocal
    Zddd = Obs % ddd10(Iobs) % Value
    Zfff = Obs % fff10(Iobs) % Value
    IF (.NOT. BTEST (Obs % ddd10(Iobs) % Flags, ExtremeValueFlag) .AND. &
        Zddd /= RMDI .AND. Zfff /= RMDI) THEN
      Obs % u10(Iobs) % Value = -Zfff * SIN (Zddd * Rep)
      Obs % v10(Iobs) % Value = -Zfff * COS (Zddd * Rep)
    END IF
  END DO

END IF

!---------------------------------------
! 3. Calculate upper air wind components
!---------------------------------------

IF (Obs % Header % ddd % Present .AND. &
    Obs % Header % fff % Present) THEN

  IF (.NOT. Obs % header % u % Present .AND. &
      .NOT. Obs % header % v % Present) THEN
    CALL Ops_Alloc (Obs % header % u,           &
                    "Obs % u",                  &
                    NumObsLocal,                &
                    Obs % u,                    &
                    HdrIn = Obs % Header % ddd)
    CALL Ops_Alloc (Obs % header % v,           &
                    "Obs % v",                  &
                    NumObsLocal,                &
                    Obs % v,                    &
                    HdrIn = Obs % Header % ddd)
    Obs % u(:,:) = Obs % ddd(:,:)
    Obs % u(:,:) % Value = RMDI
    Obs % v(:,:) = Obs % u(:,:)
  END IF
  DO Iobs = 1, NumObsLocal
    DO Lev = 1, Obs % header % ddd % NumLev
      Zddd = Obs % ddd(Iobs,Lev) % Value
      Zfff = Obs % fff(Iobs,Lev) % Value
      IF (.NOT. BTEST (Obs % ddd(Iobs,Lev) % Flags, ExtremeValueFlag) .AND. &
          Zddd /= RMDI .AND. Zfff /= RMDI) THEN
        Obs % u(Iobs,Lev) % Value = -Zfff * SIN (Zddd * Rep)
        Obs % v(Iobs,Lev) % Value = -Zfff * COS (Zddd * Rep)
      END IF
    END DO
  END DO

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_UV
