!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Returns time in "Minutes UTC", i.e. minutes since 00:00Z 1/1/1970
! given input date in the form of yyyy/mm/dd/hh/min
!
! The time in 'minutes since 00:00Z 1/1/1970' is calculated by simple maths.
! The input year is 4 digits to avoid problems at the turn of the century.
! The code accounts for leap years, but will not work for the year 2100
! which is not a leap year.
!-------------------------------------------------------------------------------
SUBROUTINE Gen_DateToMinUTC_scalar( &
  Year,                             & ! in  - Year
  Month,                            & ! in  - Month
  Day,                              & ! in  - Day
  Hour,                             & ! in  - Hour
  Minute,                           & ! in  - Minute
  MinUTC,                           & ! out - Time in minutes UTC
  ErrStat,                          & ! out - Status code
  ShowOutput)                         ! in opt - show diagnostic messages

USE GenMod_Control, ONLY : GeneralMode, DebugMode

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  StatusOK,            &
  StatusWarning,       &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY : IMDI

IMPLICIT NONE

! Subroutine arguments

INTEGER, INTENT(IN)            :: Year
INTEGER, INTENT(IN)            :: Month
INTEGER, INTENT(IN)            :: Day
INTEGER, INTENT(IN)            :: Hour
INTEGER, INTENT(IN)            :: Minute
INTEGER, INTENT(OUT)           :: MinUTC
INTEGER, INTENT(OUT)           :: ErrStat
LOGICAL, OPTIONAL, INTENT(IN)  :: ShowOutput

! Local constants

CHARACTER (LEN=*), PARAMETER :: RoutineName="Gen_DateToMinUTC_scalar"

! Local variables

INTEGER :: Total ! Cumulative minutes counter
INTEGER :: I     ! Do loop variable
LOGICAL :: DateError
LOGICAL :: ShowOutput_Local

!---------------------------------------------------------------------
! 0. Tracing
!---------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry(RoutineName)
ErrStat = StatusOK

IF (PRESENT(ShowOutput)) THEN
  ShowOutput_Local = ShowOutput
ELSE
  ShowOutput_Local = .TRUE.
END IF

!---------------------------------------------------------------------
! 1.1. Update table for leap years
!---------------------------------------------------------------------

IF (MOD(Year,4) == 0) THEN
  MonthTab(2) = 29
ELSE
  MonthTab(2) = 28
END IF

!---------------------------------------------------------------------
! 1.2. Check date passed to routine
!---------------------------------------------------------------------

! Do as 2 if tests to avoid array bounds error on MonthTab check

IF (Month < 1 .OR. Month > 12) THEN
  DateError = .TRUE.
ELSE IF (Year   < 1970 .OR. &
         Day    < 1 .OR. Day    > MonthTab(Month) .OR. &
         Hour   < 0 .OR. Hour   > 23 .OR. &
         Minute < 0 .OR. Minute > 59) THEN
  DateError = .TRUE.
ELSE
  DateError = .FALSE.
END IF

IF (DateError) THEN
  MinUTC = IMDI
  IF (GeneralMode >= DebugMode .AND. ShowOutput_Local) THEN
    WRITE(*,'(A,A)') &
      RoutineName, " Invalid date supplied, MinUTC set to IMDI."
    WRITE(*,*) Year, Month, Day, Hour, Minute
  END IF
  ErrStat = StatusWarning
END IF

IF (ErrStat == StatusOK) THEN

  !---------------------------------------------------------------------
  ! 2. Calculate minutes from start of month.
  !---------------------------------------------------------------------

  Total = Minute + 60*Hour + MinutesInDay * (Day - 1)

  !---------------------------------------------------------------------
  ! 3. Calculate minutes from start of year.
  !---------------------------------------------------------------------

  IF (Month > 1) THEN
    DO I = 1, Month - 1
      Total = Total + MinutesInDay * MonthTab(I)
    END DO
  END IF

  !---------------------------------------------------------------------
  ! 4. Calculate non leap day minutes for years since 1970
  !---------------------------------------------------------------------

  Total = Total + MinutesInYear * (Year - 1970)

  !---------------------------------------------------------------------
  ! 5. Calculate leap day minutes from start of 1970
  !---------------------------------------------------------------------

  Total = Total + MinutesInDay * INT((Year - 1969) / 4)

  MinUTC = Total
END IF

!---------------------------------------------------------------------
! 6. Tracing
!---------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit(RoutineName)

END SUBROUTINE Gen_DateToMinUTC_scalar
