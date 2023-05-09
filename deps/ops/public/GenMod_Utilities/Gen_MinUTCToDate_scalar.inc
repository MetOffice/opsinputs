!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Calculates date in broken form given date in the form of minutes
! since 01/01/1970 00:00Z (Universal co-ordinated time)
!
! Breaks down the time in minutes UTC to year/month/day/hour/minute.
! Acconts for all leap years up to the year 2100.
!-------------------------------------------------------------------------------

SUBROUTINE Gen_MinUTCToDate_scalar (MinUTC,     & ! time in minutes UTC
                                    Year,       & ! year
                                    Month,      & ! month
                                    Day,        & ! day
                                    Hour,       & ! hour
                                    Minute,     & ! minute
                                    ErrStat,    & ! status code
                                    ShowOutput)   ! show diagnostic messages

USE GenMod_Control, ONLY: &
  GeneralMode,            &
  DebugMode

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  MessageOut,          &
  StatusOK,            &
  StatusWarning,       &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)           :: MinUTC
INTEGER, INTENT(OUT)          :: Year
INTEGER, INTENT(OUT)          :: Month
INTEGER, INTENT(OUT)          :: Day
INTEGER, INTENT(OUT)          :: Hour
INTEGER, INTENT(OUT)          :: Minute
INTEGER, INTENT(OUT)          :: ErrStat
LOGICAL, OPTIONAL, INTENT(IN) :: ShowOutput

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "Gen_MinUTCToDate_scalar"
INTEGER                       :: itemp
INTEGER                       :: i
LOGICAL                       :: ShowOutput_Local

!---------------------------------------------------------------------
! 0. Tracing and Initialisation
!---------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

ErrStat = StatusOK

IF (PRESENT (ShowOutput)) THEN
  ShowOutput_Local = ShowOutput
ELSE
  ShowOutput_Local = .TRUE.
END IF

!---------------------------------------------------------------------
! 1. Check input
!---------------------------------------------------------------------

IF (MinUTC < 0) THEN
  Year = IMDI
  Month = IMDI
  Day = IMDI
  Hour = IMDI
  Minute = IMDI
  IF (GeneralMode >= DebugMode .AND. ShowOutput_Local) THEN
    WRITE (MessageOut, '(2A,I0,A)') &
      RoutineName, " Invalid date supplied: ", MinUTC, " Output set to IMDI"
  END IF
  ErrStat = StatusWarning
  GOTO 9999
END IF

!---------------------------------------------------------------------
! 1. Extract hours and minutes
!---------------------------------------------------------------------

Minute = MOD (MinUTC, 60)
Hour   = INT (MOD (MinUTC, MinutesInDay) / 60)

!---------------------------------------------------------------------
! 1. Calculate days
!---------------------------------------------------------------------

itemp = INT (MinUTC / MinutesInday)

!---------------------------------------------------------------------
! 1. Calculate 4 year chunks from 1970 (1461=3*365+366)
!---------------------------------------------------------------------

Year = 1970 + 4 * INT (itemp / 1461)
itemp = MOD (itemp, 1461)

!---------------------------------------------------------------------
! 1. Now take last chunk of (up to) 3 years
!---------------------------------------------------------------------

IF (itemp > 364) THEN
  Year = Year + 1
  itemp = itemp - 365
END IF
IF (itemp > 364) THEN
  Year = Year + 1
  itemp = itemp - 365
END IF
IF (itemp > 365) THEN
  Year = Year + 1
  itemp = itemp - 366
END IF

!---------------------------------------------------------------------
! 1. itemp is now year day (0 to 364/365)
!---------------------------------------------------------------------

itemp = itemp + 1

!---------------------------------------------------------------------
! itemp is now year day (1 to 365/366).  Is year a leap year?
!---------------------------------------------------------------------

IF (MOD (Year, 4) == 0) THEN
  MonthTab(2) = 29
ELSE
  MonthTab(2) = 28
END IF

!---------------------------------------------------------------------
! Find day and month
!---------------------------------------------------------------------

Months: DO i = 1,12
  IF (itemp >= 1 .AND. itemp <= MonthTab(i)) THEN
    Day = itemp
    Month = i
  END IF
  itemp = itemp - MonthTab(i)
END DO Months

9999 CONTINUE

!---------------------------------------------------------------------
! Tracing
!---------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Gen_MinUTCToDate_scalar
