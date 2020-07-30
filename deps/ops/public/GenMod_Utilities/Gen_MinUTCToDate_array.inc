!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Calculates date in broken form given date in the form of minutes
! since 01/01/1970 00:00Z (Universal co-ordinated time)
!
! Breaks down the time in minutes UTC to year/month/day/hour/minute.
! Acconts for all leap years up to the year 2100.
!-------------------------------------------------------------------------------

SUBROUTINE Gen_MinUTCToDate_array (MinUTC,     &
                                   Year,       &
                                   Month,      &
                                   Day,        &
                                   Hour,       &
                                   Minute,     &
                                   ErrStat,    &
                                   ShowOutput)

USE GenMod_Control, ONLY: &
  GeneralMode,            &
  DebugMode

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  gen_warn,            &
  MessageOut,          &
  StatusWarning,       &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)           :: MinUTC(:)
INTEGER, INTENT(OUT)          :: Year(:)
INTEGER, INTENT(OUT)          :: Month(:)
INTEGER, INTENT(OUT)          :: Day(:)
INTEGER, INTENT(OUT)          :: Hour(:)
INTEGER, INTENT(OUT)          :: Minute(:)
INTEGER, INTENT(OUT)          :: ErrStat
LOGICAL, OPTIONAL, INTENT(IN) :: ShowOutput

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "Gen_MinUTCToDate_array"
INTEGER                       :: ITEMP
INTEGER                       :: I
INTEGER                       :: Loop
INTEGER                       :: NumDateErrors
CHARACTER(len=80)             :: ErrorMessage
LOGICAL                       :: ShowOutput_Local

!---------------------------------------------------------------------
! 0. Tracing and Initialisation
!---------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (PRESENT (ShowOutput)) THEN
  ShowOutput_Local = ShowOutput
ELSE
  ShowOutput_Local = .TRUE.
END IF

NumDateErrors = 0

reports: DO Loop = 1, SIZE (MinUTC)

  !---------------------------------------------------------------------
  ! 1. Check input
  !---------------------------------------------------------------------

  IF (MinUTC(Loop) < 0) THEN
    Year(Loop) = IMDI
    Month(Loop) = IMDI
    Day(Loop) = IMDI
    Hour(Loop) = IMDI
    Minute(Loop) = IMDI
    NumDateErrors = NumDateErrors + 1
    IF (GeneralMode >= DebugMode .AND. ShowOutput_Local) THEN
      WRITE (MessageOut, '(2A,I6,I17,A)') RoutineName, " Invalid date supplied: ", &
        Loop, MinUTC(Loop), " Output set to IMDI"
    END IF
    ErrStat = StatusWarning
    CYCLE   ! Move to next ob
  END IF

  !---------------------------------------------------------------------
  ! 1. Extract hours and minutes
  !---------------------------------------------------------------------

  Minute(Loop) = MOD (MinUTC(Loop), 60)
  Hour(Loop) = INT (MOD (MinUTC(Loop), MinutesInDay) / 60)

  !---------------------------------------------------------------------
  ! 1. Calculate days
  !---------------------------------------------------------------------

  ITEMP = INT (MinUTC(Loop) / MinutesInday)

  !---------------------------------------------------------------------
  ! 1. Calculate 4 year chunks from 1970 (1461=3*365+366)
  !---------------------------------------------------------------------

  Year(Loop) = 1970 + 4 * INT (ITEMP / 1461)
  ITEMP = MOD (ITEMP, 1461)

  !---------------------------------------------------------------------
  ! 1. Now take last chunk of (up to) 3 years
  !---------------------------------------------------------------------

  IF (ITEMP > 364) THEN
    Year(Loop) = Year(Loop) + 1
    ITEMP = ITEMP - 365
  END IF
  IF (ITEMP > 364) THEN
    Year(Loop) = Year(Loop) + 1
    ITEMP = ITEMP - 365
  END IF
  IF (ITEMP > 365) THEN
    Year(Loop) = Year(Loop) + 1
    ITEMP = ITEMP - 366
  END IF

  !---------------------------------------------------------------------
  ! 1. ITEMP IS NOW YEAR DAY (0 TO 364/365)
  !---------------------------------------------------------------------

  ITEMP = ITEMP + 1

  !---------------------------------------------------------------------
  ! ITEMP IS NOW YEAR DAY (1 TO 365/366). IS YEAR A LEAP YEAR ?
  !---------------------------------------------------------------------

  IF (MOD (Year(Loop), 4) == 0) THEN
    MonthTab(2) = 29
  ELSE
    MonthTab(2) = 28
  END IF

  !---------------------------------------------------------------------
  ! FIND DAY AND MONTH
  !---------------------------------------------------------------------

  Months: DO I = 1,12
    IF (ITEMP >= 1 .AND. ITEMP <= MonthTab(I)) THEN
      Day(Loop) = ITEMP
      Month(Loop) = I
    END IF
    ITEMP = ITEMP - MonthTab(I)
  END DO Months

END DO reports

!---------------------------------------------------------------------
! Tracing
!---------------------------------------------------------------------

ErrorMessage = ""
IF (NumDateErrors > 0 .AND. ShowOutput_Local) THEN
  WRITE (ErrorMessage, '(I0,A)') NumDateErrors, ' obs had invalid date '
  ErrStat = StatusWarning
  CALL gen_warn (RoutineName,  &
                 ErrorMessage)
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Gen_MinUTCToDate_array
