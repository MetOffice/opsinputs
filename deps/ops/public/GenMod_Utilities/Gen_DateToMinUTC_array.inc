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

SUBROUTINE Gen_DateToMinUTC_array (Year,       &
                                   Month,      &
                                   Day,        &
                                   Hour,       &
                                   Minute,     &
                                   MinUTC,     &
                                   ErrStat,    &
                                   id,         &
                                   ShowOutput)

USE GenMod_Control, ONLY: &
  GeneralMode,            &
  DebugMode,              &
  nproc

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  gen_warn,            &
  MessageOut,          &
  StatusOK,            &
  StatusWarning,       &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)           :: Year(:)
INTEGER, INTENT(IN)           :: Month(:)
INTEGER, INTENT(IN)           :: Day(:)
INTEGER, INTENT(IN)           :: Hour(:)
INTEGER, INTENT(IN)           :: Minute(:)
INTEGER, INTENT(OUT)          :: MinUTC(:)
INTEGER, INTENT(OUT)          :: ErrStat
INTEGER, INTENT(IN), OPTIONAL :: id(:)
LOGICAL, INTENT(IN), OPTIONAL :: ShowOutput

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "Gen_DateToMinUTC_array"
INTEGER                       :: Total          ! Cumulative minutes counter
INTEGER                       :: I              ! Do loop variable
INTEGER                       :: Loop           ! Do loop variable
INTEGER                       :: NumDateErrors
LOGICAL                       :: DateError
LOGICAL                       :: ShowOutput_Local
CHARACTER(len=80)             :: ErrorMessage

!---------------------------------------------------------------------
! 0. Tracing and Initialisation
!---------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (PRESENT (ShowOutput)) THEN
  ShowOutput_Local = ShowOutput
ELSE
  ShowOutput_Local = .FALSE.
END IF

NumDateErrors = 0

DO Loop = 1, SIZE (MinUTC)

  ErrStat = StatusOK

  !---------------------------------------------------------------------
  ! 1.1. Update table for leap years
  !---------------------------------------------------------------------

  IF (MOD (Year(Loop), 4) == 0) THEN
    MonthTab(2) = 29
  ELSE
    MonthTab(2) = 28
  END IF

  !---------------------------------------------------------------------
  ! 1.2. Check date passed to routine
  !---------------------------------------------------------------------

  ! Do as 2 if tests to avoid array bounds error on MonthTab check

  IF (Month(Loop)  < 1 .OR. Month(Loop)  > 12) THEN
    DateError = .TRUE.
  ELSE IF (Year(Loop) < 1970 .OR. &
           Day(Loop) < 1 .OR. &
           Day(Loop) > MonthTab(Month(Loop)) .OR. &
           Hour(Loop) < 0 .OR. &
           Hour(Loop) > 23 .OR. &
           Minute(Loop) < 0 .OR. &
           Minute(Loop) > 59) THEN
    DateError = .TRUE.
  ELSE
    DateError = .FALSE.
  END IF

  IF (DateError) THEN
    NumDateErrors = NumDateErrors + 1
    MinUTC(Loop) = IMDI
    IF (GeneralMode >= DebugMode .AND. ShowOutput_Local) THEN
      IF (PRESENT (id)) THEN
        WRITE (MessageOut, '(2A,I6)') RoutineName, &
          " Invalid date supplied, MinUTC set to IMDI for ob", id(Loop)
      ELSE
        WRITE (MessageOut, '(2A,I6)') RoutineName, &
          " Invalid date supplied, MinUTC set to IMDI for ob", Loop
      END IF
      WRITE (MessageOut, '(5A)') Year(Loop), Month(Loop), Day(Loop), Hour(Loop), Minute(Loop)
    END IF
    ErrStat = StatusWarning
    CYCLE   ! Move to next ob
  END IF

  IF (ErrStat == StatusOK) THEN

    !---------------------------------------------------------------------
    ! 2. Calculate minutes from start of month.
    !---------------------------------------------------------------------

    Total = Minute(Loop) + 60 * Hour(Loop) + MinutesInDay * (Day(Loop) - 1)

    !---------------------------------------------------------------------
    ! 3. Calculate minutes from start of year.
    !---------------------------------------------------------------------

    IF (Month(Loop) > 1) THEN
      DO I = 1, Month(Loop) - 1
        Total = Total + MinutesInDay * MonthTab(I)
      END DO
    END IF

    !---------------------------------------------------------------------
    ! 4. Calculate non leap day minutes for years since 1970
    !---------------------------------------------------------------------

    Total = Total + MinutesInYear * (Year(Loop) - 1970)

    !---------------------------------------------------------------------
    ! 5. Calculate leap day minutes from start of 1970
    !---------------------------------------------------------------------

    Total = Total + MinutesInDay * INT ((Year(Loop) - 1969) / 4)

    MinUTC(Loop) = Total
  END IF
END DO

IF (nproc == 1 .AND. NumDateErrors > 0 .AND. ShowOutput_Local) THEN
  ! Cannot do this MPP with an ISUM as routine only called if
  ! NumObsLocal > 0
  WRITE (ErrorMessage, '(I0,A)') NumDateErrors, ' obs had invalid date '
  ErrStat = StatusWarning
  CALL gen_warn (RoutineName, &
                 ErrorMessage)
END IF

!---------------------------------------------------------------------
! 6. Tracing
!---------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Gen_DateToMinUTC_array
