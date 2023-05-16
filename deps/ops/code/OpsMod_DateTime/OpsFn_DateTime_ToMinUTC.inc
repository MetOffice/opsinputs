!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Returns time in "Minutes UTC", i.e. minutes since 00:00Z 1/1/1970
! given input date in the form of yyyy/mm/dd/hh/min
!
! The time in 'minutes since 00:00Z 1/1/1970' is calculated by simple maths.
! The input year is 4 digits to avoid problems at the turn of the century.
! The code accounts for leap years, but will not work for the year 2100
! which is not a leap year.
!-------------------------------------------------------------------------------

FUNCTION OpsFn_DateTime_ToMinUTCDateTime (date_time) RESULT (min_utc)

IMPLICIT NONE

! Function arguments:
TYPE (DateTime_type), INTENT(IN) :: date_time

! Function result:
INTEGER                          :: min_utc

min_utc = OpsFn_DateTime_ToMinUTCVars (date_time % year, date_time % month, date_time % day, &
                                       date_time % hour, date_time % minute)

END FUNCTION OpsFn_DateTime_ToMinUTCDateTime

FUNCTION OpsFn_DateTime_ToMinUTCArray (date_time_array) RESULT (min_utc)

IMPLICIT NONE

! Function arguments:
INTEGER, INTENT(IN) :: date_time_array(:)

! Function result:
INTEGER             :: min_utc

IF (SIZE (date_time_array) >= 5) THEN
  min_utc = OpsFn_DateTime_ToMinUTCVars (date_time_array(1), date_time_array(2), date_time_array(3), &
                                         date_time_array(4), date_time_array(5))
ELSE IF (SIZE (date_time_array) == 4) THEN
  min_utc = OpsFn_DateTime_ToMinUTCVars (date_time_array(1), date_time_array(2), date_time_array(3), &
                                         date_time_array(4))
ELSE
  min_utc = OpsFn_DateTime_ToMinUTCVars (date_time_array(1), date_time_array(2), date_time_array(3))
END IF

END FUNCTION OpsFn_DateTime_ToMinUTCArray

FUNCTION OpsFn_DateTime_ToMinUTCVarsArray (years, months, days, hours, minutes) RESULT (min_utc)

IMPLICIT NONE

! Function arguments:
INTEGER, INTENT(IN)           :: years(:)
INTEGER, INTENT(IN)           :: months(:)
INTEGER, INTENT(IN)           :: days(:)
INTEGER, OPTIONAL, INTENT(IN) :: hours(:)
INTEGER, OPTIONAL, INTENT(IN) :: minutes(:)

! Function result:
INTEGER                       :: min_utc(SIZE (years))

! Local declarations:
INTEGER                       :: i

DO i = 1, SIZE (years)
  IF (PRESENT (minutes)) THEN
    min_utc(i) = OpsFn_DateTime_ToMinUTCVars (years(i), months(i), days(i), hours(i), minutes(i))
  ELSE IF (PRESENT (hours)) THEN
    min_utc(i) = OpsFn_DateTime_ToMinUTCVars (years(i), months(i), days(i), hours(i))
  ELSE
    min_utc(i) = OpsFn_DateTime_ToMinUTCVars (years(i), months(i), days(i))
  END IF
END DO

END FUNCTION OpsFn_DateTime_ToMinUTCVarsArray

FUNCTION OpsFn_DateTime_ToMinUTCVars (year, month, day, hour, minute) RESULT (min_utc)

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

IMPLICIT NONE

! Function arguments:
INTEGER, INTENT(IN)           :: year
INTEGER, INTENT(IN)           :: month
INTEGER, INTENT(IN)           :: day
INTEGER, OPTIONAL, INTENT(IN) :: hour
INTEGER, OPTIONAL, INTENT(IN) :: minute

! Function result:
INTEGER                       :: min_utc

! Local declarations:
INTEGER                       :: local_month_table(12)
LOGICAL                       :: date_error
INTEGER                       :: i
INTEGER                       :: local_minute
INTEGER                       :: local_hour

min_utc = IMDI

!---------------------------------------------------------------------
! 1.1. Update table for leap years
!---------------------------------------------------------------------

local_month_table = month_table

IF (MOD (year, 4) == 0) THEN
  local_month_table(2) = 29
END IF

IF (PRESENT (hour)) THEN
  local_hour = hour
ELSE
  local_hour = 0
END IF

IF (PRESENT (minute)) THEN
  local_minute = minute
ELSE
  local_minute = 0
END IF

!---------------------------------------------------------------------
! 1.2. Check date passed to routine
!---------------------------------------------------------------------

! Do as 2 if tests to avoid array bounds error on MonthTab check

IF (month < 1 .OR. month > 12) THEN
  date_error = .TRUE.
ELSE IF (year < 1970 .OR. &
         day < 1 .OR. day > local_month_table(month) .OR. &
         local_hour < 0 .OR. local_hour > 23 .OR. &
         local_minute < 0 .OR. local_minute > 59) THEN
  date_error = .TRUE.
ELSE
  date_error = .FALSE.
END IF

IF (.NOT. date_error) THEN

  !---------------------------------------------------------------------
  ! 2. Calculate minutes from start of month.
  !---------------------------------------------------------------------

  min_utc = local_minute + 60 * local_hour + minutes_in_day * (Day - 1)

  !---------------------------------------------------------------------
  ! 3. Calculate minutes from start of year.
  !---------------------------------------------------------------------

  IF (month > 1) THEN
    DO i = 1, month - 1
      min_utc = min_utc + minutes_in_day * local_month_table(i)
    END DO
  END IF

  !---------------------------------------------------------------------
  ! 4. Calculate non leap day minutes for years since 1970
  !---------------------------------------------------------------------

  min_utc = min_utc + minutes_in_year * (year - 1970)

  !---------------------------------------------------------------------
  ! 5. Calculate leap day minutes from start of 1970
  !---------------------------------------------------------------------

  min_utc = min_utc + minutes_in_day * INT ((year - 1969) / 4)

END IF

END FUNCTION OpsFn_DateTime_ToMinUTCVars
