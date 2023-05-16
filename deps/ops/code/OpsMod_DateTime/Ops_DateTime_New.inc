!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Constructs a new DateTime_type.
!-------------------------------------------------------------------------------

FUNCTION Ops_DateTime_New (year,          &
                           month,         &
                           day,           &
                           hour,          &
                           minute,        &
                           second,        &
                           diff_from_utc) RESULT (dt)

IMPLICIT NONE

! Function arguments:
INTEGER, INTENT(IN)           :: year
INTEGER, INTENT(IN)           :: month
INTEGER, INTENT(IN)           :: day
INTEGER, INTENT(IN)           :: hour
INTEGER, INTENT(IN)           :: minute
INTEGER, OPTIONAL, INTENT(IN) :: second
INTEGER, OPTIONAL, INTENT(IN) :: diff_from_utc

! Function result:
TYPE (DateTime_type) :: dt

dt % year = year
dt % month = month
dt % day = day
dt % hour = hour
dt % minute = minute
IF (PRESENT (second)) THEN
  dt % second = second
ELSE
  dt % second = 0
END IF
IF (PRESENT (diff_from_utc)) THEN
  dt % diff_from_utc = diff_from_utc
ELSE
  dt % diff_from_utc = 0
END IF

END FUNCTION Ops_DateTime_New
