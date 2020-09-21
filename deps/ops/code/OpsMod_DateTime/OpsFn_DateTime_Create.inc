!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Returns a specified time as a DateTime_type value.
!-------------------------------------------------------------------------------

FUNCTION OpsFn_DateTime_CreateFromVars (year,   &
                                        month,  &
                                        day,    &
                                        hour,   &
                                        minute, &
                                        second) RESULT (d)

IMPLICIT NONE

! Function arguments:
INTEGER, INTENT(IN)            :: year
INTEGER, INTENT(IN)            :: month
INTEGER, INTENT(IN)            :: day
INTEGER, OPTIONAL, INTENT(IN)  :: hour
INTEGER, OPTIONAL, INTENT(IN)  :: minute
INTEGER, OPTIONAL, INTENT(IN)  :: second

! Function result:
TYPE (DateTime_type) :: d

d % year = year
d % month = month
d % day = day
d % diff_from_utc = 0
IF (PRESENT (hour)) THEN
  d % hour = hour
ELSE
  d % hour = 0
END IF
IF (PRESENT (minute)) THEN
  d % minute = minute
ELSE
  d % minute = 0
END IF
IF (PRESENT (second)) THEN
  d % second = second
ELSE
  d % second = 0
END IF

END FUNCTION OpsFn_DateTime_CreateFromVars

FUNCTION OpsFn_DateTime_CreateFromMinUTC (min_utc) RESULT (d)

USE GenMod_Utilities, ONLY: &
  Gen_MinUTCToDate

IMPLICIT NONE

! Function arguments:
INTEGER, INTENT(IN)  :: min_utc

! Function result:
TYPE (DateTime_type) :: d

! Local declarations:
INTEGER              :: errstat

d % diff_from_utc = 0
d % second = 0

CALL Gen_MinUTCToDate (min_utc,    &
                       d % year,   &
                       d % month,  &
                       d % day,    &
                       d % hour,   &
                       d % minute, &
                       errstat)

END FUNCTION OpsFn_DateTime_CreateFromMinUTC

FUNCTION OpsFn_DateTime_FromMinUTCArray (min_utc) RESULT (d)

! Function arguments:
INTEGER, INTENT(IN)  :: min_utc(:)

! Function result:
TYPE (DateTime_type) :: d(1:SIZE (min_utc))

! Local declarations:
INTEGER              :: i

DO i = 1, SIZE (min_utc)
  d(i) = OpsFn_DateTime_CreateFromMinUTC (min_utc(i))
END DO

END FUNCTION OpsFn_DateTime_FromMinUTCArray
