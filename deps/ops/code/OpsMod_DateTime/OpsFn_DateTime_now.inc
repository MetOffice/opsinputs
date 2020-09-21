!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Returns time now as a DateTime_type value.
!-------------------------------------------------------------------------------

FUNCTION OpsFn_DateTime_now () RESULT (now)

IMPLICIT NONE

! Function result:
TYPE (DateTime_type) :: now

! Local declarations:
INTEGER              :: values(8)

CALL DATE_AND_TIME (values = values)

now % year = values(1)
now % month = values(2)
now % day = values(3)
now % diff_from_utc = values(4)
now % hour = values(5)
now % minute = values(6)
now % second = values(7)

END FUNCTION OpsFn_DateTime_now
