!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Function to convert year/month/day to an ODB format time number.
!-------------------------------------------------------------------------------

ELEMENTAL FUNCTION OpsFn_ConvertToODBTime_real (hour,   &
                                                minute, &
                                                second)

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI,                                  &
  RMDI

IMPLICIT NONE

! Function arguments:
REAL, INTENT(IN)           :: hour
REAL, INTENT(IN)           :: minute
REAL, OPTIONAL, INTENT(IN) :: second

! Function result:
INTEGER                    :: OpsFn_ConvertToODBTime_real

IF (hour /= RMDI .AND. &
    minute /= RMDI) THEN
  OpsFn_ConvertToODBTime_real = NINT (hour) * 10000 + NINT (minute) * 100
  IF (PRESENT (second)) THEN
    IF (second /= RMDI .AND. second < 60) THEN
      OpsFn_ConvertToODBTime_real = OpsFn_ConvertToODBTime_real + NINT (second)
    END IF
  END IF
ELSE
  OpsFn_ConvertToODBTime_real = IMDI
END IF

END FUNCTION OpsFn_ConvertToODBTime_real
