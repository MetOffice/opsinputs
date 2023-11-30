!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Function to convert year/month/day to an ODB format date number.
!-------------------------------------------------------------------------------

ELEMENTAL FUNCTION OpsFn_ConvertToODBDate_real (year,  &
                                                month, &
                                                day)

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI,                                  &
  RMDI

IMPLICIT NONE

! Function arguments:
REAL, INTENT(IN) :: year
REAL, INTENT(IN) :: month
REAL, INTENT(IN) :: day

! Function result:
INTEGER          :: OpsFn_ConvertToODBDate_real

IF (year /= RMDI .AND. &
    month /= RMDI .AND. &
    day /= RMDI) THEN
  OpsFn_ConvertToODBDate_real = NINT (year) * 10000 + NINT (month) * 100 + NINT (day)
ELSE
  OpsFn_ConvertToODBDate_real = IMDI
END IF

END FUNCTION OpsFn_ConvertToODBDate_real
