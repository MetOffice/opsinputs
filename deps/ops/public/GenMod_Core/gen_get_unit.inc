!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Supplies an unused fortran or C I/O unit number.
!
! The module GenMod_FortranIO keeps track of which unit numbers have been
! assigned. If a unit number cant be supplied then gen_warn is called
! with a warning.
!-------------------------------------------------------------------------------

SUBROUTINE gen_get_unit (unit,                      &
                         CUnit,                     &
                         status_if_get_unit_failed)

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(OUT)          :: unit        ! Unit number supplied
LOGICAL, OPTIONAL, INTENT(IN) :: CUnit
INTEGER, OPTIONAL, INTENT(IN) :: status_if_get_unit_failed

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "gen_get_unit"
LOGICAL                       :: Local_CUnit
INTEGER                       :: local_status_if_get_unit_failed

IF (PRESENT (CUnit)) THEN
  Local_CUnit = CUnit
ELSE
  Local_CUnit = .FALSE.
END IF

IF (PRESENT (status_if_get_unit_failed)) THEN
  local_status_if_get_unit_failed = status_if_get_unit_failed
ELSE
  local_status_if_get_unit_failed = StatusFatal
END IF

! Get a unit for Fortran IO
IF (.NOT. Local_CUnit) THEN
  unit = LB

  DO WHILE (UnitsInUse(unit) .AND. unit < UB)
    unit = unit + 1
  END DO

  IF (unit == UB) THEN
    SELECT CASE (local_status_if_get_unit_failed)
      CASE (StatusOK)
        CALL gen_message (RoutineName,                                       &
                          'No unused unit numbers available for fortran IO')
      CASE (StatusWarning)
        CALL gen_warn (RoutineName,                                       &
                       'No unused unit numbers available for fortran IO')
      CASE DEFAULT
        CALL gen_fail (RoutineName,                                       &
                       'No unused unit numbers available for fortran IO')
    END SELECT
    unit = 0
  ELSE
    UnitsInUse(unit) = .TRUE.
  END IF

! Get a unit for C IO
ELSE
  unit = UB + 1

  DO WHILE (UnitsInUse(unit) .AND. unit < MAX_UNITS)
    unit = unit + 1
  END DO

  IF (unit == MAX_UNITS) THEN
    SELECT CASE (local_status_if_get_unit_failed)
      CASE (StatusOK)
        CALL gen_message (RoutineName,                                 &
                          'No unused unit numbers available for C IO')
      CASE (StatusWarning)
        CALL gen_warn (RoutineName,                                 &
                       'No unused unit numbers available for C IO')
      CASE DEFAULT
        CALL gen_fail (RoutineName,                                 &
                       'No unused unit numbers available for C IO')
    END SELECT
    unit = 0
  ELSE
    UnitsInUse(unit) = .TRUE.
  END IF
END IF

END SUBROUTINE gen_get_unit
