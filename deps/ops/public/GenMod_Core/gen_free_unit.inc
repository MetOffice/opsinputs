!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Frees a fortran or C I/O unit number previously assigned by Gen_GetUnit.
!-------------------------------------------------------------------------------

SUBROUTINE gen_free_unit (unit,                       &
                          CUnit,                      &
                          status_if_free_unit_failed)

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(INOUT)        :: unit        ! Unit number supplied
LOGICAL, OPTIONAL, INTENT(IN) :: CUnit
INTEGER, OPTIONAL, INTENT(IN) :: status_if_free_unit_failed

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "gen_free_unit"
LOGICAL                       :: Local_CUnit
INTEGER                       :: local_status_if_free_unit_failed

IF (PRESENT (CUnit)) THEN
  Local_CUnit = CUnit
ELSE
  Local_CUnit = .FALSE.
END IF

IF (PRESENT (status_if_free_unit_failed)) THEN
  local_status_if_free_unit_failed = status_if_free_unit_failed
ELSE
  local_status_if_free_unit_failed = StatusFatal
END IF

! Get a unit for Fortran IO
IF (.NOT. Local_CUnit) THEN
  IF (unit >= LB .AND. unit < UB) THEN
    UnitsInUse(unit) = .FALSE.
    unit = 0
  ELSE
    SELECT CASE (local_status_if_free_unit_failed)
      CASE (StatusOK)
        CALL gen_message (RoutineName,                                             &
                          'Attempt to free an illegal unit number for fortran IO')
      CASE (StatusWarning)
        CALL gen_warn (RoutineName,                                             &
                       'Attempt to free an illegal unit number for fortran IO')
      CASE DEFAULT
        CALL gen_fail (RoutineName,                                             &
                       'Attempt to free an illegal unit number for fortran IO')
    END SELECT
  END IF

! Get a unit for C IO
ELSE

  IF (unit >= UB + 1 .AND. unit < MAX_UNITS) THEN
    UnitsInUse(unit) = .FALSE.
    unit = 0
  ELSE
    SELECT CASE (local_status_if_free_unit_failed)
      CASE (StatusOK)
        CALL gen_message (RoutineName,                                       &
                          'Attempt to free an illegal unit number for C IO')
      CASE (StatusWarning)
        CALL gen_warn (RoutineName,                                       &
                       'Attempt to free an illegal unit number for C IO')
      CASE DEFAULT
        CALL gen_fail (RoutineName,                                       &
                       'Attempt to free an illegal unit number for C IO')
    END SELECT
  END IF
END IF

END SUBROUTINE gen_free_unit
