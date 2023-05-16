!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Assigns an array of real data to a specified ODB column.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetODBVal_array_real64 (session,     &
                                       table_name,  &
                                       var,         &
                                       data,        &
                                       values,      &
                                       mask,        &
                                       unpack_mask)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  RMDI

USE ops_odb_wrappers, ONLY: &
  ops_odb_varindex

USE OpsMod_Kinds, ONLY: &
  real64

USE OpsMod_ODBKinds, ONLY: &
  odb_real

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
CHARACTER(len=*), INTENT(IN)       :: table_name
CHARACTER(len=*), INTENT(IN)       :: var
REAL(kind=odb_real), INTENT(INOUT) :: data(:,0:)
REAL(kind=real64), INTENT(IN)      :: values(:)
LOGICAL, OPTIONAL, INTENT(IN)      :: mask(:)
LOGICAL, OPTIONAL, INTENT(IN)      :: unpack_mask(:)

! Local declarations:
INTEGER                            :: idx
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_SetODBVal_array_real64"
INTEGER                            :: i
INTEGER                            :: j

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL ops_odb_varindex (session,    &
                       table_name, &
                       var,        &
                       idx)

IF (PRESENT (mask)) THEN
  WHERE (mask .AND. values /= RMDI)
    data(:,idx) = values(:)
  END WHERE
ELSE IF (PRESENT (unpack_mask)) THEN
  j = 1
  DO i = 1, SIZE (unpack_mask)
    IF (unpack_mask(i)) THEN
      IF (values(j) /= RMDI) THEN
        data(i,idx) = values(j)
      END IF
      j = j + 1
    END IF
  END DO
ELSE
  WHERE (values /= RMDI)
    data(:,idx) = values(:)
  END WHERE
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetODBVal_array_real64
