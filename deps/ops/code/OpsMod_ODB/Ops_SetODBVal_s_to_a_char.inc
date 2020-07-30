!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Sets a column of an ODB table to a scalar character value.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetODBVal_s_to_a_char (session,     &
                                      table_name,  &
                                      var,         &
                                      data,        &
                                      value,       &
                                      unpack_mask)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE ops_odb_wrappers, ONLY: &
  ops_odb_varindex

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
CHARACTER(len=*), INTENT(IN)       :: value
LOGICAL, OPTIONAL, INTENT(IN)      :: unpack_mask(:)

! Local declarations:
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_SetODBVal_s_to_a_char"
INTEGER                            :: idx
INTEGER                            :: i
CHARACTER(len=8)                   :: local_value

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL ops_odb_varindex (session,    &
                       table_name, &
                       var,        &
                       idx)

local_value = value

IF (SIZE (data, dim = 1) > 0) THEN
  IF (PRESENT (unpack_mask)) THEN
    DO i = 1, SIZE (unpack_mask)
      IF (unpack_mask(i)) THEN
        data(i,idx) = TRANSFER (local_value, data(i,idx))
      END IF
    END DO
  ELSE
    data(:,idx) = TRANSFER (local_value, data(1,idx))
  END IF
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetODBVal_s_to_a_char
