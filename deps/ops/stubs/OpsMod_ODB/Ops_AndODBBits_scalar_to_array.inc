!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Set a bit of an ODB column bitfield.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_AndODBBits_scalar_to_array (session,    &
                                           table_name, &
                                           var,        &
                                           data,       &
                                           bits,       &
                                           mask)

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
INTEGER, INTENT(IN)                :: bits
LOGICAL, OPTIONAL, INTENT(IN)      :: mask(:)

! Local declarations:
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_AndODBBits_scalar_to_array"
INTEGER                            :: idx

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL ops_odb_varindex (session,    &
                       table_name, &
                       var,        &
                       idx)

IF (PRESENT (mask)) THEN
  WHERE (mask)
    data(:,idx) = IAND (NINT (data(:,idx)),bits)
  END WHERE
ELSE
  data(:,idx) = IAND (NINT (data(:,idx)),bits)
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_AndODBBits_scalar_to_array
