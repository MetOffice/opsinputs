!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Writes an array of data to an ODB table.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_WriteODBTable (h,          &
                              table_name, &
                              data,       &
                              poolno)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE ops_odb_wrappers, ONLY: &
  ops_odb_put

USE OpsMod_ODBKinds, ONLY: &
  odb_real

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: h
CHARACTER(len=*), INTENT(IN)       :: table_name
REAL(kind=odb_real), INTENT(INOUT) :: data(:,0:)
INTEGER, OPTIONAL, INTENT(IN)      :: poolno

! Local declarations:
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_WriteODBTable"

IF (UseTrace) CALL gen_trace_entry (RoutineName,          &
                                    Message = table_name)

CALL ops_odb_put (h,                    &
                  table_name,           &
                  data,                 &
                  pool_number = poolno)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_WriteODBTable
