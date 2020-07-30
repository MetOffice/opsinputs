!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Set a cell of an ODB table to a scalar character value.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetODBVal_scalar_char (session,    &
                                      table_name, &
                                      var,        &
                                      data,       &
                                      value)

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
REAL(kind=odb_real), INTENT(INOUT) :: data(0:)
CHARACTER(len=*), INTENT(IN)       :: value

! Local declarations:
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_SetODBVal_scalar_char"
INTEGER                            :: idx
CHARACTER(len=8)                   :: local_value

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL ops_odb_varindex (session,    &
                       table_name, &
                       var,        &
                       idx)

local_value = value

data(idx) = TRANSFER (local_value, data(idx))

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetODBVal_scalar_char
