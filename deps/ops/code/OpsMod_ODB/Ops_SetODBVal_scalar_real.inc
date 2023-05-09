!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Set a cell of an ODB table to a scalar real value.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetODBVal_scalar_real (handle,     &
                                      table_name, &
                                      var,        &
                                      data,       &
                                      value)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  RMDI

USE ops_odb_wrappers, ONLY: &
  ops_odb_varindex

USE OpsMod_ODBKinds, ONLY: &
  odb_real

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: handle
CHARACTER(len=*), INTENT(IN)       :: table_name
CHARACTER(len=*), INTENT(IN)       :: var
REAL(kind=odb_real), INTENT(INOUT) :: data(0:)
REAL, INTENT(IN)                   :: value

! Local declarations:
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_SetODBVal_scalar_real"
INTEGER                            :: idx

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (value /= RMDI) THEN
  CALL ops_odb_varindex (handle,     &
                         table_name, &
                         var,        &
                         idx)

  data(idx) = value
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetODBVal_scalar_real
