!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Sets a bit of an ODB bitfield column.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetODBBit_scalar (handle,     &
                                 table_name, &
                                 var,        &
                                 data,       &
                                 bit)

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
TYPE (ODBSession_type), INTENT(IN) :: handle
CHARACTER(len=*), INTENT(IN)       :: table_name
CHARACTER(len=*), INTENT(IN)       :: var
REAL(kind=odb_real), INTENT(INOUT) :: data(0:)
INTEGER, INTENT(IN)                :: bit

! Local declarations:
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_SetODBBit_scalar"
INTEGER                            :: idx

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL ops_odb_varindex (handle,     &
                       table_name, &
                       var,        &
                       idx)

data(idx) = IBSET (NINT (data(idx)),bit)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetODBBit_scalar
