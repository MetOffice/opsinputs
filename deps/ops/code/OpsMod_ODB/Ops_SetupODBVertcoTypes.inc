!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Sets up the ODB vertco type values.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SetupODBVertcoTypes (session)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE ops_odb_wrappers, ONLY: &
  ops_odb_getval

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN) :: session

! Local declarations:
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_SetupODBVertcoTypes"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (.NOT. have_setup_vertcotypes) THEN

  vertco_type_amb_wind_num = NINT (ops_odb_getval (session, "$amb_wind_num"))
  vertco_type_cha_number = NINT (ops_odb_getval (session, "$cha_number"))
  vertco_type_derived_pressure = NINT (ops_odb_getval (session, "$derived_pressure"))
  vertco_type_imp_param = NINT (ops_odb_getval (session, "$imp_param"))
  vertco_type_lidar_range = NINT (ops_odb_getval (session, "$lidar_range"))
  vertco_type_model_pressure = NINT (ops_odb_getval (session, "$model_pressure"))
  vertco_type_modlevno = NINT (ops_odb_getval (session, "$modlevno"))
  vertco_type_pressure = NINT (ops_odb_getval (session, "$pressure"))
  vertco_type_tangent_height = NINT (ops_odb_getval (session, "$tangent_height"))
  vertco_type_lane_number = NINT (ops_odb_getval (session, "$lane_number"))

  have_setup_vertcotypes = .TRUE.
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SetupODBVertcoTypes
