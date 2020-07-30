!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Reads the OPS_MODELTYPE environment variable and returns the value.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_GetModelType (model_type)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE OpsMod_EnvUtils, ONLY: &
  ops_get_env

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(OUT)        :: model_type

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_GetModelType"
CHARACTER(len=5)            :: GetEnvOutput

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL ops_get_env ("OPS_MODELTYPE",                 &
                  GetEnvOutput,                    &
                  result_if_env_not_set = "ATMOS")
SELECT CASE (GetEnvOutput)
  CASE ("OCEAN")
    model_type = ModelType_Ocean
  CASE ("SST")
    model_type = ModelType_SST
  CASE ("ATMOS")
    model_type = ModelType_Atmos
  CASE DEFAULT
    CALL gen_fail (RoutineName,                                        &
                   TRIM (GetEnvOutput) // ' is not a valid ModelType')
END SELECT

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_GetModelType
