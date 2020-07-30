!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Reads the operator switches which determine the output files to produce.  The
! operator switches are obtained by a series of calls to ops_env_is_true which
! read the switches in from environment variables.
!
! Outputs:
!   Output file production switches as logical variables. These are passed via
!   module OpsMod_Control.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ReadOutputSwitches

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE OpsMod_EnvUtils, ONLY: &
    ops_env_is_false,      &
    ops_env_is_true

IMPLICIT NONE

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_ReadOutputSwitches"

!------------------------------------------------------------------------------
! 0. Tracing
!------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
! 1. Get output switches
!-------------------------------------------------------------------------------

ProduceAcobs = ops_env_is_true ("OPS_PRODUCEACOBS")
ProduceVarobs = ops_env_is_true ("OPS_PRODUCEVAROBS")
ProduceListing = ops_env_is_true ("OPS_PRODUCELISTING")
ProduceStatistics = ops_env_is_true ("OPS_PRODUCESTATISTICS")
ProduceAssocData = ops_env_is_true ("OPS_PRODUCEASSOCDATA")
ProduceObstore = ops_env_is_true ("OPS_PRODUCEOBSTORE")
ProduceModelObs = ops_env_is_true ("OPS_PRODUCEMODELOBS")
ProduceNetCDFObs = ops_env_is_true ("OPS_PRODUCENETCDFOBS")
ProduceODB = ops_env_is_true ("OPS_PRODUCEODB")
ProduceODB2 = ops_env_is_true ("OPS_PRODUCEODB2")
ProduceBufr = ops_env_is_true ("OPS_PRODUCEBUFR")
ProduceGrib = ops_env_is_true ("OPS_PRODUCEGRIB")
ProduceCx = .NOT. ops_env_is_false ("OPS_PRODUCECX")
ProduceCxVarobsAllObs = ops_env_is_true ("OPS_PRODUCECXVAROBSALLOBS")

!-------------------------------------------------------------------------------
! 2. Tracing and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ReadOutputSwitches
