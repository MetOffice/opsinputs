!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Contains the namelists and routines required for the control of OPS programs.
!-------------------------------------------------------------------------------

MODULE OpsMod_Control

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE OpsMod_DateTime, ONLY: &
  DateTime_type

IMPLICIT NONE

SAVE

! Public declarations:

CHARACTER(len=*), PARAMETER  :: DefaultDocURL = "http://www-nwp/~opsrc/OPS/view/dev/doc"

INTEGER                      :: RunType
INTEGER, PARAMETER           :: RunType_Main = 1
INTEGER, PARAMETER           :: RunType_Anal = 2

INTEGER                      :: ModelType
INTEGER, PARAMETER           :: ModelType_Atmos = 1
INTEGER, PARAMETER           :: ModelType_Ocean = 2
INTEGER, PARAMETER           :: ModelType_SST = 3

! General control options
LOGICAL                      :: ProduceAcobs = .TRUE.
LOGICAL                      :: ProduceVarobs = .TRUE.
LOGICAL                      :: ProduceAssocData = .FALSE.
LOGICAL                      :: ProduceStatistics = .FALSE.
LOGICAL                      :: ProduceListing = .FALSE.
LOGICAL                      :: ProduceNetCDFObs = .FALSE.
LOGICAL                      :: OutputInFeedbackFileFormat = .FALSE.
LOGICAL                      :: ProduceODB = .FALSE.
LOGICAL                      :: ProduceODB2 = .FALSE.
LOGICAL                      :: ProduceBufr = .FALSE.
LOGICAL                      :: ProduceObstore = .FALSE.
LOGICAL                      :: ProduceModelObs = .FALSE.
LOGICAL                      :: ProduceGrib = .FALSE.
LOGICAL                      :: ProduceCx = .FALSE.
LOGICAL                      :: ProduceCxVarobsAllObs = .FALSE.

LOGICAL                      :: read_cx_twice = .FALSE.

! Output options:
LOGICAL                      :: print_absent_fields = .FALSE.
INTEGER                      :: print_level_max = 200
INTEGER                      :: print_level_min = 1
INTEGER                      :: print_level_stride = 1

! Cycle time
INTEGER, PARAMETER           :: cycle_size = 4
INTEGER                      :: CycleTime(cycle_size) = IMDI
INTEGER                      :: CycleUTC = IMDI
INTEGER, PARAMETER           :: cycle_year = 1
INTEGER, PARAMETER           :: cycle_month = 2
INTEGER, PARAMETER           :: cycle_day = 3
INTEGER, PARAMETER           :: cycle_hour = 4
INTEGER, PARAMETER           :: len_runid = 4
CHARACTER(len=len_runid)     :: runid = ""

INTEGER                      :: OverallTimeWindowStart   ! Time Windows used for Cx extraction
INTEGER                      :: OverallTimeWindowEnd     ! --------------"-------------------
TYPE (DateTime_type)         :: overall_window_start
TYPE (DateTime_type)         :: overall_window_end
REAL                         :: ModelNorthBound
REAL                         :: ModelSouthBound
REAL                         :: ModelEastBound
REAL                         :: ModelWestBound
LOGICAL                      :: GlobalRun = .TRUE.

! MPP group for OPS
INTEGER                      :: mpi_group = IMDI

! Out of area/time obs diagnostics
LOGICAL                      :: print_out_of_area_obs = .FALSE.
LOGICAL                      :: print_out_of_time_obs = .FALSE.

! Switch for compatibility with old style obstore
LOGICAL                      :: write_old_obstores = .FALSE.

LOGICAL                      :: CheckBackgroundTimes = .TRUE.

CONTAINS

INCLUDE 'Ops_GetModelType.inc'
INCLUDE 'Ops_InitMPI.inc'
INCLUDE 'Ops_ReadCycleTime.inc'
INCLUDE 'Ops_ReadObsGroupSwitches.inc'
INCLUDE 'Ops_ReadOutputSwitches.inc'
INCLUDE 'OpsFn_ModelRuntoModelId.inc'

END MODULE OpsMod_Control
