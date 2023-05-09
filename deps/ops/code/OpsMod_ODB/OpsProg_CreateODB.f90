!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Converts MetDB data to an ODB.  The basic inputs are the same as for
! OpsProg_ExtractAndProcess (OPS_CUTOFF, the normal observation group switches,
! extract control namelists).  Each subtype is extracted from the MetDB and
! written to the (ECMA) ODB separately.
!-------------------------------------------------------------------------------

PROGRAM OpsProg_CreateODB

USE GenMod_Control, ONLY: &
  GeneralMode,            &
  QuietMode,              &
  mype

USE GenMod_Core, ONLY:  &
  gen_close_stats_file, &
  gen_fail,             &
  gen_open_stats_file,  &
  gen_summary,          &
  gen_trace_entry,      &
  gen_trace_exit,       &
  gen_trace_report,     &
  gen_warn,             &
  MessageOut,           &
  StatsOut,             &
  StatusOK,             &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE GenMod_Setup, ONLY: &
  Gen_SetupControl

USE OpsMod_Control, ONLY:   &
  cycle_year,               &
  cycle_month,              &
  cycle_day,                &
  cycle_hour,               &
  CycleTime,                &
  DefaultDocURL,            &
  ModelType,                &
  Ops_GetModelType,         &
  Ops_InitMPI,              &
  Ops_ReadCycleTime,        &
  Ops_ReadObsGroupSwitches, &
  Ops_ReadOutputSwitches,   &
  Runid

USE OpsMod_DateTime, ONLY: &
  DateTime_type,           &
  OpsFn_DateTime_now

USE OpsMod_EnvUtils, ONLY: &
  ops_env_is_true

USE OpsMod_Extract, ONLY: &
  default_cutoff,         &
  default_cutoff_before,  &
  default_EastBound,      &
  default_NorthBound,     &
  default_SouthBound,     &
  default_WestBound,      &
  Extract_type,           &
  MainObsource,           &
  Obsource_BUFR,          &
  Obsource_BufrFile,      &
  Obsource_BufrDir,       &
  Obsource_HDFFile,       &
  Obsource_MetDB,         &
  Obsource_Obstore,       &
  Ops_GetCutoff,          &
  Ops_GetObsource,        &
  OpsFn_ObsourcetoString

USE OpsMod_ExtractManager, ONLY:  &
  Ops_PrepStructures,             &
  Ops_ReadExtractControlDefaults

USE OpsMod_ExtraObs, ONLY: &
  Ops_ExtraObsOnly

USE OpsMod_Gcom, ONLY: &
  gc_exit

USE OpsMod_GRIB2Extract, ONLY: &
  Ops_GRIB2Extraction

USE OpsMod_GRIBExtract, ONLY: &
  Ops_GRIBExtraction

USE OpsMod_GroundLidar, ONLY: &
  Ops_ReadGroundLidarNL

USE OpsMod_HDFExtract, ONLY: &
  Ops_HDF5Extraction

USE OpsMod_HDFOcean, ONLY:      &
  Ops_HDF5ExtractionOcean,      &
  Ops_HDFFileObStructureCreate

USE OpsMod_MetDBExtract, ONLY: &
  Ops_MetDBExtraction

USE OpsMod_MetDBGeneral, ONLY: &
  Ops_KillMetdb

USE OpsMod_ObsGroupInfo, ONLY: &
  ObsGroupGroundLidar,         &
  ObsGroupRadar,               &
  ObsGroupRadarN,              &
  ObsGroupRadarZ,              &
  OpsFn_ObsGroupNumToName

USE OpsMod_ObstoreExtract, ONLY: &
  Ops_ObstoreExtraction,         &
  Ops_ReadFromAnObstore

USE OpsMod_ObstoreUtils, ONLY: &
  obstore_type,                &
  ops_obstore_close,           &
  ops_obstore_open_read

USE OpsMod_ODB2Funcs, ONLY: &
  Ops_MetDBExtraction_ODB2

USE OpsMod_ODBCreate, ONLY: &
  Ops_SubTypeDatatoODB

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

USE OpsMod_ODBUtils, ONLY: &
  Ops_CloseODB,            &
  Ops_OpenODB,             &
  Ops_ReadODBControlNL,    &
  OpsODB_InitODBElemDesp

USE OpsMod_Radar, ONLY: &
  Ops_ReadRadarNL

USE OpsMod_TURBO, ONLY: &
  Ops_TURBOExtraction

IMPLICIT NONE

! Local declarations:
CHARACTER(len=*), PARAMETER :: ProgName = "OpsProg_CreateODB"
INTEGER                     :: Error
INTEGER                     :: rc
TYPE (ODBSession_type)      :: ecma
INTEGER                     :: i
INTEGER                     :: j
INTEGER                     :: k
TYPE (Extract_type)         :: TempMDBData
INTEGER, POINTER            :: obs_group_list(:)
CHARACTER(len=80)           :: StatsString
TYPE (obstore_type)         :: obstore
LOGICAL                     :: odb1_mode
INTEGER                     :: start_ob
INTEGER                     :: end_ob
TYPE (DateTime_type)        :: date_time

CALL Gen_SetupControl (DefaultDocURL)

CALL Ops_InitMPI

!-------------------------
! 0.0 Print message
!-------------------------

IF (GeneralMode >= QuietMode .AND. mype == 0) THEN

  WRITE (MessageOut, '(A)') "========================================="
  WRITE (MessageOut, '(A)') "OpsProg_CreateODB : Execution starts"
  date_time = OpsFn_DateTime_now ()
  WRITE (MessageOut, "(A4,I2.2,A1,I2.2,A1,I2.2,A4,I2.2,A1,I2.2,A1,I4)")        &
    "at ", date_time % hour, ":", date_time % minute, ":", date_time % second, &
    " on ", date_time % day, "/", date_time % month, "/", date_time % year
  WRITE (MessageOut, '(A)') "========================================="

END IF

IF (UseTrace) CALL gen_trace_entry (ProgName)

CALL gen_open_stats_file ("OpsProg_CreateODB statistics", &
                          rc)
IF (rc /= StatusOK) THEN
  CALL gen_fail (ProgName,                       &
                 "Error in gen_open_stats_file")
END IF

CALL Ops_ReadODBControlNL

odb1_mode = .NOT. ops_env_is_true ("OPS_CREATEODB_FORMAT_ODB2")

IF (odb1_mode) THEN
  IF (ops_env_is_true ("OPS_APPEND_ODB")) THEN
    CALL Ops_OpenODB (ecma,  &
                      "OLD", &
                      "ECMA")
  ELSE
    CALL Ops_OpenODB (ecma,  &
                      "NEW", &
                      "ECMA")
  END IF
END IF

CALL Ops_ReadCycleTime

IF (mype == 0) THEN
  WRITE (StatsOut, '(A,2(I2.2,A),I4.4,A,I2.2)') " " // TRIM (Runid) // " ",&
              CycleTime(cycle_day), "/", CycleTime(cycle_month), "/", &
              CycleTime(cycle_year), " ", cycleTime(cycle_hour)
END IF

CALL Ops_ReadObsGroupSwitches (obs_group_list)
CALL Ops_ReadOutputSwitches

IF (COUNT (obs_group_list /= IMDI) == 0) THEN
  CALL gen_warn (ProgName,                         &
                 'No Observation types requested')
  GOTO 9999
END IF

CALL Ops_GetObsource (MainObsource)

CALL Ops_GetCutoff (default_cutoff,        &
                    default_cutoff_before)

CALL Ops_GetModelType (ModelType)

default_NorthBound = 90.0
default_SouthBound = -90.0
default_EastBound = 180.0
default_WestBound = -180.0

CALL Ops_ReadExtractControlDefaults

DO i = 1, SIZE (obs_group_list)

  TempMDBData % ObGroup = obs_group_list(i)

  CALL OpsODB_InitODBElemDesp (ecma,              &
                               obs_group_list(i))

  IF (MainObsource == Obsource_Obstore) THEN
    CALL Ops_PrepStructures (TempMDBData,           &
                             obstore = obstore,     &
                             metdb_to_odb = .TRUE.)
  ELSE
    CALL Ops_PrepStructures (TempMDBData,           &
                             metdb_to_odb = .TRUE.)
  END IF

  IF (obs_group_list(i) == ObsGroupRadar .OR. &
      obs_group_list(i) == ObsGroupRadarN .OR. &
      obs_group_list(i) == ObsGroupRadarZ) THEN
    CALL Ops_ReadRadarNL (TempMDBData % ObGroup)
  END IF

  IF (obs_group_list(i) == ObsGroupGroundLidar) THEN
    CALL Ops_ReadGroundLidarNL
  END IF

  IF (MainObsource == Obsource_Obstore .AND. &
      TempMDBData % SubTypeData(1) % SubTypeName == "ALL") THEN
    CALL Ops_ReadFromAnObstore (TempMDBData)
    DO j = 1, TempMDBData % NumSubTypes
      IF (TempMDBData % SubTypeData(j) % TotalNumObs > 0) THEN
        DO k = 1, TempMDBData % SubTypeData(j) % NumBatches
          start_ob = SUM (TempMDBData % SubTypeData(j) % ObsPerBatchPerPe(1:k - 1,mype)) + 1
          end_ob = SUM (TempMDBData % SubTypeData(j) % ObsPerBatchPerPe(1:k,mype))
          CALL Ops_SubTypeDatatoODB (TempMDBData % SubTypeData(j), &
                                     start_ob,                     &
                                     end_ob,                       &
                                     ecma)
        END DO
      END IF
    END DO
  ELSE
    IF (MainObsource == Obsource_Obstore) THEN
      CALL ops_obstore_open_read (TempMDBData % ObGroup, &
                                  obstore,               &
                                  .TRUE.)
    END IF
    DO j = 1, TempMDBData % NumSubtypes
      IF (j > 1) THEN
        TempMDBData % SubTypeData(j) % IdStart = &
                    SUM (TempMDBData % SubTypeData(1:j - 1) % TotalNumObs)
      ELSE
        TempMDBData % SubTypeData(j) % IdStart = 0
      END IF
      IF (odb1_mode) THEN
        IF (TempMDBData % SubTypeData(j) % extraobs_only) THEN
          CALL Ops_ExtraObsOnly (TempMDBData % SubTypeData(j), &
                                 ecma = ecma)
        ELSE
          IF ((TempMDBData % SubTypeData(j) % Obsource == Obsource_BUFR .OR. &
               TempMDBData % SubTypeData(j) % Obsource == Obsource_BufrFile .OR. &
               TempMDBData % SubTypeData(j) % Obsource == Obsource_BufrDir) .AND. &
              TempMDBData % SubTypeData(j) % bufr_extract) THEN
            CALL Ops_TURBOExtraction (TempMDBData % SubTypeData(j),                                                          &
                                      TempMDBData % SubTypeData(j) % LenRep * TempMDBData % SubTypeData(j) % BUFRBufferSize, &
                                      ecma = ecma)
          ELSE IF (TempMDBData % SubTypeData(j) % Obsource == Obsource_BufrFile .OR. &
                   TempMDBData % SubTypeData(j) % Obsource == Obsource_BufrDir) THEN
            CALL gen_fail (ProgName,                                                    &
                           "Bufr file or directory input not supported for subtype " // &
                             TRIM (TempMDBData % SubTypeData(j) % SubTypeName))
          ELSE IF (MainObsource == Obsource_obstore) THEN
            CALL Ops_ObstoreExtraction (TempMDBData % SubTypeData(j), &
                                        obstore,                      &
                                        ecma = ecma)
          ELSE IF (TempMDBData % SubTypeData(j) % GribData) THEN
            CALL Ops_GRIBExtraction (TempMDBData % SubTypeData(j), &
                                     ecma = ecma)
          ELSE IF (TempMDBData % SubTypeData(j) % Grib2Data) THEN
            CALL Ops_GRIB2Extraction (TempMDBData % SubTypeData(j), &
                                      ecma = ecma)
          ELSE IF (MainObsource == Obsource_HDFFile) THEN
            CALL Ops_HDFFileObStructureCreate (TempMDBData % SubTypeData(j),&
                                               ecma = ecma)
          ELSE IF (TempMDBData % SubTypeData(j) % HDF5) THEN
            IF (TempMDBData % SubTypeData(j) % SubTypeName(1:5) == "OCEAN" .OR. &
                TempMDBData % SubTypeData(j) % SubTypeName(1:8) == "OCEANCOL" .OR. &
                TempMDBData % SubTypeData(j) % SubTypeName(1:3) == "COP") THEN
              CALL Ops_HDF5ExtractionOcean (TempMDBData % SubTypeData(j), &
                                            ecma = ecma)
            ELSE
              CALL Ops_HDF5Extraction (TempMDBData % SubTypeData(j), &
                                       ecma = ecma)
            END IF
          ELSE
            CALL Ops_MetDBExtraction (TempMDBData % SubTypeData(j), &
                                      ecma = ecma)
          END IF
        END IF
      ELSE
        IF (mype == 0) THEN
          CALL Ops_MetDBExtraction_ODB2 (obs_group_list(i),            &
                                         TempMDBData % SubTypeData(j))
        END IF
      END IF
    END DO
    IF (MainObsource == Obsource_Obstore) THEN
      CALL ops_obstore_close (obstore,               &
                              WriteHeader = .FALSE.)
    END IF
  END IF

  IF (mype == 0) THEN
    WRITE (StatsOut, '(A)') &
        ' --------------------------------------------------------------------'
    StatsString = ' Total ' // TRIM (OpsFn_ObsourcetoString(MainObsource)) // ' ' //    &
                  TRIM (OpsFn_ObsGroupNumToName(obs_group_list(i))) // ' obs '
    WRITE (StatsString(35:44), '(A2,I8)') '= ', &
           SUM (TempMDBData % SubTypeData(:) % TotalNumObs)
    WRITE (StatsOut, '(A)') TRIM (StatsString)
    WRITE (StatsOut, '(A)') &
        ' --------------------------------------------------------------------'
    WRITE (StatsOut, '(A)') ' '
  END IF
  IF (TempMDBData % NumSubTypes > 0) THEN
    DEALLOCATE (TempMDBData % SubTypeData)
  END IF
END DO

IF (mype == 0 .AND. &
    (MainObsource == Obsource_MetDB .OR. MainObsource == Obsource_BUFR) .AND. &
    ANY (obs_group_list(:) /= ObsGroupRadarN .AND. &
         obs_group_list(:) /= ObsGroupGroundLidar)) THEN
  CALL Ops_KillMetdb
END IF

IF (odb1_mode) THEN
  CALL Ops_CloseODB (ecma)
END IF

9999 CONTINUE

CALL gen_close_stats_file (Error)
IF (Error /= StatusOK) THEN
  CALL gen_fail (ProgName,                  &
                 "Cannot close stats file")
END IF

CALL gen_summary

IF (UseTrace) THEN
  CALL gen_trace_exit (ProgName)
  CALL gen_trace_report
END IF

IF (GeneralMode >= QuietMode .AND. mype == 0) THEN

  WRITE (MessageOut,'(A)') "========================================="
  WRITE (MessageOut,'(A)') "OpsProg_CreateODB ends normally"
  date_time = OpsFn_DateTime_now ()
  WRITE(MessageOut, "(A4,I2.2,A1,I2.2,A1,I2.2,A4,I2.2,A1,I2.2,A1,I4)")         &
    "at ", date_time % hour, ":", date_time % minute, ":", date_time % second, &
    " on ", date_time % day, "/", date_time % month, "/", date_time % year
  WRITE (MessageOut,'(A)') "========================================="

END IF

CALL gc_exit

END PROGRAM OpsProg_CreateODB
