!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Constants relating to ODB flags copied from cma.hh.
!-------------------------------------------------------------------------------

MODULE OpsMod_ODBFlags

IMPLICIT NONE

SAVE

! Public declarations:

!===================================================================
! Status Flags
!===================================================================

INTEGER, PARAMETER :: StatusActive = 0
INTEGER, PARAMETER :: StatusPassive = 1
INTEGER, PARAMETER :: StatusRejected = 2
INTEGER, PARAMETER :: StatusBlacklisted = 3
INTEGER, PARAMETER :: StatusBlacklistMonthly = 4
INTEGER, PARAMETER :: StatusBlacklistConstant = 5
INTEGER, PARAMETER :: StatusBlacklistExperimental = 6
INTEGER, PARAMETER :: StatusWhitelisted = 7

!===================================================================
! Report RDB Flags
!===================================================================

INTEGER, PARAMETER :: RepRDB_lat_humon = 0
INTEGER, PARAMETER :: RepRDB_lat_QCsub = 1
INTEGER, PARAMETER :: RepRDB_lat_override = 2
INTEGER, PARAMETER :: RepRDB_lat_flag = 3
INTEGER, PARAMETER :: RepRDB_lat_HQC_flag = 4
INTEGER, PARAMETER :: RepRDB_lon_humon = 5
INTEGER, PARAMETER :: RepRDB_lon_QCsub = 6
INTEGER, PARAMETER :: RepRDB_lon_override = 7
INTEGER, PARAMETER :: RepRDB_lon_flag = 8
INTEGER, PARAMETER :: RepRDB_lon_HQC_flag = 9
INTEGER, PARAMETER :: RepRDB_date_humon = 10
INTEGER, PARAMETER :: RepRDB_date_QCsub = 11
INTEGER, PARAMETER :: RepRDB_date_override = 12
INTEGER, PARAMETER :: RepRDB_date_flag = 13
INTEGER, PARAMETER :: RepRDB_date_HQC_flag = 14
INTEGER, PARAMETER :: RepRDB_time_humon = 15
INTEGER, PARAMETER :: RepRDB_time_QCsub = 16
INTEGER, PARAMETER :: RepRDB_time_override = 17
INTEGER, PARAMETER :: RepRDB_time_flag = 18
INTEGER, PARAMETER :: RepRDB_time_HQC_flag = 19
INTEGER, PARAMETER :: RepRDB_stalt_humon = 20
INTEGER, PARAMETER :: RepRDB_stalt_QCsub = 21
INTEGER, PARAMETER :: RepRDB_stalt_override = 22
INTEGER, PARAMETER :: RepRDB_stalt_flag = 23
INTEGER, PARAMETER :: RepRDB_stalt_HQC_flag = 24

!===================================================================
! Datum RDB Flags
!===================================================================

INTEGER, PARAMETER :: DatRDB_press_humon = 0
INTEGER, PARAMETER :: DatRDB_press_QCsub = 1
INTEGER, PARAMETER :: DatRDB_press_override = 2
INTEGER, PARAMETER :: DatRDB_press_flag = 3
INTEGER, PARAMETER :: DatRDB_press_HQC_flag = 4
INTEGER, PARAMETER :: DatRDB_press_judged_prev_an = 5
INTEGER, PARAMETER :: DatRDB_press_used_prev_an = 6
INTEGER, PARAMETER :: DatRDB_press_unused = 7
INTEGER, PARAMETER :: DatRDB_varno_humon = 8
INTEGER, PARAMETER :: DatRDB_varno_QCsub = 9
INTEGER, PARAMETER :: DatRDB_varno_override = 10
INTEGER, PARAMETER :: DatRDB_varno_flag = 11
INTEGER, PARAMETER :: DatRDB_varno_HQC_flag = 12
INTEGER, PARAMETER :: DatRDB_varno_judged_prev_an = 13
INTEGER, PARAMETER :: DatRDB_varno_used_prev_an = 14

!===================================================================
! Report Event #1 Flags
!===================================================================

INTEGER, PARAMETER :: RepEvent_no_data = 0
INTEGER, PARAMETER :: RepEvent_all_rejected = 1
INTEGER, PARAMETER :: RepEvent_bad_practice = 2
INTEGER, PARAMETER :: RepEvent_rdb_rejected = 3
INTEGER, PARAMETER :: RepEvent_rdb_activated = 4
INTEGER, PARAMETER :: RepEvent_whitelist_activated = 5
INTEGER, PARAMETER :: RepEvent_horipos_outrange = 6
INTEGER, PARAMETER :: RepEvent_time_outrange = 8
INTEGER, PARAMETER :: RepEvent_redundant = 9
INTEGER, PARAMETER :: RepEvent_land = 10
INTEGER, PARAMETER :: RepEvent_sea = 11
INTEGER, PARAMETER :: RepEvent_stalt_missing = 12
INTEGER, PARAMETER :: RepEvent_modsurf_stalt_distance = 13
INTEGER, PARAMETER :: RepEvent_namelist_rejected = 14
INTEGER, PARAMETER :: RepEvent_QC_failed = 15
INTEGER, PARAMETER :: RepEvent_blacklisted = 16
INTEGER, PARAMETER :: RepEvent_used_in_superob = 17
INTEGER, PARAMETER :: RepEvent_track_reject = 18
INTEGER, PARAMETER :: RepEvent_superob = 20
INTEGER, PARAMETER :: RepEvent_GoodConstraint = 22

!===================================================================
! Datum Event #1 Flags
!===================================================================

INTEGER, PARAMETER :: DatEvent_vertco_missing = 0
INTEGER, PARAMETER :: DatEvent_obsvalue_missing = 1
INTEGER, PARAMETER :: DatEvent_fg_missing = 2
INTEGER, PARAMETER :: DatEvent_rdb_rejected = 3
INTEGER, PARAMETER :: DatEvent_rdb_activated = 4
INTEGER, PARAMETER :: DatEvent_whitelist_activated = 5
INTEGER, PARAMETER :: DatEvent_bad_practice = 6
INTEGER, PARAMETER :: DatEvent_vertpos_outrange = 7
INTEGER, PARAMETER :: DatEvent_reflevel_outrange = 8
INTEGER, PARAMETER :: DatEvent_fg2big = 9
INTEGER, PARAMETER :: DatEvent_depar2big = 10
INTEGER, PARAMETER :: DatEvent_obs_error2big = 11
INTEGER, PARAMETER :: DatEvent_datum_redundant = 12
INTEGER, PARAMETER :: DatEvent_level_redundant = 13
INTEGER, PARAMETER :: DatEvent_land = 14
INTEGER, PARAMETER :: DatEvent_sea = 15
INTEGER, PARAMETER :: DatEvent_back_performed = 16
INTEGER, PARAMETER :: DatEvent_duplicate = 17
INTEGER, PARAMETER :: DatEvent_levels2many = 18
INTEGER, PARAMETER :: DatEvent_multilevel_check = 19
INTEGER, PARAMETER :: DatEvent_level_selection = 20
INTEGER, PARAMETER :: DatEvent_vertco_consistency = 21
INTEGER, PARAMETER :: DatEvent_vertco_type_changed = 22
INTEGER, PARAMETER :: DatEvent_namelist_rejected = 23
INTEGER, PARAMETER :: DatEvent_combined_flagging = 24
INTEGER, PARAMETER :: DatEvent_report_rejected = 25
INTEGER, PARAMETER :: DatEvent_extreme_value = 26
INTEGER, PARAMETER :: DatEvent_consistency = 27
INTEGER, PARAMETER :: DatEvent_data_correct = 28
INTEGER, PARAMETER :: DatEvent_buddy_reject = 29
INTEGER, PARAMETER :: DatEvent_corr_performed = 30

!===================================================================
! Record Type Flags
!===================================================================

INTEGER, PARAMETER :: RecType_Superob = 0
INTEGER, PARAMETER :: RecType_ModelLevel = 1

END MODULE OpsMod_ODBFlags
