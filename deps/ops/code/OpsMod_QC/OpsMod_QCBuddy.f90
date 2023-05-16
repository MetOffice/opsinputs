!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Contains generic Quality Control code:
!   low-level background check routines
!   buddy check: sorting, pairing and actual buddy check routines
!-------------------------------------------------------------------------------

MODULE OpsMod_QCBuddy

USE OpsMod_ObsGroupInfo, ONLY: &
    max_obs_group_num

IMPLICIT NONE

SAVE

! Public declarations:
REAL              :: BdDamp1 = 1.0                     ! Buddy check damping factor 1
REAL              :: BdDamp2 = 0.5                     ! Buddy check damping factor 2
REAL              :: BdDlat = 5.0                      ! width (deg) of latitude bands for sorting
REAL              :: BdGamma = 1.0                     ! Non-divergence constraint
INTEGER           :: BdNumBands = 36                   ! number of latitude bands
REAL              :: BdRangeMax(max_obs_group_num) = 400.0 ! Max distance between buddies (km)
INTEGER           :: MaxSameId(max_obs_group_num) = 0  ! Max number of buddies of same id
INTEGER           :: MaxBdTot(max_obs_group_num) = 9   ! Max number of buddies overall (*)
INTEGER           :: MaxBdRow(max_obs_group_num) = 6   ! Max number of buddies per row (*)
                                                       ! * Buddy checking an ObsGroup with itself this is the number
                                                       ! of pairs in which a particular observation is the first ob.
                                                       ! There is no hard limit on the number of times that it can
                                                       ! appear as the second ob, but in data dense areas an ob will
                                                       ! typically occur MaxBdTot times as first ob and
                                                       ! approx MaxBdTot times as second ob.
REAL              :: CorScaleT = 21600.0               ! Time correlation scale (sec: 360 min)
REAL              :: CorScaleV = 6.0                   ! Vertical correlation scale
REAL              :: CorScaleH = 200.0                 ! Horiz correlation scale (km)
REAL              :: CorScaleHsurf = 200.0             ! Horiz correlation scale (km) surf data
REAL              :: HCfact_NH = 1.0                   ! Northern hemisphere (North of TROPLAT)
REAL              :: HCfact_TR = 1.5                   ! Tropics (between +/-(TROPLAT-TROPINT))
REAL              :: HCfact_SH = 1.5                   ! Southern hemisphere (South of -TROPLAT)
REAL              :: TropLat = 30.0                    ! Latitude at which hemispheric values start
                                                       !   changing to tropical values
REAL              :: TropInt = 10.0                    ! Latitude interval over which values change
REAL              :: BdPrintBox(12) = &                ! Print buddy check calc. for obs in boxes
                           (/ 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0 /)

REAL    :: OceanScaleHsyn = 400.0        ! Synoptic correlation scale (km)
REAL    :: OceanScaleHmes = 100.0        ! Mesoscale correlation scale (km)
REAL    :: OceanHsynFactTR = 2.0         ! Scaling factor for EW scales in 'tropics'
REAL    :: OceanHmesFactTR = 2.0         ! Scaling factor for EW scales in 'tropics'
REAL    :: OceanHsynAnisoTR = 4.0        ! EW/NS scales in 'tropics'
REAL    :: OceanHmesAnisoTR = 4.0        ! EW/NS scales in 'tropics'

CONTAINS

INCLUDE 'Ops_QcBdChkOcean.inc'
INCLUDE 'Ops_QcBdChkSs.inc'
INCLUDE 'Ops_QcBdChkSv.inc'
INCLUDE 'Ops_QcBdChkUs.inc'
INCLUDE 'Ops_QcBdChkUv.inc'
INCLUDE 'Ops_QcBdPair.inc'
INCLUDE 'Ops_QcBdSort.inc'
INCLUDE 'Ops_QcBuddy.inc'
INCLUDE 'Ops_QcBuddyOcean.inc'
INCLUDE 'Ops_QcOceanScales.inc'

END MODULE OpsMod_QCBuddy
