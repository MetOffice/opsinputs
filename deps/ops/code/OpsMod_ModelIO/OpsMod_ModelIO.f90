!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Module containing the derived data types and subroutines to read all of the
! model fields into OPS program 1. The derived data types are based on those
! held in VarMod_PFinfo.
!-------------------------------------------------------------------------------

MODULE OpsMod_ModelIO

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI,                                  &
  RMDI

USE GenMod_ModelIO, ONLY: &
  UM_header_type

USE OpsMod_DateTime, ONLY: &
  DateTime_type

IMPLICIT NONE

SAVE

! Public declarations:

TYPE DomainInfo_type
  TYPE (UM_header_type), ALLOCATABLE :: header(:)
  REAL                               :: pole_lat = RMDI
  REAL                               :: pole_lon = RMDI
  INTEGER                            :: grid_type = IMDI
  LOGICAL                            :: variable_resolution = .FALSE.
  REAL                               :: first_lat = RMDI
  REAL                               :: first_lon = RMDI
  REAL                               :: lat_spacing = RMDI
  REAL                               :: lon_spacing = RMDI
  INTEGER                            :: num_lats = IMDI
  INTEGER                            :: num_lons = IMDI
END TYPE DomainInfo_type

TYPE NetCDFFields_type
  INTEGER                            :: netcdf_handle = IMDI
  INTEGER                            :: netcdf_num_lats = IMDI
  INTEGER                            :: netcdf_num_lons = IMDI
  INTEGER                            :: netcdf_num_times = IMDI
  REAL, ALLOCATABLE                  :: netcdf_latitudes(:)
  REAL, ALLOCATABLE                  :: netcdf_longitudes(:)
  REAL, ALLOCATABLE                  :: netcdf_cha(:,:,:)
  REAL, ALLOCATABLE                  :: netcdf_mss(:,:,:)
END TYPE NetCDFFields_type

TYPE OceanPresearchGrid_type
  INTEGER                            :: nlons    ! Num of longitudes
  INTEGER                            :: nlats    ! Num of latitudes
  REAL                               :: lonmin   ! Min longitude
  REAL                               :: latmin   ! Min latitude
  REAL                               :: dlon     ! Lon spacing
  REAL                               :: dlat     ! Lat spacing
  INTEGER                            :: maxxdiff
  INTEGER                            :: maxydiff ! Max diffs between model points
  REAL, ALLOCATABLE                  :: lons(:,:)
  REAL, ALLOCATABLE                  :: lats(:,:)
  INTEGER, ALLOCATABLE               :: ixpos(:,:)
  INTEGER, ALLOCATABLE               :: iypos(:,:)
END TYPE OceanPresearchGrid_type

TYPE OceanNetCDFFields_type
  INTEGER                            :: netcdf_handle = IMDI
  INTEGER                            :: netcdf_num_lats = IMDI
  INTEGER                            :: netcdf_num_lons = IMDI
  INTEGER                            :: netcdf_num_depths = IMDI
  INTEGER                            :: netcdf_num_bkg_times = IMDI
  INTEGER                            :: netcdf_num_err_times = IMDI
  TYPE (DateTime_type)               :: ref_bkg_time
  LOGICAL                            :: depth_in_s_levels = .FALSE.
  REAL, ALLOCATABLE                  :: time_counter(:)
  INTEGER, ALLOCATABLE               :: time_counter_adj(:)
  REAL, ALLOCATABLE                  :: nav_lev(:)
  REAL, ALLOCATABLE                  :: nav_lat(:,:)
  REAL, ALLOCATABLE                  :: nav_lon(:,:)
  REAL, ALLOCATABLE                  :: sossheig(:,:,:)
  REAL, ALLOCATABLE                  :: iiceconc(:,:,:)
  REAL, ALLOCATABLE                  :: votemper(:,:,:,:)
  REAL, ALLOCATABLE                  :: vosaline(:,:,:,:)
  REAL, ALLOCATABLE                  :: s_obs_var(:,:,:,:)
  REAL, ALLOCATABLE                  :: s_syn_var(:,:,:,:)
  REAL, ALLOCATABLE                  :: s_mes_var(:,:,:,:)
  REAL, ALLOCATABLE                  :: ssh_mes_var(:,:,:)
  REAL, ALLOCATABLE                  :: ssh_obs_var(:,:,:)
  REAL, ALLOCATABLE                  :: ssh_syn_var(:,:,:)
  REAL, ALLOCATABLE                  :: seaice_mes_var(:,:,:)
  REAL, ALLOCATABLE                  :: seaice_obs_var(:,:,:)
  REAL, ALLOCATABLE                  :: seaice_syn_var(:,:,:)
  REAL, ALLOCATABLE                  :: chl_obs_var(:,:,:)
  REAL, ALLOCATABLE                  :: chl_tot_var(:,:,:)
  REAL, ALLOCATABLE                  :: lchl_obs_var(:,:,:)
  REAL, ALLOCATABLE                  :: lchl_tot_var(:,:,:)
  REAL, ALLOCATABLE                  :: kd490_obs_var(:,:,:)
  REAL, ALLOCATABLE                  :: kd490_tot_var(:,:,:)
  REAL, ALLOCATABLE                  :: lkd490_obs_var(:,:,:)
  REAL, ALLOCATABLE                  :: lkd490_tot_var(:,:,:)
  REAL, ALLOCATABLE                  :: schltot(:,:,:)
  REAL, ALLOCATABLE                  :: skd490tot(:,:,:)
  REAL, ALLOCATABLE                  :: t_obs_var(:,:,:,:)
  REAL, ALLOCATABLE                  :: t_syn_var(:,:,:,:)
  REAL, ALLOCATABLE                  :: t_mes_var(:,:,:,:)
  REAL, ALLOCATABLE                  :: gdept_0(:,:,:,:)
  REAL, ALLOCATABLE                  :: mean_sea_height(:,:)
END TYPE OceanNetCDFFields_type

TYPE (OceanNetCDFFields_type)        :: OceanNetCDFFields
TYPE (OceanPresearchGrid_type)       :: OceanPresearchGrid

CONTAINS

INCLUDE 'Ops_DefineModelArea.inc'
INCLUDE 'Ops_FieldsFile_ReadHeaderEnv.inc'
INCLUDE 'Ops_GetNetCDFField.inc'
INCLUDE 'Ops_InitNetCDFFields.inc'
INCLUDE 'Ops_OceanInitNetCDFFields.inc'
INCLUDE 'Ops_ReadOceanNetCDFFields.inc'
INCLUDE 'Ops_DeallocateOceanNetCDFFields.inc'
INCLUDE 'Ops_FindTimeCoordIndices.inc'
INCLUDE 'Ops_PartitionedFindCoordIndices.inc'
INCLUDE 'Ops_InQuadrilateral.inc'

END MODULE OpsMod_ModelIO
