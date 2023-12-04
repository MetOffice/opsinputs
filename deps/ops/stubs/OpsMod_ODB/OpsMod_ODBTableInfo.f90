!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! ODB table related parameters and derived types.
!-------------------------------------------------------------------------------

MODULE OpsMod_ODBTableInfo

USE GenMod_MiscUMScienceConstants, ONLY: &
  RMDI

USE OpsMod_ODBKinds, ONLY: &
  odb_real

IMPLICIT NONE

SAVE

! Public declarations:
TYPE Tables_type
  REAL(kind=odb_real), ALLOCATABLE  :: body(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: sat_body(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: conv_body(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: errstat(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: hdr(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: sat(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: radiance(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: scatt(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: satob(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: gnssro(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: ssmis(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: geocloud(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: modsurf(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: radar(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: conv(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: sbuv(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: amsr(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: aatsr(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: aod(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: iasi_principle_component(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: lidar(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: hloswind(:,:)
  REAL(kind=odb_real), ALLOCATABLE  :: altimeter(:,:)
  LOGICAL                           :: body_active = .FALSE.
  LOGICAL                           :: sat_body_active = .FALSE.
  LOGICAL                           :: conv_body_active = .FALSE.
  LOGICAL                           :: errstat_active = .FALSE.
  LOGICAL                           :: hdr_active = .FALSE.
  LOGICAL                           :: modsurf_active = .FALSE.
  LOGICAL                           :: radar_active = .FALSE.
  LOGICAL                           :: conv_active = .FALSE.
  LOGICAL                           :: sat_active = .FALSE.
  LOGICAL                           :: radiance_active = .FALSE.
  LOGICAL                           :: scatt_active = .FALSE.
  LOGICAL                           :: satob_active = .FALSE.
  LOGICAL                           :: gnssro_active = .FALSE.
  LOGICAL                           :: ssmis_active = .FALSE.
  LOGICAL                           :: geocloud_active = .FALSE.
  LOGICAL                           :: sbuv_active = .FALSE.
  LOGICAL                           :: amsr_active = .FALSE.
  LOGICAL                           :: aatsr_active = .FALSE.
  LOGICAL                           :: aod_active = .FALSE.
  LOGICAL                           :: iasi_principle_component_active = .FALSE.
  LOGICAL                           :: lidar_active = .FALSE.
  LOGICAL                           :: hloswind_active = .FALSE.
  LOGICAL                           :: altimeter_active = .FALSE.
  INTEGER                           :: num_body_elements = 0
  INTEGER, ALLOCATABLE              :: obsvarnos(:)
  INTEGER, ALLOCATABLE              :: obslevels(:)
  REAL, ALLOCATABLE                 :: obsvertco_references(:)
  INTEGER, ALLOCATABLE              :: obsvertco_types(:)
END TYPE

CHARACTER(len=*), PARAMETER         :: body_flags(2) = (/"datum_status",  &
                                                         "datum_event1"/)
CHARACTER(len=*), PARAMETER         :: conv_body_flags(1) = (/"level"/)
CHARACTER(len=*), PARAMETER         :: hdr_flags(2) = (/"report_status",  &
                                                        "report_event1"/)

END MODULE OpsMod_ODBTableInfo
