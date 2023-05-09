!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Set ocean specific length scales and anisotropy.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_QcOceanScales (Latitude, &
                              SynScale, &
                              MesScale, &
                              SynAniso, &
                              MesAniso)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(IN)            :: Latitude(:)
REAL, INTENT(OUT)           :: SynScale(:) ! ocean synoptic error correlation length
REAL, INTENT(OUT)           :: MesScale(:) ! ocean mesoscale error correlation length
REAL, INTENT(OUT)           :: SynAniso(:) ! ocean synoptic error anisotropy
REAL, INTENT(OUT)           :: MesAniso(:) ! ocean mesoscale error anisotropy

! Local declarations:
INTEGER                     :: J         ! loops over observations
REAL                        :: ABSlat    ! absolute value of latitude
REAL                        :: SynFact   ! scaling for ocean synoptic correlation scale
REAL                        :: MesFact   ! scaling for ocean mesoscale correlation scale
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_QcOceanScales"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! 1.  Loop through observations, calculating scales and anisotropies

DO J = 1, SIZE (Latitude)

  ! In the 'extratropics' the scales are set to OceanScaleHsyn/mes
  ! and taken as isotropic.
  ! In the deep tropics the EW scales are OceanScaleH * FactTR
  ! and the NS scales are = (EW scale)/AnisoTR.
  ! The scales and anisotropy vary linearly over the transition latitudes.

  ABSLat = ABS (Latitude(J))
  IF (ABSLat > TropLat) THEN
    SynFact = 1.0
    MesFact = 1.0
    SynAniso(J) = 1.0
    MesAniso(J) = 1.0
  ELSE IF (ABSLat > TropLat - TropInt) THEN
    SynFact = 1.0 + (1.0 - OceanHsynFactTR) * (ABSLat - TropLat) / TropInt
    MesFact = 1.0 + (1.0 - OceanHmesFactTR) * (ABSLat - TropLat) / TropInt
    SynAniso(J) = 1.0 + (1.0 - OceanHsynAnisoTR) * (ABSLat - TropLat) / TropInt
    MesAniso(J) = 1.0 + (1.0 - OceanHmesAnisoTR) * (ABSLat - TropLat) / TropInt
  ELSE
    SynFact = OceanHsynFactTR
    MesFact = OceanHmesFactTR
    SynAniso(J) = OceanHsynAnisoTR
    MesAniso(J) = OceanHmesAnisoTR
  END IF
  SynScale(J) = OceanScaleHsyn * SynFact
  MesScale(J) = OceanScaleHmes * MesFact

END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_QcOceanScales
