!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Use data from the Ocean Pre-search grid file ("OceanPresearchGrid_type") to
! refine the coordinate search to local gridpoints to each observation. Use this
! range to find the grid box containing the observations. 
!
! Return i_coordinates,j_coordinates of bottom corner grid point for the model
! grid cell containing the observation.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_PartitionedFindCoordIndices (presearch_data,  &
                                            grid_longitudes, &
                                            grid_latitudes,  &
                                            obs_longitudes,  &
                                            obs_latitudes,   &
                                            i_coordinates,   &
                                            j_coordinates)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
TYPE (OceanPresearchGrid_type), INTENT(IN) :: presearch_data
REAL, INTENT(IN)                           :: grid_longitudes(:,:)         ! Grid point longitude
REAL, INTENT(IN)                           :: grid_latitudes(:,:)          ! Grid point latitude
REAL, INTENT(IN)                           :: obs_longitudes(:)            ! Longitude of obsrvations 
REAL, INTENT(IN)                           :: obs_latitudes(:)             ! Latitude of observations
INTEGER, INTENT(OUT)                       :: i_coordinates(:)             ! I-index of observations 
INTEGER, INTENT(OUT)                       :: j_coordinates(:)             ! J-index of observations 

! Local declarations:
CHARACTER(len=*), PARAMETER                :: RoutineName = 'Ops_PartitionedFindCoordIndices'
REAL, ALLOCATABLE                          :: obs_longitudes_local(:)
REAL                                       :: zlammax ! Maximum for handling wrap-around points
INTEGER                                    :: kpi     ! Number of local longitudes
INTEGER                                    :: kpj     ! Number of local latitudes
INTEGER                                    :: ji
INTEGER                                    :: jj
INTEGER                                    :: i
INTEGER                                    :: isx
INTEGER                                    :: isy
INTEGER                                    :: jimin   ! minimum model region point, i index
INTEGER                                    :: jimax   ! maximum model region point, i index
INTEGER                                    :: jjmin   ! minimum model region point, j index
INTEGER                                    :: jjmax   ! maximum model region point, j index
INTEGER                                    :: jojimin 
INTEGER                                    :: jojimax 
INTEGER                                    :: jojjmin 
INTEGER                                    :: jojjmax 
INTEGER                                    :: ipx1    ! bottom corner presearch_data index
INTEGER                                    :: ipy1    ! bottom corner presearch_data index
INTEGER                                    :: ip
INTEGER                                    :: jp
INTEGER                                    :: ipx
INTEGER                                    :: ipy
INTEGER                                    :: ipmx
REAL, ALLOCATABLE                          :: zphitmax(:,:)
REAL, ALLOCATABLE                          :: zphitmin(:,:)
REAL, ALLOCATABLE                          :: zlamtmax(:,:)
REAL, ALLOCATABLE                          :: zlamtmin(:,:)
REAL, ALLOCATABLE                          :: zlamtm(:,:,:)
REAL, ALLOCATABLE                          :: zphitm(:,:,:)
LOGICAL                                    :: llfourflag
INTEGER                                    :: ifourflagcountt
INTEGER                                    :: ifourflagcountf
INTEGER                                    :: ifourflagcountr(5)

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-----------------------------------------------------------------------
! Define grid for grid search
!-----------------------------------------------------------------------

kpi = SIZE (grid_longitudes, DIM = 1)
kpj = SIZE (grid_longitudes, DIM = 2)

!-----------------------------------------------------------------------
! Set up data for grid search
!-----------------------------------------------------------------------

ALLOCATE (zphitmax(kpi - 1,kpj - 1))
ALLOCATE (zphitmin(kpi - 1,kpj - 1))
ALLOCATE (zlamtmax(kpi - 1,kpj - 1))
ALLOCATE (zlamtmin(kpi - 1,kpj - 1))
ALLOCATE (zlamtm(4,kpi - 1,kpj - 1))
ALLOCATE (zphitm(4,kpi - 1,kpj - 1))

!-----------------------------------------------------------------------
! Copy longitudes
!-----------------------------------------------------------------------

ALLOCATE (obs_longitudes_local(SIZE (Obs_longitudes)))

DO i = 1, SIZE (Obs_longitudes)
   obs_longitudes_local(i) = obs_longitudes(i)
END DO

!-----------------------------------------------------------------------
! Set default values for output
!-----------------------------------------------------------------------

i_coordinates(:) = -1
j_coordinates(:) = -1

!-----------------------------------------------------------------------
! Copy grid positions to temporary arrays and renormalize to 0 to 360.
!-----------------------------------------------------------------------

DO jj = 1, kpj-1
  DO ji = 1, kpi-1
    zlamtm(1,ji,jj) = grid_longitudes(ji,jj)
    zphitm(1,ji,jj) = grid_latitudes(ji,jj)
    zlamtm(2,ji,jj) = grid_longitudes(ji + 1,jj)
    zphitm(2,ji,jj) = grid_latitudes(ji + 1,jj)
    zlamtm(3,ji,jj) = grid_longitudes(ji + 1,jj + 1)
    zphitm(3,ji,jj) = grid_latitudes(ji + 1,jj + 1)
    zlamtm(4,ji,jj) = grid_longitudes(ji,jj + 1)
    zphitm(4,ji,jj) = grid_latitudes(ji,jj + 1)
  END DO
END DO
WHERE (zlamtm(:,:,:) < 0.0)
  zlamtm(:,:,:) = zlamtm(:,:,:) + 360.0
END WHERE
WHERE (zlamtm(:,:,:) > 360.0)
  zlamtm(:,:,:) = zlamtm(:,:,:) - 360.0
END WHERE

!-----------------------------------------------------------------------
! Handle case of the wraparound; beware, not working with orca180
!-----------------------------------------------------------------------

DO jj = 1, kpj-1
  DO ji = 1, kpi-1
    zlammax = MAXVAL (zlamtm(:,ji,jj))
    WHERE (zlammax - zlamtm(:, ji, jj) > 180) zlamtm(:,ji,jj) = zlamtm(:,ji,jj) + 360.0
    zphitmax(ji,jj) = MAXVAL (zphitm(:,ji,jj))
    zphitmin(ji,jj) = MINVAL (zphitm(:,ji,jj))
    zlamtmax(ji,jj) = MAXVAL (zlamtm(:,ji,jj))
    zlamtmin(ji,jj) = MINVAL (zlamtm(:,ji,jj))
  END DO
END DO

!-----------------------------------------------------------------------
! Do coordinate search using pre-search data with local searches.
!-----------------------------------------------------------------------

ifourflagcountt = 0
ifourflagcountf = 0
ifourflagcountr(:) = 0

gpkobs: DO i = 1, SIZE (Obs_longitudes)
  ! Normal case
  !        specify 4 points which surround the lat lon of interest
  !                          x i,j+1  x i+1, j+1
  !
  !                             * lon,lat
  !                          x i,j    x i+1,j

  ! bottom corner point

  ipx1 = INT ((obs_longitudes_local(i) - presearch_data % lonmin) / presearch_data % dlon + 1.0)
  ipy1 = INT ((obs_latitudes(i) - presearch_data % latmin) / presearch_data % dlat + 1.0)

  ipx = ipx1 + 1
  ipy = ipy1 + 1

  ! flag for searching around four points separately
  ! default to false

  llfourflag = .FALSE.

  ! check for point fully outside of region

  IF (ipx1 > presearch_data % nlons .OR. ipy1 > presearch_data % nlats .OR. ipx < 1 .OR. ipy < 1) THEN
    CYCLE
  END IF

  ! check wrap around

  IF (ipx > presearch_data % nlons .OR. ipy > presearch_data % nlats .OR. ipx1 < 1 .OR. ipy1 < 1) THEN
    llfourflag = .TRUE.
    ifourflagcountr(1) = ifourflagcountr(1) + 1
  END IF

  IF (.NOT. llfourflag) THEN
    IF (MAXVAL (presearch_data % ixpos(ipx1:ipx,ipy1:ipy)) == -1) CYCLE ! cycle if no lookup points found
  END IF

  jimin = 0
  jimax = 0
  jjmin = 0
  jjmax = 0

  IF (.NOT. llfourflag) THEN

    ! calculate points range
    ! define a square region encompassing the four corner points
    ! do I need the -1 points?

    jojimin = MINVAL (presearch_data % ixpos(ipx1:ipx,ipy1:ipy)) - 1
    jojimax = MAXVAL (presearch_data % ixpos(ipx1:ipx,ipy1:ipy)) + 1
    jojjmin = MINVAL (presearch_data % iypos(ipx1:ipx,ipy1:ipy)) - 1
    jojjmax = MAXVAL (presearch_data % iypos(ipx1:ipx,ipy1:ipy)) + 1

    jimin = jojimin - 1
    jimax = jojimax + 1
    jjmin = jojjmin - 1
    jjmax = jojjmax + 1

    IF (jojimin < 0 .OR. jojjmin < 0) THEN
      llfourflag = .TRUE.
      ifourflagcountr(2) = ifourflagcountr(2) + 1
    END IF
    IF (jojimax - jojimin > presearch_data % maxxdiff) THEN
      llfourflag = .TRUE.
      ifourflagcountr(3) = ifourflagcountr(3) + 1
    END IF
    IF (jojjmax - jojjmin > presearch_data % maxydiff) THEN
      llfourflag = .TRUE.
      ifourflagcountr(4) = ifourflagcountr(4) + 1
    END IF

  END IF

  ipmx = 0
  IF (llfourflag) ipmx = 1

  IF (llfourflag) THEN
    ifourflagcountt = ifourflagcountt + 1
  ELSE
    ifourflagcountf = ifourflagcountf + 1
  END IF

  gridpointsn : DO ip = 0, ipmx
    DO jp = 0, ipmx

      IF (i_coordinates(i) /= -1) EXIT gridpointsn

      ipx = ipx1 + ip
      ipy = ipy1 + jp

      IF (llfourflag) THEN

        ! deal with wrap around

        IF (ipx > presearch_data % nlons) ipx = 1
        IF (ipy > presearch_data % nlats) ipy = 1
        IF (ipx < 1) ipx = presearch_data % nlons
        IF (ipy < 1) ipy = presearch_data % nlats

        ! get i,j

        isx = presearch_data % ixpos(ipx,ipy)
        isy = presearch_data % iypos(ipx,ipy)

        ! estimate appropriate search region (use max/min values)

        jimin = isx - presearch_data % maxxdiff - 1
        jimax = isx + presearch_data % maxxdiff + 1
        jjmin = isy - presearch_data % maxydiff - 1
        jjmax = isy + presearch_data % maxydiff + 1

      END IF

      IF (jimin < 1) jimin = 1
      IF (jimax > kpi - 1) jimax = kpi - 1
      IF (jjmin < 1) jjmin = 1
      IF (jjmax > kpj - 1) jjmax = kpj - 1

      !---------------------------------------------------------------
      ! Ensure that all observation longtiudes are between 0 and 360
      !---------------------------------------------------------------

      IF (obs_longitudes_local(i) < 0.0) obs_longitudes_local(i) = obs_longitudes_local(i) + 360.0
      IF (obs_longitudes_local(i) > 360.0) obs_longitudes_local(i) = obs_longitudes_local(i) - 360.0

      !---------------------------------------------------------------
      ! Ensure that all observation longtiudes are between -180/180
      !---------------------------------------------------------------

      IF (obs_longitudes_local(i) > 180) obs_longitudes_local(i) = obs_longitudes_local(i) - 360.0

      IF (i_coordinates(i) == -1) THEN

        ! Normal case
        gridpoints : DO jj = jjmin, jjmax
          DO ji = jimin, jimax

            IF (obs_longitudes_local(i) > zlamtmax(ji,jj) .OR. &
                obs_longitudes_local(i) < zlamtmin(ji,jj)) THEN
              CYCLE
            END IF

            IF (ABS (obs_latitudes(i)) < 85) THEN
              IF (obs_latitudes(i) > zphitmax(ji,jj) .OR. &
                  obs_latitudes(i) < zphitmin(ji,jj)) THEN
                CYCLE
              END IF
            END IF

            IF (Ops_InQuadrilateral (obs_longitudes_local(i), obs_latitudes(i), zlamtm(:,ji,jj), zphitm(:,ji,jj))) THEN
              i_coordinates(i) = ji + 1
              j_coordinates(i) = jj + 1
              EXIT gridpoints
            END IF

          END DO
        END DO gridpoints
      END IF

      ! In case of failure retry for obs. longtiude + 360.
      IF (i_coordinates(i) == -1) THEN
        gridpoints_greenwich : DO jj = jjmin, jjmax
          DO ji = jimin, jimax

            IF (obs_longitudes_local(i)+360.0 > zlamtmax(ji,jj) .OR. &
                obs_longitudes_local(i)+360.0 < zlamtmin(ji,jj)) THEN
              CYCLE
            END IF

            IF (ABS (obs_latitudes(i)) < 85) THEN
              IF (obs_latitudes(i) > zphitmax(ji,jj) .OR. &
                  obs_latitudes(i) < zphitmin(ji,jj)) THEN
                CYCLE
              END IF
            END IF

            IF (Ops_InQuadrilateral (obs_longitudes_local(i) + 360.0, obs_latitudes(i), zlamtm(:,ji,jj), zphitm(:,ji,jj))) THEN
              i_coordinates(i) = ji + 1
              j_coordinates(i) = jj + 1
              EXIT gridpoints_greenwich
            END IF
          END DO
        END DO gridpoints_greenwich

      END IF ! in case of retry   

    END DO
  END DO gridpointsn

END DO gpkobs  

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_PartitionedFindCoordIndices
