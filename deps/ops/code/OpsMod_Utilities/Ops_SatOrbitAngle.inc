!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Compute the angle around the orbit relative to the Earth-Sun ecliptic plane,
!
! Returns angle between 0 - 360 degrees.
! If input values are out of range, angles of RMDI will be returned.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_SatOrbitAngle (Obs,          & ! Time, EphemLat, EphemLon, Lat, Lon
                              Orbit_angles, & ! Satetllite Orbital Angles in degrees
                              mode)

USE GenMod_Constants, ONLY: &
    RMDItol                   ! Tolerance allowed on Missing data values.

USE GenMod_Control, ONLY: &
    DebugMode,            &
    VerboseMode

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI,                                &
    RMDI

USE OpsMod_DateTime, ONLY:   &
    DateTime_type,           &
    minutes_in_day,          &
    OpsFn_DateTime_Create,   &
    OpsFn_DateTime_ToMinUTC, &
    seconds_in_day

USE OpsMod_ObsInfo, ONLY: &
    ob_type,              &
    OutOfAreaReport

IMPLICIT NONE

! Subroutine arguments:
TYPE (ob_type), INTENT(IN)      :: Obs
REAL, INTENT(OUT)               :: Orbit_angles(:)             ! satellite orbit angles, degrees
INTEGER, INTENT(IN)             :: mode

! Local declarations:
REAL, PARAMETER                 :: Sec2Min = 1.0 / 60.0               ! Convert seconds to minutes.
REAL, PARAMETER                 :: mins2days = 1.0 / minutes_in_day   ! Convert minutes to days.
CHARACTER(len=*), PARAMETER     :: RoutineName = "Ops_SatOrbitAngle"
INTEGER                         :: Diy                   ! Day in year (1-366)
INTEGER                         :: Time                  ! Time GMT (millisecs from midnight)
INTEGER                         :: i                     ! loop through spots 1 to NLocations
REAL                            :: lat                   ! latitude
REAL                            :: lon                   ! longitude
REAL                            :: elat(3)                   ! ephemeris latitudes
REAL                            :: elon(3)                   ! ephemeris longitudes
REAL                            :: ex1
REAL                            :: ey1
REAL                            :: ez1
REAL                            :: ex2
REAL                            :: ey2
REAL                            :: ez2
REAL                            :: pos_vector(3)
REAL                            :: vel_vector(3)
REAL                            :: timesi                ! time offset for ith Ob.
REAL                            :: daystart              ! Time offsets within current day.
REAL                            :: dayend                ! End of current day.
INTEGER                         :: MinUTCRef             ! Reference time as min UTC.
INTEGER                         :: MinUTCNow             ! Current time as min UTC
INTEGER                         :: MinUTCJan1            ! Min UTC for 1/1/Year
REAL                            :: secsRef
REAL                            :: secsNow               ! Seconds and fractions
INTEGER                         :: num_miss_time         ! count number failures
INTEGER                         :: num_miss_lon
INTEGER                         :: num_miss_lat
INTEGER                         :: num_miss_elon
INTEGER                         :: num_miss_elat
INTEGER                         :: num_fail_time
INTEGER                         :: num_fail_daterange
INTEGER                         :: num_fail_timerange
INTEGER                         :: num_fail_angle
INTEGER                         :: NLocations             ! number of points in arrays
CHARACTER(len=100), ALLOCATABLE :: messages(:)
INTEGER                         :: num_messages
TYPE (DateTime_type)            :: time_now

IF (UseTrace) CALL gen_trace_entry (RoutineName)

daystart = RMDI
dayend = RMDI
NLocations = Obs % header % NumObsLocal
num_miss_time = 0
num_miss_lon = 0
num_miss_lat = 0
num_miss_elon = 0
num_miss_elat = 0
num_fail_time = 0
num_fail_daterange = 0
num_fail_timerange = 0
num_fail_angle = 0

!-------------------------------------------------------------------------------
! Initialise output as missing data
!-------------------------------------------------------------------------------

Orbit_angles(1:NLocations) = RMDI

!-------------------------------------------------------------------------------
! Convert reference time to minutes UTC.
!-------------------------------------------------------------------------------

MinUTCRef = OpsFn_DateTime_ToMinUTC (Obs % Header % ValidityTime)
secsRef = Obs % Header % ValidityTime % second

IF (mode >= VerboseMode) THEN
  ALLOCATE (messages(7 * NLocations + 6))
ELSE IF (mode >= DebugMode) THEN
  ALLOCATE (messages(6))
END IF

num_messages = 0

! Loop through spots
DO i = 1, NLocations
  lat = Obs % Latitude(i)
  lon = Obs % Longitude(i)
  timesi = Obs % Time(i)
  elat = Obs % EphemLat(i,:)
  elon = Obs % EphemLon(i,:)

  ! If coordinates are not valid then do not process further.
  IF (ABS (timesi - RMDI) <= RMDItol) THEN
    IF (mode >= VerboseMode) THEN
      num_messages = num_messages + 1
      WRITE (messages(num_messages), '(A,A,I0,A)') &
        RoutineName, ' Missing time encountered for ob ',i,&
              '. Output set to missing data.'
    END IF
    num_miss_time = num_miss_time + 1
    CYCLE
  END IF

  IF (BTEST (Obs % ReportFlags(i), OutOfAreaReport)) THEN
    IF (mode >= VerboseMode) THEN
      num_messages = num_messages + 1
      WRITE (messages(num_messages), '(A,A,I0,A)') &
        RoutineName, ' Missing time encountered for ob ',i,&
              '. Output set to missing data.'
    END IF
    num_miss_time = num_miss_time + 1
    CYCLE
  END IF

  IF (ABS (lat - RMDI) <= RMDItol) THEN
    IF (mode >= VerboseMode) THEN
      num_messages = num_messages + 1
      WRITE (messages(num_messages), '(A,A,I0,A)') &
        RoutineName, ' Missing Latitude encountered for ob ',i,&
              '. Output set to missing data.'
    END IF
    num_miss_lat = num_miss_lat + 1
    CYCLE
  END IF

  IF (ABS (lon - RMDI) <= RMDItol) THEN
    IF (mode >= VerboseMode) THEN
      num_messages = num_messages + 1
      WRITE (messages(num_messages), '(A,A,I0,A)') &
        RoutineName, ' Missing Longitude encountered for ob ',i,&
              '. Output set to missing data.'
    END IF
    num_miss_lon = num_miss_lon + 1
    CYCLE
  END IF

  IF ((ABS (elat(1) - RMDI) <= RMDItol) .OR. (ABS (elat(2) - RMDI) <= RMDItol)) THEN
    IF (mode >= VerboseMode) THEN
      num_messages = num_messages + 1
      WRITE (messages(num_messages), '(A,A,I0,A)') &
        RoutineName, ' Missing Ephemeris Latitude(1) encountered for ob ',i,&
              '. Output set to missing data.'
    END IF
    num_miss_elat = num_miss_elat + 1
    CYCLE
  END IF

  IF ((ABS (elon(1) - RMDI) <= RMDItol) .OR. (ABS (elon(2) - RMDI) <= RMDItol)) THEN
    IF (mode >= VerboseMode) THEN
      num_messages = num_messages + 1
      WRITE (messages(num_messages), '(A,A,I0,A)') &
        RoutineName, ' Missing Ephemeris Longitude(1) encountered for ob ',i,&
              '. Output set to missing data.'
    END IF
    num_miss_elon = num_miss_elon + 1
    CYCLE
  END IF

  ! If this day is different to previous, then recompute time data.
  IF (timesi < daystart .OR. timesi >= dayend) THEN

    !   Find current year, month & day by adding time to reference.

    MinUTCNow = MinUTCRef + FLOOR ((secsRef + timesi) * sec2Min)
    time_now = OpsFn_DateTime_Create (MinUTCNow)

    IF (time_now % year == IMDI) THEN
      IF (mode >= VerboseMode) THEN
        num_messages = num_messages + 1
        WRITE (messages(num_messages), '(A,A,I0,A)') &
          RoutineName, ' Obs time conversion failed for ob ', i, &
                '. Output set to missing data.'
      END IF
      num_fail_time = num_fail_time + 1
    END IF

    secsNow = timesi + secsRef + (MinUTCRef - MinUTCNow) * 60.0

    ! Get the time offset in seconds corresponding to the start of the
    ! day that we are in now.

    daystart =  timesi - REAL (time_now % hour * 60 + time_now % minute) * 60.0 - secsNow
    dayend = daystart + seconds_in_day

    ! Find the start of the current year.
    MinUTCJan1 = OpsFn_DateTime_ToMinUTC (OpsFn_DateTime_Create (year = time_now % year, &
                                                                 month = 1,              &
                                                                 day = 1,                &
                                                                 hour = 0,               &
                                                                 minute = 0))

    ! And use that to compute the current day in year. (First day is 1).
    diy = FLOOR ((MinUTCNow - MinUTCJan1) * mins2days) + 1

    IF (time_now % year > 1950 .AND. time_now % year < 2200 .AND.   &
        Diy >= 1 .AND. Diy <= 366) THEN
      continue
    ELSE
      IF (mode >= VerboseMode) THEN
        num_messages = num_messages + 1
        WRITE (messages(num_messages),'(A,5(A,I0))') &
          RoutineName, ' Obs time conversion failed for ob ', i, &
                '. Date is out of range. Year=', time_now % year, ' Month=', time_now % month, &
                 ' Day=', time_now % day, ' Day in year=', diy
      END IF
      num_fail_daterange = num_fail_daterange + 1
      daystart = RMDI
      dayend = RMDI
    END IF
  END IF ! Day is different to previous one.

  ! Get time since start of day in milliseconds
  Time = (timesi - daystart) * 1000.0

  ! Perform range checking for inputs
  IF (Time < 0 .OR. Time > seconds_in_day * 1000) THEN
    IF (mode >= VerboseMode) THEN
      num_messages = num_messages + 1
      WRITE (messages(num_messages),'(A,6(A,I0))') &
        RoutineName, '  Time is out of range ',i,&
              ' Year=', time_now % year, ' Month=', time_now % month, &
               ' Day=', time_now % day, ' Day in year=', diy, ' Time=', Time
    END IF
    num_fail_timerange = num_fail_timerange + 1
    CYCLE
  END IF

  ! check co-ordinates
  IF  (lat >= -90.0 .AND. lat <= 90.0 .AND. &
       lon >= -180.0 .AND. lon <= 180.0) THEN

    ! Position and velocity vectors
    ! Initialise variables
    ex1 = RMDI
    ey1 = RMDI
    ez1 = RMDI
    ex2 = RMDI
    ey2 = RMDI
    ez2 = RMDI
    pos_vector(:) = RMDI
    vel_vector(:) = RMDI

    ! Convert satellites 1st and 2nd latitude, longitude positions to x,y,z co-ordinates
    CALL Ops_LatLon_to_XYZ (elat(1), &
                            elon(1), &
                            ex1,     &
                            ey1,     &
                            ez1)
    CALL Ops_LatLon_to_XYZ (elat(2), &
                            elon(2), &
                            ex2,     &
                            ey2,     &
                            ez2)

    ! Satellite's position vector, pos_vector(x,y,z)
    pos_vector(1) = 0.5 * (ex1 + ex2)
    pos_vector(2) = 0.5 * (ey1 + ey2)
    pos_vector(3) = 0.5 * (ez1 + ez2)

    ! Satellite's velocity vector, vel_vector(x,y,z)
    vel_vector(1) = ex2 - ex1
    vel_vector(2) = ey2 - ey1
    vel_vector(3) = ez2 - ez1

    ! Time since start of day in seconds
    Time = Time / 1000.0

    ! Angular position in orbit
    CALL Ops_Orbit_Angle (time_now % year, & ! in
                          diy,             & ! in
                          Time,            & ! in
                          pos_vector,      & ! in
                          vel_vector,      & ! in
                          Orbit_angles(i))   ! out  Along-track angle wrt solar noon

    IF (ABS (Orbit_angles(i) - RMDI) <= RMDItol) THEN
      IF (mode >= VerboseMode) THEN
        num_messages = num_messages + 1
        WRITE (messages(num_messages), '(A,A,I0,A)') &
          RoutineName, ' SatOrbitAngle computation failed for ob ', i, &
                '. Output set to missing data.'
      END IF
      num_fail_angle = num_fail_angle + 1
    END IF

  END IF   ! lat & lon ok

END DO  ! i

! output summary of missing values and conversion failures

IF (mode >= DebugMode) THEN
  IF (num_miss_time > 0) THEN
    num_messages = num_messages + 1
    WRITE (messages(num_messages),'(A,A,I0,A)') &
    RoutineName, ' Missing time encountered for ',num_miss_time,&
               ' observations'
  END IF
  IF (num_miss_lat > 0) THEN
    num_messages = num_messages + 1
    WRITE (messages(num_messages),'(A,A,I0,A)') &
    RoutineName, ' Missing Latitude encountered for ',num_miss_lat,&
               ' observations'
  END IF
  IF (num_miss_lon > 0) THEN
    num_messages = num_messages + 1
    WRITE (messages(num_messages),'(A,A,I0,A)') &
    RoutineName, ' Missing Longitude encountered for ',num_miss_lon,&
              ' observations'
  END IF
  IF (num_miss_elat > 0) THEN
    num_messages = num_messages + 1
    WRITE (messages(num_messages),'(A,A,I0,A)') &
    RoutineName, ' Missing Ephemeris Latitude encountered for ',num_miss_elat,&
               ' observations'
  END IF
  IF (num_miss_elon > 0) THEN
    num_messages = num_messages + 1
    WRITE (messages(num_messages),'(A,A,I0,A)') &
    RoutineName, ' Missing Ephemeris Longitude encountered for ',num_miss_elon,&
              ' observations'
  END IF
  IF (num_fail_time > 0) THEN
    num_messages = num_messages + 1
    WRITE (messages(num_messages),'(A,A,I0,A)') &
    RoutineName, ' Time conversion failed for ',num_fail_time,&
              ' observations'
  END IF
  IF (num_fail_daterange > 0) THEN
    num_messages = num_messages + 1
    WRITE (messages(num_messages),'(A,A,I0,A)') &
    RoutineName, ' Date range problem for ',num_fail_daterange,&
              ' observations'
  END IF
  IF (num_fail_timerange > 0) THEN
    num_messages = num_messages + 1
    WRITE (messages(num_messages),'(A,A,I0,A)') &
    RoutineName, ' Time range problem for ',num_fail_timerange,&
              ' observations'
  END IF
  IF (num_fail_angle > 0) THEN
    num_messages = num_messages + 1
    WRITE (messages(num_messages),'(A,A,I0,A)') &
    RoutineName, ' SatOrbitAngle computation problem for ',num_fail_angle,&
              ' observations'
  END IF
END IF

!-------------------------------------------------------------------------------
! Tracing
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_SatOrbitAngle
