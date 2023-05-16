!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Compute solar zenith and azimuth angles for an array of latitudes and
! longitudes, for a given time and date.
!
! Air Almanac useful for checking GHA and DECL
! Norton's Star Atlas for equation of time
! Robinson N. Solar Radiation Ch 2 for useful introduction to
! theory/terminology.
!
! If input values are out of range, angles of zero will be returned.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_Solar_Zenith (Obs,                & ! Longitudes of locations in Deg. E.
                             Zeniths,            & ! Solar Zeniths in degrees
                             ignore_out_of_area)

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
    RMDI,                                & ! Real Missing data indicator.
    PI                                     ! 3.1415926....

USE OpsMod_Constants, ONLY: &
    deg2rad,                &
    rad2deg

USE OpsMod_DateTime, ONLY:   &
    DateTime_type,           &
    minutes_in_day,          &
    OpsFn_DateTime_Create,   &
    OpsFn_DateTime_ToMinUTC, &
    seconds_in_day,          &
    seconds_in_hour

USE OpsMod_ObsInfo, ONLY: &
    ob_type,              &
    OutOfAreaReport

USE OpsMod_Process, ONLY: &
    ProcessMode

IMPLICIT NONE

! Subroutine arguments:
TYPE (ob_type), INTENT(IN)      :: Obs
REAL, INTENT(OUT)               :: Zeniths(:)             ! local solar zenith angles, degrees
LOGICAL, OPTIONAL, INTENT(IN)   :: ignore_out_of_area

! Local declarations:
REAL, PARAMETER                 :: Pi_Over_2 = PI / 2.0
REAL, PARAMETER                 :: secs2hours = 1.0 / seconds_in_hour ! convert seconds to hours
REAL, PARAMETER                 :: msecs2hours = 1.0 / (1000 * seconds_in_hour) ! convert milli seconds to hours
REAL, PARAMETER                 :: day2century = 1.0 / 36525.0        ! convert days to centuries.
REAL, PARAMETER                 :: one_over_360 = 1.0 / 360.0
REAL, PARAMETER                 :: DegLon2Hour = 1.0 / 15.0           ! convert degrees longitude to time offset.
REAL, PARAMETER                 :: Sec2Min = 1.0 / 60.0               ! Convert seconds to minutes.
REAL, PARAMETER                 :: mins2days = 1.0 / minutes_in_day   ! Convert minutes to days.
CHARACTER(len=*), PARAMETER     :: RoutineName = "Ops_Solar_Zenith"
INTEGER                         :: Diy                   ! Day in year (1-366)
INTEGER                         :: Time                  ! Time GMT (millisecs from midnight)
INTEGER                         :: centday               ! century day
INTEGER                         :: i                     ! loop through spots 1 to NLocations
REAL                            :: ang                   ! local hour angle
REAL                            :: decl                  ! solar declination (radians)
REAL                            :: eqnt                  ! "equation of time" (seconds)
REAL                            :: hourf                 ! hour (+fraction) of day
REAL                            :: lat                   ! latitude
REAL                            :: lon                   ! longitude
REAL                            :: timesi                ! time offset for ith Ob.
REAL                            :: localtime             ! local solar time (hrs)
REAL                            :: rcd                   ! fraction of days elapsed this century
REAL                            :: rcd2                  ! "rcd" squared
REAL                            :: rcosdc                ! cosine of solar declination
REAL                            :: rsindc                ! sine of solar declination
REAL                            :: rsinev                ! sine of elevation angle
REAL                            :: rcoslt                ! cosine of latitude
REAL                            :: rsinlt                ! sine of latitude
REAL                            :: sinalp
REAL                            :: taneqn
REAL                            :: ydeg
REAL                            :: yrad
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
INTEGER                         :: num_fail_time
INTEGER                         :: num_fail_daterange
INTEGER                         :: num_fail_timerange
INTEGER                         :: NLocations             ! number of points in arrays
LOGICAL                         :: ignore_out_of_area_local
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
num_fail_time = 0
num_fail_daterange = 0
num_fail_timerange = 0

IF (PRESENT (ignore_out_of_area)) THEN
  ignore_out_of_area_local = ignore_out_of_area
ELSE
  ignore_out_of_area_local = .TRUE.
END IF

!-------------------------------------------------------------------------------
! Initialise output as missing data
!-------------------------------------------------------------------------------
Zeniths(1:NLocations) = RMDI

!-------------------------------------------------------------------------------
! Convert reference time to minutes UTC.
!-------------------------------------------------------------------------------
MinUTCRef = OpsFn_DateTime_ToMinUTC (Obs % Header % ValidityTime)
secsRef = Obs % Header % ValidityTime % second

IF (ProcessMode >= VerboseMode) THEN
  ALLOCATE (messages(7 * NLocations + 6))
ELSE IF (ProcessMode >= DebugMode) THEN
  ALLOCATE (messages(6))
END IF

num_messages = 0

! Loop through spots
DO i = 1, NLocations
  lat = Obs % Latitude(i)
  lon = Obs % Longitude(i)
  timesi = Obs % Time(i)

  ! If coordinates are not valid then do not process further.
  IF (ABS (timesi - RMDI) <= RMDItol) THEN
    IF (ProcessMode >= VerboseMode) THEN
      num_messages = num_messages + 1
      WRITE (messages(num_messages), '(A,A,I0,A)') &
        RoutineName, ' Missing time encountered for ob ',i,&
              '. Output set to missing data.'
    END IF
    num_miss_time = num_miss_time + 1
    CYCLE
  END IF

  IF (ignore_out_of_area_local) THEN
    IF (BTEST (Obs % ReportFlags(i), OutOfAreaReport)) THEN
      IF (ProcessMode >= VerboseMode) THEN
        num_messages = num_messages + 1
        WRITE (messages(num_messages), '(A,A,I0,A)') &
          RoutineName, ' Missing time encountered for ob ',i,&
                '. Output set to missing data.'
      END IF
      num_miss_time = num_miss_time + 1
      CYCLE
    END IF
  END IF

  IF (ABS (lat - RMDI) <= RMDItol) THEN
    IF (ProcessMode >= VerboseMode) THEN
      num_messages = num_messages + 1
      WRITE (messages(num_messages), '(A,A,I0,A)') &
        RoutineName, ' Missing Latitude encountered for ob ',i,&
              '. Output set to missing data.'
    END IF
    num_miss_lat = num_miss_lat + 1
    CYCLE
  END IF

  IF (ABS (lon - RMDI) <= RMDItol) THEN
    IF (ProcessMode >= VerboseMode) THEN
      num_messages = num_messages + 1
      WRITE (messages(num_messages), '(A,A,I0,A)') &
        RoutineName, ' Missing Longitude encountered for ob ',i,&
              '. Output set to missing data.'
    END IF
    num_miss_lon = num_miss_lon + 1
    CYCLE
  END IF

  ! If this day is different to previous, then recompute time data.
  IF (timesi < daystart .OR. timesi >= dayend) THEN

    !   Find current year, month & day by adding time to reference.

    MinUTCNow = MinUTCRef + FLOOR ((secsRef + timesi) * sec2Min)
    time_now = OpsFn_DateTime_Create (MinUTCNow)

    IF (time_now % year == IMDI) THEN
      IF (ProcessMode >= VerboseMode) THEN
        num_messages = num_messages + 1
        WRITE (messages(num_messages), '(A,A,I0,A)') &
          RoutineName, ' Obs time conversion failed for ob ', i, &
                '. Output set to missing data.'
      END IF
      num_fail_time = num_fail_time + 1
    END IF

    secsNow = timesi + secsRef + (MinUTCRef - MinUTCNow) * 60.0
    !
    ! Get the time offset in seconds corresponding to the start of the
    ! day that we are in now.
    !
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

      !     Century day (ie no. of days since 0-JAN-1900) - 2000 is a leap year
      !     Integer division should be enforced here.
      centday = (time_now % year - 1900) * 365 + Diy + (time_now % year - 1901) / 4

      rcd = REAL (centday) * day2century ! Fraction of days elapsed this century
      rcd2 = rcd * rcd
      ydeg = (rcd * 36000.769 + 279.697) * one_over_360
      ydeg = MOD (ydeg,1.0) * 360.0
      yrad = ydeg * deg2rad

      !   Compute equation of time (in seconds) for this day
      !   (No reference for this but it gives the correct answers
      !   when compared with table in Norton's Star Atlas)

      eqnt = - (( 93.0 + 14.23 * rcd - 0.0144 * rcd2) * SIN (yrad))      &
             - ((432.5 - 3.71 * rcd - 0.2063 * rcd2) * COS (yrad))       &
             + ((596.9 - 0.81 * rcd - 0.0096 * rcd2) * SIN (2.0 * yrad)) &
             - ((  1.4 + 0.28 * rcd) * COS (2.0 * yrad))                 &
             + ((  3.8 + 0.6 * rcd) * SIN (3.0 * yrad))                  &
             + (( 19.5 - 0.21 * rcd - 0.0103 * rcd2) * COS (3.0 * yrad)) &
             - (( 12.8 - 0.03 * rcd) * SIN (4.0 * yrad))

      !   Get solar declination for given day (radians)
      sinalp = SIN ((ydeg - eqnt / 240.0) * deg2rad)
      taneqn = 0.43382 - 0.00027 * rcd
      decl = ATAN (taneqn * sinalp)
      eqnt = eqnt * secs2hours                    ! Convert to hours

      !   Sine and cosine of declination
      rsindc = SIN (decl)
      rcosdc = COS (decl)

    ELSE
      IF (ProcessMode >= VerboseMode) THEN
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
    IF (ProcessMode >= VerboseMode) THEN
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

    rsinlt = SIN (lat * deg2rad)
    rcoslt = COS (lat * deg2rad)
    hourf = REAL (Time) * msecs2hours  ! Convert msecs elapsed to GMT
    !   Calculate solar zenith (degrees)
    localtime = lon * DegLon2Hour + eqnt + hourf   ! Local solar time (hrs)
    ang = (localtime * 15.0 + 180.0) * deg2rad  ! Local hour angle
    !            (when longitude is 0 this is the GHA given in the Air Almanac)
    rsinev = rsindc * rsinlt + rcosdc * rcoslt * COS (ang)
    Zeniths(i) = (PI_over_2 - ASIN (rsinev)) * rad2deg

  END IF   ! lat & lon ok

END DO  ! i

! output summary of missing values and conversion failures

IF (ProcessMode >= DebugMode) THEN
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
END IF

!-------------------------------------------------------------------------------
! Tracing
!-------------------------------------------------------------------------------
IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_Solar_Zenith
