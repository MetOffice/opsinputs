!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Select and sort observations for duplicate check.  Sorts observations by 1.5
! degree latitude band and longitudeLoops through bands/observations (ObA),
! loops through obs in current/next band that might be close to ObA Flag ObB as
! SurplusReport if it is close to ObA (ie within SearchDLat, SearchDLon,
! SearchDZ and SearchDTime of ObA).  For simplicity no checking is done across
! +/- 180 longitude.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ThinObs4D (ObsName,     &
                          NumObs,      &
                          Callsign,    &
                          Latitude,    &
                          Longitude,   &
                          Zcoord,      &
                          Time,        &
                          SearchDLat,  &
                          SearchDLon,  &
                          SearchDZ,    &
                          SearchDTime, &
                          Preference)

USE GenMod_Control, ONLY: &
    GeneralMode,          &
    VerboseMode,          &
    DiagnosticMode,       &
    mype

USE GenMod_Core, ONLY: &
    gen_message,       &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE OpsMod_Sort, ONLY: &
    Ops_InitSortMask,  &
    Ops_IntegerSort,   &
    Ops_RealSort

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)    :: ObsName         ! Observation type
INTEGER, INTENT(IN)             :: NumObs          ! Number of observations
CHARACTER(len=*), INTENT(IN)    :: Callsign(:)     ! Observation callsigns (for listing)
REAL, INTENT(IN)                :: Latitude(:)     ! Latitude
REAL, INTENT(IN)                :: Longitude(:)    ! Longitude
REAL, INTENT(IN)                :: Zcoord(:)       ! Vertical coordinate
REAL, INTENT(IN)                :: Time(:)         ! Time
REAL, INTENT(IN)                :: SearchDLat      ! Tolerance in latitude
REAL, INTENT(IN)                :: SearchDLon      ! Tolerance in longitude
REAL, INTENT(IN)                :: SearchDZ        ! Tolerance in vertical coord
REAL, INTENT(IN)                :: SearchDTime     ! Tolerance in time
INTEGER, INTENT(INOUT)          :: Preference(:)   ! Search preference

! Local declarations:
INTEGER                         :: J               ! loops over observations
INTEGER                         :: JSort           ! loops over sorted observations
INTEGER                         :: JBand           ! loops over bands
INTEGER                         :: LBand           ! previous band
INTEGER                         :: SortNumObs      ! number of sorted observations
INTEGER                         :: NumCDups        ! number of close duplicates
INTEGER                         :: KeepA
INTEGER                         :: KeepB
REAL, PARAMETER                 :: DupDLat  = 1.5  ! Width of latitude band (degrees)
INTEGER, PARAMETER              :: NumBands = 120  ! =180.0/DupDLat
LOGICAL                         :: Mask(NumObs)    ! Mask for sorting
INTEGER                         :: IBand(NumObs)   ! Band number
INTEGER                         :: GrpInfo(NumObs) ! Used for sorting
INTEGER                         :: Index(NumObs)   ! Pointers to sorted obs
INTEGER                         :: BandStart(NumBands + 1) ! first ob in each latitude Band
INTEGER                         :: Ob1Band(NumBands) ! use to start next search
INTEGER                         :: Ob1             ! use to start next search
INTEGER                         :: JASort          ! loops over sorted observations
INTEGER                         :: JABand          ! loops over bands
INTEGER                         :: JBSort          ! loops over sorted observations
INTEGER                         :: JBBand          ! loops over bands
INTEGER                         :: JA              ! index of first ob in pair
INTEGER                         :: JB              ! index of second ob in pair
LOGICAL                         :: FirstOb         ! first ob in band > ZLon1
REAL                            :: ZLon1           ! minimum longitude to match current ob
REAL                            :: ZLon2           ! maximum longitude to match current ob
CHARACTER(len=*), PARAMETER     :: RoutineName = "Ops_ThinObs4D"
CHARACTER(len=120)              :: Message(3)
CHARACTER(len=200), ALLOCATABLE :: message_strings(:)
INTEGER                         :: message_num

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! 1.  Loop through observations, calculating band and setting mask

iband = 0
DO J = 1, NumObs
  ! Use 89.999 to round up values  eg ranges are 60.0 to 61.499
  IBand(J) = MAX (1, INT ((89.999 - Latitude(J)) / DupDlat) + 1)

  Mask(J) = .TRUE.
  IF (Preference(J) <= 0) Mask(J) = .FALSE.  ! Omit observations as required
  IF (Latitude(J) == RMDI) Mask(J) = .FALSE.
END DO

! 2. Sort the selected observations (set up index)

! Initialise the index and group information arrays.
SortNumObs = NumObs
CALL Ops_InitSortMask (Index,      &
                       GrpInfo,    &
                       SortNumObs, &
                       Mask)

IF (SortNumObs > 1) THEN
  ! sort on each key in turn, starting with the most significant.
  CALL Ops_IntegerSort (IBand(:),   &
                        SortNumObs, &
                        Index,      &
                        GrpInfo)
  CALL Ops_RealSort (Longitude(:), &
                     SortNumObs,   &
                     Index,        &
                     GrpInfo)
ELSE IF (SortNumObs == 1) THEN
  Index(1) = 1
END IF

! 3. Set BandStart array

LBand = 0
DO JSort = 1, SortNumObs
  J = Index(JSort)
  DO JBand = LBand + 1, IBand(J)
    BandStart(JBand) = JSort
  END DO
  LBand = IBand(J)
END DO

DO JBand = LBand + 1, NumBands + 1
  BandStart(JBand) = SortNumObs + 1
END DO

! 2.  Find close pairs of observations
NumCDups = 0
KeepA = 0
KeepB = 0

IF (GeneralMode >= VerboseMode .AND. mype == 0) THEN
  ALLOCATE (message_strings(NumObs))
  message_num = 1
END IF

DO JABand = 1, NumBands

  DO JBBand = 1, NumBands
    Ob1Band(JBBand) = BandStart(JBBand)
  END DO

  ! Loop through set A observations in JABand
  JAloop: DO JASort = BandStart(JABand), BandStart(JABand + 1) - 1
    JA = Index(JASort)
    ZLon1 = Longitude(JA) - SearchDLon
    ZLon2 = Longitude(JA) + SearchDLon
    IF (Preference(JA) <= 0) CYCLE JAloop

    BANDloop: DO JBBand = JABand, MIN (NumBands,JABand + 1)
      Ob1 = Ob1Band(JBBand)
      IF (JBBand == JABand) Ob1 = JASort + 1
      FirstOb = .TRUE.

      ! Loop through set B observations in JBBand
      JBloop1: DO JBSort = Ob1, BandStart(JBBand + 1) - 1
        JB = Index(JBSort)
        IF (Longitude(JB) > ZLon2) CYCLE BANDloop  ! next band in B
        IF (Longitude(JB) < ZLon1) CYCLE JBloop1   ! next ob B
        IF (Preference(JB) <= 0) CYCLE JBloop1   ! next ob B
        IF (FirstOb) Ob1Band(JBBand) = JBSort
        FirstOb = .FALSE.
        IF (ABS (Latitude(JA) - Latitude(JB)) <= SearchDLat .AND. &
            ABS (Longitude(JA) - Longitude(JB)) <= SearchDLon .AND. &
            ABS (Time(JA) - Time(JB)) <= SearchDTime .AND. &
            ABS (Zcoord(JA) - Zcoord(JB)) <= SearchDZ) THEN
          NumCDups = NumCDups + 1

          IF (GeneralMode >= VerboseMode .AND. mype == 0) THEN
            WRITE (message_strings(message_num), '(2(I6,2X,A10,2X,I6,2F7.2,I6,2X,F8.1))') &
              JA, TRIM (Callsign(JA)), Preference(JA), &
              Latitude(JA), Longitude(JA), &
              NINT (Time(JA)) / 60, Zcoord(JA), &
              JB, Callsign(JB), Preference(JB), &
              Latitude(JB), Longitude(JB), &
              NINT (Time(JB)) / 60, Zcoord(JB)
            message_num = message_num + 1
          END IF

          IF (Preference(JA) < Preference(JB)) THEN
            Preference(JA) = -1
            KeepB = KeepB + 1
            CYCLE JAloop
          ELSE
            Preference(JB) = -1
            KeepA = KeepA + 1
          END IF
        END IF
      END DO JBloop1

    END DO BANDloop   ! end of secondary loop over bands (JBBand)

  END DO JAloop  ! end of main loop over observations (JA)
END DO           ! end of main loop over bands (JABand)

IF (GeneralMode >= VerboseMode .AND. mype == 0) THEN
  CALL gen_message (RoutineName,                        &
                    message_strings(1:message_num - 1))
  DEALLOCATE (message_strings)
END IF

IF (GeneralMode >= DiagnosticMode) THEN
  WRITE (Message(1), '(I0,1X,A,I0,A)') NumObs, TRIM (ObsName) // ' obs input ',SortNumObs,' sorted '
  WRITE (Message(2), '(I0,A,I0,1X,I0)') NumCDups,' surplus obs found.  KeepA, KeepB  = ', KeepA, KeepB
  WRITE (Message(3), '(A,4F9.2)') 'DLat, DLon, DZ, DTime: ', SearchDLat, SearchDLon, SearchDZ, SearchDTime
  CALL gen_message (RoutineName,  &
                    Message(1:3))
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ThinObs4D
