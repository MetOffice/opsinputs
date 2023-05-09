!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Pairing of observations for buddy check.
! Calculation of distance and angles for each pair of obs.
!
! Method:  see OSDP 2, sections 3.4 and 3.5
!
! Inputs:  see argument list
!
! Outputs: List of observation pairs, plus distance and angles for each pair
!-------------------------------------------------------------------------------

SUBROUTINE Ops_QcBdPair (ObsA,         &
                         LatitudeA,    &
                         LongitudeA,   &
                         CallsignA,    &
                         BdIndexA,     &
                         BdBandStartA, &
                         NumPairs,     &
                         PairObA,      &
                         PairObB,      &
                         PairDist,     &
                         PairRotA,     &
                         PairRotB)

USE GenMod_Control, ONLY: &
    DiagnosticMode,       & ! (PARAMETER) normal program development
    mype

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    Pi,                                  &
    Earth_Radius

USE OpsMod_ObsInfo, ONLY: &
    OB_type                   ! Structure storing observations

USE OpsMod_QC, ONLY: &
    QcMode

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(IN)   :: ObsA         ! observation data (ObsGroup A)
REAL, INTENT(IN)             :: LatitudeA(:)
REAL, INTENT(IN)             :: LongitudeA(:)
CHARACTER(len=*), INTENT(IN) :: CallsignA(:)
INTEGER, INTENT(IN)          :: BdIndexA(:)      ! pointers to sorted obs
INTEGER, INTENT(IN)          :: BdBandStartA(:)  ! first ob in each latitude band
INTEGER, INTENT(OUT)         :: NumPairs         ! number of Pairs found overall
INTEGER, INTENT(OUT)         :: PairObA(:)       ! ob from set A in pair
INTEGER, INTENT(OUT)         :: PairObB(:)       ! ob from set B in pair
REAL, INTENT(OUT)            :: PairDist(:)      ! horizontal distance (km) between obs
REAL, INTENT(OUT)            :: PairRotA(:)      ! direction of ob B from ob A
REAL, INTENT(OUT)            :: PairRotB(:)      ! reciprocal direction of ob A from B

! Local declarations:
REAL                         :: SearchRadius   ! radius used to limit search for buddies
INTEGER                      :: JABand         ! loops over latitude bands
INTEGER                      :: JASort         ! loops over observations
INTEGER                      :: JA             ! loops over observations = BdIndexA(JASort)
INTEGER                      :: JBBand         ! loops over latitude bands
INTEGER                      :: JBSort         ! loops over observations
INTEGER                      :: JB             ! loops over observations = BdIndexA(JBSort)
INTEGER                      :: JPair          ! loops over observation pairs
INTEGER                      :: IPair          ! variable used for compressing observation pairs
INTEGER                      :: NumSearchBands ! no of bands to check each side of JABand
INTEGER                      :: IBBand1         ! first band to check
INTEGER                      :: IBBand2         ! last band to check
INTEGER                      :: Ob1Band(BdNumBands)   ! first observation to check in each band
INTEGER                      :: Ob1             ! first observation to check in current band
INTEGER                      :: MaxPairTot     ! Max number of buddies overall with ob first in pair
INTEGER                      :: MaxPairRow     ! Max number of buddies per row with ob first in pair
INTEGER                      :: NumPairTot     ! number of Pairs for the current observation
INTEGER                      :: NumPairRow     ! no of Pairs in the current row of set B
INTEGER                      :: NumSameId      ! no of buddies with the same callsign
INTEGER                      :: MaxPairId      ! Max. no of buddies with the same callsign
LOGICAL                      :: FirstOb         ! first ob in band with longitude > ZLon1
REAL                         :: AKM = Earth_Radius * 0.001  ! Radius of Earth in km
REAL                         :: DistKM        ! Distance between obs in km
REAL                         :: ZLon1         ! longitude one - lowest longitude to check
REAL                         :: ZLon2         ! longitude two - highest longitude to check
REAL                         :: Rad           ! radius of circle of latitude
REAL                         :: MidBandLat    ! latitude at middle of current band
REAL                         :: SearchDLat    ! lat. difference corresponding to SearchRadius
REAL                         :: SearchDLon    ! long. difference corresponding to SearchRadius
REAL                         :: SearchDLatB   ! = SearchDLat + BdDlat/2
REAL                         :: DLat          ! difference in latitude between obs A and B
REAL                         :: DLon          ! difference in longitude between obs A and B
REAL                         :: Alpha         ! angle in tangent cone approximation
REAL                         :: Beta          ! angle in tangent cone approximation
REAL                         :: SinBeta       ! sine of Beta
CHARACTER(len=*), PARAMETER  :: RoutineName = "Ops_QcBdPair"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! 1.  Initialise routine

SearchRadius = BdRangeMax(ObsA % header % ObsGroup)
MaxPairTot = MaxBdTot(ObsA % header % ObsGroup) ! Max number of buddies overall
MaxPairRow = MaxBdRow(ObsA % header % ObsGroup) ! Max number of buddies per row
         ! Buddy checking an ObsGroup with itself MaxPairTot limits the number
         ! of pairs in which a particular observation is the first ob.
         ! There is no hard limit on the number of times that it can
         ! appear as the second ob, but in data dense areas an ob will
         ! typically occur MaxPairTot times as first ob and
         ! approx MaxPairTot times as second ob.
MaxPairId = MaxSameId(ObsA % header % ObsGroup)
         ! MaxPairId is the number of times that an ob can have
         ! another with the same callsign as the second ob of the pair.

IF (QcMode >= DiagnosticMode .AND. mype == 0) THEN
  WRITE (MessageOut, '(/4A/A,F7.2,A,F7.2,A,I2,A,I2,A,I2)') &
    RoutineName, ' ObsGroups: ', &
    ObsA % header % ObsGroupName, ObsA % header % ObsGroupName, &
    ' BdDlat = ', BdDlat, '  SearchRadius = ', SearchRadius, &
    ' MaxPairTot = ', MaxPairTot, '  MaxPairRow = ', MaxPairRow, &
    ' MaxPairId = ', MaxPairId
END IF

SearchDLat = (180.0 / Pi) * SearchRadius / AKM         ! eqn 3.1
SearchDLatB = SearchDLat + 0.5 * BdDlat
NumSearchBands = INT (SearchDLat / BdDlat) + 1
NumPairs = 0

PairObA(:) = 0
PairObB(:) = 0
PairDist(:) = 0
! 2.  Find close pairs of observations
DO JABand = 1, BdNumBands

  MidBandLat = 90.0 - BdDlat * (JABand - 0.5)
  Rad = AKM * COS ((ABS (MidBandLat) + BdDlat * 0.5) * Pi / 180.0) ! eqn 3.2a
  IF (Rad <= 10.0) THEN
    SearchDLon = 360.0   ! Adjacent to pole
  ELSE
    SearchDLon = (180.0 / Pi) * SearchRadius / Rad                      ! eqn 3.2b
  END IF

  DO JBBand = 1, BdNumBands
    Ob1Band(JBBand) = BdBandStartA(JBBand)
  END DO
  IBBand1 = MAX (1, JABand - NumSearchBands)
  IBBand2 = MIN (BdNumBands, JABand + NumSearchBands)
  IBBand1 = JABand

  ! Loop through set A observations in JABand
  JAloop: DO JASort = BdBandStartA(JABand),BdBandStartA(JABand + 1) - 1
    JA = BdIndexA(JASort)
    ZLon1 = LongitudeA(JA) - SearchDLon
    ZLon2 = LongitudeA(JA) + SearchDLon
    NumPairTot = 0
    NumSameId = 0

    ! Usually JBBand loops over JABand-1 (if A /= B), JABand and JABand+1
    BANDloop: DO JBBand = IBBand1, IBBand2
      MidBandLat = 90.0 - BdDlat * (JBBand - 0.5)
      IF (ABS (LatitudeA(JA) - MidBandLat) > SearchDLatB) CYCLE BANDloop
      Ob1 = Ob1Band(JBBand)
      IF (JBBand == JABand) Ob1 = JASort + 1
      NumPairRow = 0
      FirstOb = .TRUE.

      ! Loop through set B observations in JBBand
      JBloop1: DO JBSort = Ob1, BdBandStartA(JBBand + 1) - 1
        JB = BdIndexA(JBSort)
        IF (LongitudeA(JB) > ZLon2) CYCLE BANDloop  ! next band in B
        IF (LongitudeA(JB) < ZLon1) CYCLE JBloop1   ! next ob B
        IF (FirstOb) Ob1Band(JBBand) = JBSort
        FirstOb = .FALSE.
        IF (CallsignA(JA) == CallsignA(JB)) THEN
          IF (NumSameId >= MaxPairId) CYCLE JBloop1 ! next ob B
          NumSameId = NumSameId + 1
        END IF
        IF (ABS(LatitudeA(JA) - LatitudeA(JB)) <= SearchDLat) THEN
          NumPairs = NumPairs + 1
          NumPairTot = NumPairTot + 1
          NumPairRow = NumPairRow + 1
          PairObA(NumPairs) = JA
          PairObB(NumPairs) = JB
          IF (NumPairTot >= MaxPairTot) CYCLE JAloop    ! next observation A
          IF (NumPairRow >= MaxPairRow) CYCLE BANDloop  ! next band in B
        END IF
      END DO JBloop1

      ! If observation A is near end of band (+180) then check
      ! the start of the band also (wrap around)
      IF (ZLon2 <= 180.0) CYCLE BANDloop    ! next band in B
      IF (SearchDLon >= 180.0 .AND. JABand == JBBand) CYCLE BANDloop  ! next band in B
      ZLon2 = ZLon2 - 360.0
      JBloop2: DO JBSort = BdBandStartA(JBBand), BdBandStartA(JBBand + 1) - 1
        JB = BdIndexA(JBSort)
        IF (LongitudeA(JB) > ZLon2) CYCLE BANDloop  ! next band in B
        IF (CallsignA(JA) == CallsignA(JB)) THEN
          IF (NumSameId >= MaxPairId) CYCLE JBloop2 ! next ob B
          NumSameId = NumSameId + 1
        END IF
        IF (ABS (LatitudeA(JA) - LatitudeA(JB)) <= SearchDLat) THEN
          NumPairs = NumPairs + 1
          NumPairTot = NumPairTot + 1
          PairObA(NumPairs) = JA
          PairObB(NumPairs) = JB
          IF (NumPairTot >= MaxPairTot) CYCLE JAloop    ! next observation A
          NumPairRow = NumPairRow + 1
          IF (NumPairRow >= MaxPairRow) CYCLE BANDloop  ! next band in B
        END IF
      END DO JBloop2  ! end of secondary loop over observations (JB)
    END DO BANDloop   ! end of secondary loop over bands (JBBand)

  END DO JAloop  ! end of main loop over observations (JA)
END DO           ! end of main loop over bands (JABand)

! 3.  Calculate horizontal distance between the two obs for each pair
!     discard if > SearchRadius, otherwise calculate bearing of A from B
!     and reciprocal bearing of B from A

IPair = 0
DO JPair = 1, NumPairs
  JA = PairObA(JPair)
  JB = PairObB(JPair)

  DLat = (LatitudeA(JB) - LatitudeA(JA)) * Pi / 180.0
  DLon = (LongitudeA(JB) - LongitudeA(JA)) * Pi / 180.0
  DistKM  = AKM * SQRT (DLat ** 2 + 4.0 * SIN (DLon * 0.5) ** 2 &
                       * COS (LatitudeA(JA) * Pi / 180.0) &
                       * COS (LatitudeA(JB) * Pi / 180.0))    ! eqn 3.3

  IF (DistKM > SearchRadius) CYCLE  ! omit this pair, move to next
  IPair = IPair + 1
  PairObA(IPair) = PairObA(JPair)
  PairObB(IPair) = PairObB(JPair)
  PairDist(IPair) = DistKM

  IF (DistKM < 10.0) THEN
    PairRotA(IPair) = 0.0      !  the transformation is undefined
    PairRotB(IPair) = 0.0      !  for DistKM = 0.0, use u,v components
  ELSE
    Alpha = 0.5 * SIN (LatitudeA(JA) * Pi / 180.0) * DLon            ! eqn 3.5
    SinBeta = AKM * DLat * COS (Alpha) / DistKM                      ! eqn 3.6
    SinBeta = MIN (1.0,MAX (-1.0,SinBeta))
    Beta = ASIN (SinBeta)
    PairRotA(IPair) = Alpha + Beta                            ! eqn 3.7
    PairRotB(IPair) = Beta - Alpha                            ! eqn 3.8
  END IF
END DO       ! end of loop over pairs
NumPairs = IPair

IF (QcMode >= DiagnosticMode .AND. mype == 0) THEN
  WRITE (MessageOut, '(/,A,I0,A/)') RoutineName // ' finishing. ', NumPairs, ' pairs.'
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_QcBdPair
