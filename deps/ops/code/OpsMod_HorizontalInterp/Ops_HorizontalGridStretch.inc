!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! This routine finds the weight and grid indices needed to interpolate a field
! defined on a grid with variable latitude and longitude spacing to a set of
! observation points.  It uses the Row dependent constants and Column dependent
! constants in the UMHeader.  The obs latitudes and longitudes are then located
! and a weight is computed so that the observation can be interpolated to the
! nearest grid point west and south of the obs location (grid point north of obs
! location in atmospheric grid)
!
! This routine is a modified version of Stuart Bell's routine 3 in the
! subroutine Var_HorizontalInterp.inc.  It has been set up as an alternative
! routine to compute the weights of the obs points on a stretched grid.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_HorizontalGridStretch (PointsInRow,  &
                                      PointsInCol,  &
                                      Len1Out,      &
                                      LambdaOrigin, &
                                      PhiOrigin,    &
                                      UMHeader,     &
                                      LambdaOut,    &
                                      PhiOut,       &
                                      IOut,         &
                                      JOut,         &
                                      WtPhi,        &
                                      WtLambda)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    MessageOut,        &
    UseTrace

USE GenMod_MiscUMScienceConstants,ONLY: &
    Pi

USE GenMod_ModelIO, ONLY: &
    UM_header_type

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)               :: PointsInRow  ! # points in model row
INTEGER, INTENT(IN)               :: PointsInCol  ! column
INTEGER, INTENT(IN)               :: Len1Out      ! Dimension of output in i direction.
REAL, INTENT(IN)                  :: LambdaOrigin ! longitude origin in radians
REAL, INTENT(IN)                  :: PhiOrigin    ! latitude origin in radians
TYPE (UM_header_type), INTENT(IN) :: UMHeader
REAL, INTENT(IN)                  :: LambdaOut(:) ! obs longitudes
REAL, INTENT(IN)                  :: PhiOut(:) ! obs latitudes
INTEGER, INTENT(OUT)              :: IOut(:) ! Point such that
INTEGER, INTENT(OUT)              :: JOut(:) ! the desired output point
REAL, INTENT(OUT)                 :: WtPhi(:)    ! A number between 0 & 1
REAL, INTENT(OUT)                 :: WtLambda(:) ! A number between 0 & 1

! Local declarations:
CHARACTER(len=*), PARAMETER       :: RoutineName = "Ops_HorizontalGridStretch"
INTEGER                           :: I
INTEGER                           :: J
INTEGER                           :: K! loop counts
REAL                              :: dLambdaStretch(PointsInRow) ! Spacing between points in i direction for the input field
REAL                              :: dPhiStretch(PointsInCol)    ! spacing between points in j direction for the input field
REAL                              :: RecipdLambdaStretch(PointsInRow) ! 1/DLambdaStretch
REAL                              :: RecipdPhiStretch(PointsInCol)    ! 1/DPhiStretch
REAL                              :: grid_lambda(PointsInRow)         ! stretched grid model longitudes
REAL                              :: grid_phi(PointsInCol)            ! stretched grid model latitudes
REAL                              :: LambdaGTOrigin(Len1Out)  ! local variable to make sure LambdaOut

! ----------------------------------------------------------------------
!  Section 1.   Initialize
! ----------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!   [2] Set derived variables
WRITE (MessageOut, '(A,I0)') 'PointsInRow = ',PointsInRow
DO i = 1, PointsInRow
  dLambdaStretch(i) = UMHeader % ColDepC(i) * pi / 180.0
END DO

DO j = 1, PointsInCol
  dPhiStretch(j) = UMHeader % RowDepC(j) * pi / 180.0
END DO

RecipdLambdaStretch(:) = 1.0 / dLambdaStretch(:)

RecipdPhiStretch(:) = 1.0 / dPhiStretch(:)

! ----------------------------------------------------------------------
!  Section 2.   Find variably spaced grid longitudes and latitudes
! ----------------------------------------------------------------------

grid_lambda(1) = LambdaOrigin
DO i = 2, PointsInRow
  grid_lambda(i) = grid_lambda(i - 1) + &
  UMHeader % ColDepC(i) * Pi / 180.0
END DO

grid_phi(1) = PhiOrigin
DO j = 2, PointsInCol
  grid_phi(j) = grid_phi(j - 1) + &
  UMHeader % RowDepC(j) * Pi / 180.0
END DO

! ----------------------------------------------------------------------
!  Section 3.   Find Iout and LamdbdaOut
! ----------------------------------------------------------------------

! grid_lambda must be monotonic ; which it is by construction
! grid_phi might be going north or south

! loop over columns to find IOut ; then find WtLamdbda

! Check LambdaGTOrigin is within grid values (ie is greater than LambdaOrigin)

WHERE (LambdaOut(:) < LambdaOrigin)
  LambdaGTOrigin(:) = LambdaOut(:) + 2 * Pi
ELSEWHERE
  LambdaGTOrigin(:) = LambdaOut(:)
ENDWHERE

DO k = 1, Len1Out
  IOut(k) = 1
  DO i = 2, PointsInRow - 1
    IF (LambdaGTOrigin(k) >= grid_lambda(i)) IOut(k) = i
  END DO

  WtLambda(k) = (LambdaGTOrigin(k) - grid_lambda(Iout(k))) &
     * RecipDLambdaStretch(Iout(k) + 1)

END DO

! ----------------------------------------------------------------------
!  Section 4.   Find Jout and PhiOut
! ----------------------------------------------------------------------

! loop over rows to find JOut; then find WtPhi
! Check the grid type

IF (dPhiStretch(1) > 0.0) then
  ! Phi increases (South to North grid: Ocean or New Dynamics grid)
  DO k = 1, Len1Out
    JOut(k) = 1
    DO j = 2, PointsInCol - 1
      IF (PhiOut(k) >= grid_phi(j)) JOut(k) = j
    END DO
    WtPhi(k) = (PhiOut(k) - grid_phi(Jout(k))) &
       * RecipdPhiStretch(Jout(k) + 1)
  END DO

ELSE
  ! Atmosphere Old Dynamics Grid (North to South grid)
  DO k = 1, Len1Out
    JOut(k) = 1
    DO j = 2, PointsInCol - 1
      IF (PhiOut(k) <= grid_phi(j)) JOut(k) = j
    END DO
    WtPhi(k) = (PhiOut(k) - grid_phi(Jout(k))) &
       * RecipdPhiStretch(Jout(k) + 1)
  END DO
END IF    ! sign of dPhiStretch

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_HorizontalGridStretch
