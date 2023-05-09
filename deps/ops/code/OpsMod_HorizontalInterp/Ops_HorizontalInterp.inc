!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Horizontal interpolation.  Interpolates by linear interpolation, a 2-d field
! to a 2-d set of points.  Input data can be on a sphere or within a rectangular
! box. Requested output data points must lie within the region defined for the
! input data.
!
! i=x=lambda, j=y=phi
! assumption that nth point defined by:
!        (LambdaOrigin+(n-1)*DLambdaIn , PhiOrigin+(n-1)*DPhiIn)
! assumption that input and output in radians in range
!        (0>2pi , -pi/2>+pi/2)
! assumption that check for output positions outside input grid domain
!         is done externally
!
! This routine is a modified version of the routine interpolation written by
! Mark Mawson and described in:
!           The proposed semi-Lagrangian advection scheme for the
!              semi-Implicit Unified Model integration scheme.
!                    F.R. Division working paper No 162.
!                             Mark H. Mawson
!-------------------------------------------------------------------------------

SUBROUTINE Ops_HorizontalInterp (domain,       &
                                 LambdaOrigin, &
                                 PhiOrigin,    &
                                 DLambdaIn,    &
                                 DPhiIn,       &
                                 LambdaOut,    &
                                 PhiOut,       &
                                 DataIn,       &
                                 DataOut,      &
                                 StashCode,    &
                                 interp_type)

USE GenMod_Core, ONLY: &
    gen_fail,          &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    Pi

USE GenMod_UMHeaderConstants, ONLY: &
    FH_ColDepCSize1,                &
    FH_RowDepCSize1,                &
    FH_SubModel,                    &
    FH_SubModel_Ocean,              &
    IC_XLen,                        &
    IC_YLen

USE OpsMod_ModelIO, ONLY: &
    DomainInfo_type

USE OpsMod_Stash, ONLY:    &
    StashItem_u,           &
    StashItem_u_unslanted, &
    StashItem_v,           &
    StashItem_v_unslanted, &
    StashCode_u10,         &
    StashCode_v10

USE OpsMod_Kinds, ONLY : &
    real32

IMPLICIT NONE

! Subroutine arguments:
TYPE (DomainInfo_type), INTENT(IN) :: domain       ! used for stretched grid
REAL, INTENT(IN)                   :: LambdaOrigin ! Position of 1st point in the i direction for the input field.
REAL, INTENT(IN)                   :: PhiOrigin    ! Position of 1st point in the j direction for the input field.
REAL, INTENT(IN)                   :: DLambdaIn    ! Spacing between points in the i direction for the input field.
REAL, INTENT(IN)                   :: DPhiIn       ! Spacing between points in the j direction for the input field.
REAL, INTENT(IN)                   :: LambdaOut(:) ! Lambda coordinate of output data on input grid
REAL, INTENT(IN)                   :: PhiOut(:)    ! Phi coordinate of output data on input grid
REAL(real32), INTENT(IN)           :: DataIn(:,:)  ! Data to be interpolated
REAL, INTENT(OUT)                  :: DataOut(:)   ! Data interpolated to desired locations.
INTEGER, INTENT(IN)                :: StashCode
INTEGER, OPTIONAL, INTENT(IN)      :: interp_type

! Local declarations:
CHARACTER(len=*), PARAMETER        :: RoutineName = "Ops_HorizontalInterp"
LOGICAL                            :: LStretch     ! True, if stretched grid
INTEGER                            :: Len1Out      ! Dimension of output in i direction.
INTEGER                            :: LowerBound   ! Lower Bound of DataExt in both directions
INTEGER                            :: PointsInCol  ! points in model column
INTEGER                            :: PointsInRow  ! points in model row
REAL                               :: RecipDLambdaIn  ! 1/DLambdaIn
REAL                               :: RecipDPhiIn     ! 1/DPhiIn
INTEGER, ALLOCATABLE               :: IOut(:)
INTEGER, ALLOCATABLE               :: JOut(:)
REAL, ALLOCATABLE                  :: WtLambda(:)
REAL, ALLOCATABLE                  :: WtPhi(:)
REAL, POINTER                      :: Lambda(:)
REAL, POINTER                      :: Phi(:)
INTEGER                            :: interp_type_local
CHARACTER(len=100)                 :: message

! ----------------------------------------------------------------------
!  Section 1.   Initialize
! ----------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (PRESENT (interp_type)) THEN
  interp_type_local = interp_type
ELSE
  interp_type_local = interp_type_linear_coast
END IF

!   [0] Set derived variables
IF (DLambdaIn /= 0.0) RecipDLambdaIn = 1.0 / DLambdaIn
IF (DPhiIn /= 0.0) RecipDPhiIn = 1.0 / DPhiIn

!   [1.3]  Get lower bound for DataExt

Len1Out = SIZE (DataOut)
LowerBound = 1 - num_extend_points

!   [1.4]  Allocate space for work arrays
ALLOCATE (IOut(Len1Out))
ALLOCATE (JOut(Len1Out))
ALLOCATE (WtLambda(Len1Out))
ALLOCATE (WtPhi(Len1Out))

!------------------------------------------------------------------------------
!   [3] For each output point find i and j so that the point on the
!       output grid lies between i and i+1 and j and j+1.
!       and compute the weights
!------------------------------------------------------------------------------

LStretch = .FALSE.
IF (domain % header(1) % FixHd(FH_SubModel) == FH_SubModel_Ocean) THEN
  PointsInRow = domain % header(1) % FixHd(FH_ColDepCSize1)
  PointsInCol = domain % header(1) % FixHd(FH_RowDepCSize1)
  !  check if all lat/long increments are the same
  IF (ANY (domain % header(1) % ColDepC(2:PointsInRow) /= domain % header(1) % ColDepC(1)) .OR. &
      ANY (domain % header(1) % RowDepC(2:PointsInCol) /= domain % header(1) % RowDepC(1))) THEN
    LStretch = .TRUE.
  END IF
END IF

IF (LStretch) THEN
  CALL Ops_HorizontalGridStretch (PointsInRow,        &
                                  PointsInCol,        &
                                  Len1Out,            &
                                  LambdaOrigin,       &
                                  PhiOrigin,          &
                                  domain % header(1), &
                                  LambdaOut,          &
                                  PhiOut,             &
                                  IOut,               &
                                  JOut,               &
                                  WtPhi,              &
                                  WtLambda)

ELSE

  IF (domain % variable_resolution) THEN ! Variable resolution grid

    ! Set up Phi and Lambda pointers to point to appropriate sections of row and
    ! column dependent constants by stashcode.
    SELECT CASE (StashCode)
      CASE (StashItem_u, StashCode_u10, StashItem_u_unslanted)
        Phi => domain % header(1) % RowDepC(1:domain % header(1) % IntC(IC_YLen))
        Lambda => domain % header(1) % ColDepC(domain % header(1) % IntC(IC_XLen) + 1:2 * domain % header(1) % IntC(IC_XLen))
      CASE (StashItem_v, StashCode_v10, StashItem_v_unslanted)
        Phi => domain % header(1) % RowDepC(domain % header(1) % IntC(IC_YLen) + 1:2 * domain % header(1) % IntC(IC_YLen))
        Lambda => domain % header(1) % ColDepC(1:domain % header(1) % IntC(IC_XLen))
      CASE DEFAULT
        Phi => domain % header(1) % RowDepC(1:domain % header(1) % IntC(IC_YLen))
        Lambda => domain % header(1) % ColDepC(1:domain % header(1) % IntC(IC_XLen))
    END SELECT

    CALL Ops_HorizontalInterp_Weights (LambdaOut,                                                        &
                                       Lambda,                                                           &
                                       LambdaOrigin,                                                     &
                                       domain % header(1) % ColDepC(domain % header(1) % IntC(IC_XLen)), &
                                       PhiOut,                                                           &
                                       Phi,                                                              &
                                       domain % header(1) % RowDepC(domain % header(1) % IntC(IC_YLen)), &
                                       WtLambda,                                                         &
                                       WtPhi,                                                            &
                                       IOut,                                                             &
                                       JOut)

  ELSE !  Regular grid
    WHERE (LambdaOut(:) >= LambdaOrigin)
      WtLambda(:) = (LambdaOut(:) - LambdaOrigin) * RecipDLambdaIn
    ELSEWHERE
      WtLambda(:) = (LambdaOut(:) + 2 * Pi - LambdaOrigin) * RecipDLambdaIn
    ENDWHERE
    WtPhi(:) = (PhiOut(:) - PhiOrigin) * RecipDPhiIn

    IOut(:) = INT (WtLambda(:) + 1)
    WtLambda(:) = WtLambda(:) - (IOut(:) - 1.0)

    JOut(:) = INT (WtPhi(:) + 1)
    WtPhi(:) = WtPhi(:) - (JOut(:) - 1.0)

  END IF

END IF

!------------------------------------------------------------------------------
!   [4]   Perform required interpolations.
!------------------------------------------------------------------------------

SELECT CASE (interp_type_local)
  CASE (interp_type_linear_coast)
    CALL Ops_HorizontalInterpLinearCoast (LowerBound, &
                                          Len1Out,    &
                                          Datain,     &
                                          WtLambda,   &
                                          WtPhi,      &
                                          IOut,       &
                                          JOut,       &
                                          DataOut)
  CASE (interp_type_nearest)
    CALL Ops_HorizontalInterpNearest (LowerBound, &
                                      Len1Out,    &
                                      Datain,     &
                                      WtLambda,   &
                                      WtPhi,      &
                                      IOut,       &
                                      JOut,       &
                                      DataOut)
  CASE DEFAULT
    WRITE (message, '(A,I0)') "Invalid interp type: ", interp_type_local
    CALL gen_fail (RoutineName, &
                   message)
END SELECT

!------------------------------------------------------------------------------
! [5] Tidy up
!------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_HorizontalInterp
