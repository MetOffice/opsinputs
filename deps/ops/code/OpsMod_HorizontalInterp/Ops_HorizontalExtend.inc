!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Extends horizontal grid at borders of domain to allow horizontal
! interpolation, not valid for MPP.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_HorizontalExtend (LowerBound, &
                                 LGlobal,    &
                                 LVector,    &
                                 PhiOrigin,  &
                                 DPhiIn,     &
                                 DataExt)

USE GenMod_Core, ONLY: &
    gen_fail,          &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    Pi

USE OpsMod_Kinds, ONLY: &
    real32

USE OpsMod_HorizontalInterp, ONLY: &
    num_extend_points

IMPLICIT NONE

! Subroutine arguments:
LOGICAL, INTENT(IN)         :: LGlobal      ! True, if global
LOGICAL, INTENT(IN)         :: LVector      ! True, if vector component
REAL, INTENT(IN)            :: DPhiIn       ! Holds spacing between points in the j direction for the input field.
REAL, INTENT(IN)            :: PhiOrigin    ! Position of 1st point in the j direction for the input field.
INTEGER, INTENT(IN)         :: LowerBound   ! LowerBound of DataExt
REAL(real32), INTENT(INOUT) :: DataExt(LowerBound:,LowerBound:)   ! Data input extended

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_HorizontalExtend"
INTEGER                     :: Len1In       ! Dimension of input in i direction.
INTEGER                     :: Len2In       ! Dimension of input in j direction.
INTEGER                     :: Upper1       ! Upper Bounds of DataExt
INTEGER                     :: Upper2       !    "    "
INTEGER                     :: i            !} Loop
INTEGER                     :: j            !} indices.
INTEGER                     :: HalfLen1In   ! Len1In/2
INTEGER                     :: SwapPole     ! 1 if no sign swap, -1 otherwise
REAL                        :: HalfPi          ! pi/2
REAL                        :: HalfDPhiIn      ! DPhiIn/2
REAL                        :: Test1
REAL                        :: Test2      !used in origin checking
LOGICAL                     :: FirstOnPole      ! "         "        "

! ----------------------------------------------------------------------
!  Section 1.   Initialize
! ----------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! [0] Set derived variables
HalfPi = Pi * 0.5
HalfDPhiIn = DPhiIn * 0.5

! [1.2] For global case check grid offset from pole (PhiOrigin)
IF (LGlobal) THEN
  ! real number checks (origin differs from expected
  ! by less than 10*machine precision implies check passes
  IF (DPhiIn > 0.0) THEN
    ! check offset from south pole
    Test1 = ABS (PhiOrigin + HalfPi) / SPACING (PhiOrigin)
    Test2 = ABS (PhiOrigin + HalfPi - HalfDPhiIn) / SPACING (PhiOrigin)
  ELSE
    ! check offset from north pole
    Test1 = ABS (PhiOrigin - HalfPi) / SPACING (PhiOrigin)
    Test2 = ABS (PhiOrigin - HalfPi - HalfDPhiIn) / SPACING (PhiOrigin)
  ENDIF
  ! reasonable tolerance is 10. Value of 100. allows for dumps generated on
  ! one platform to be used on another.
  IF (Test1 < 100.0) THEN
    ! origin at pole
    FirstOnPole = .TRUE.
  ELSE IF (Test2 < 100.0) THEN
    ! origin half a grid length from pole
    FirstOnPole = .FALSE.
  ELSE
    ! unexpected origin
    CALL gen_fail (RoutineName,      &
                   "Incorrect grid")
  END IF
END IF

! [1.3]  Get and check dimensions
Len1In = SIZE (DataExt,1) - 2 * num_extend_points
Len2In = SIZE (DataExt,2) - 2 * num_extend_points
Upper1 = UBOUND (DataExt,1)
Upper2 = UBOUND (DataExt,2)
IF (num_extend_points + Len1In /= Upper1 .OR. num_extend_points + Len2In /= Upper2) THEN
  CALL gen_fail (RoutineName,                 &
                 "DataExt wrong upper bound")
END IF

!-----------------------------------------------------------------------------
!   [2] Extend input data array to bigger area to allow interpolation to
!       be done without having to redo any end points.
!-----------------------------------------------------------------------------
HalfLen1In = Len1In / 2

!   [2.1] Extend in i direction.
IF (LGlobal) THEN                 !   [2.1.1] Global
  DO j = 1, Len2In
    DataExt(-2,j)         = DataExt(Len1In - 2,j)
    DataExt(-1,j)         = DataExt(Len1In - 1,j)
    DataExt(0,j)          = DataExt(Len1In,j)
    DataExt(Len1In + 1,j) = DataExt(1,j)
    DataExt(Len1In + 2,j) = DataExt(2,j)
    DataExt(Len1In + 3,j) = DataExt(3,j)
  END DO
ELSE                              !   [2.1.2] Limited Area.
  DO j = 1, Len2In
    DataExt(-2,j)         = DataExt(1,j)
    DataExt(-1,j)         = DataExt(1,j)
    DataExt(0,j)          = DataExt(1,j)
    DataExt(Len1In + 1,j) = DataExt(Len1In,j)
    DataExt(Len1In + 2,j) = DataExt(Len1In,j)
    DataExt(Len1In + 3,j) = DataExt(Len1In,j)
  END DO
END IF

!   [2.2] Extend in j direction.

IF (LGlobal) THEN                 !   [2.2.1] Global.
  SwapPole = 1
  IF (LVector) SwapPole = -1
  IF (.NOT. FirstOnPole) THEN
    ! [2.2.1.1.3] v points.
    DO i = -2, HalfLen1In + 2
      DataExt(i,-2)         = SwapPole * DataExt(i + HalfLen1In,3)
      DataExt(i,-1)         = SwapPole * DataExt(i + HalfLen1In,2)
      DataExt(i,0)          = SwapPole * DataExt(i + HalfLen1In,1)
      DataExt(i,Len2In + 1) = SwapPole * DataExt(i + HalfLen1In,Len2In)
      DataExt(i,Len2In + 2) = SwapPole * DataExt(i + HalfLen1In,Len2In - 1)
      DataExt(i,Len2In + 3) = SwapPole * DataExt(i + HalfLen1In,Len2In - 2)
    END DO

    DO i = HalfLen1In + 3, Len1In + 3
      DataExt(i,-2)         = SwapPole * DataExt(i - HalfLen1In,3)
      DataExt(i,-1)         = SwapPole * DataExt(i - HalfLen1In,2)
      DataExt(i,0)          = SwapPole * DataExt(i - HalfLen1In,1)
      DataExt(i,Len2In + 1) = SwapPole * DataExt(i - HalfLen1In,Len2In)
      DataExt(i,Len2In + 2) = SwapPole * DataExt(i - HalfLen1In,Len2In - 1)
      DataExt(i,Len2In + 3) = SwapPole * DataExt(i - HalfLen1In,Len2In - 2)
    END DO
  ELSE
    !   [2.2.1.1.4] Other points.

    DO i = -2, HalfLen1In + 2
      DataExt(i,-2)         = SwapPole * DataExt(i + HalfLen1In,4)
      DataExt(i,-1)         = SwapPole * DataExt(i + HalfLen1In,3)
      DataExt(i,0)          = SwapPole * DataExt(i + HalfLen1In,2)
      DataExt(i,Len2In + 1) = SwapPole * DataExt(i + HalfLen1In,Len2In - 1)
      DataExt(i,Len2In + 2) = SwapPole * DataExt(i + HalfLen1In,Len2In - 2)
      DataExt(i,Len2In + 3) = SwapPole * DataExt(i + HalfLen1In,Len2In - 3)
    END DO

    DO i = HalfLen1In + 3, Len1In + 3
      DataExt(i,-2)         = SwapPole * DataExt(i - HalfLen1In,4)
      DataExt(i,-1)         = SwapPole * DataExt(i - HalfLen1In,3)
      DataExt(i,0)          = SwapPole * DataExt(i - HalfLen1In,2)
      DataExt(i,Len2In + 1) = SwapPole * DataExt(i - HalfLen1In,Len2In - 1)
      DataExt(i,Len2In + 2) = SwapPole * DataExt(i - HalfLen1In,Len2In - 2)
      DataExt(i,Len2In + 3) = SwapPole * DataExt(i - HalfLen1In,Len2In - 3)
    END DO
  END IF                    ! END IF .not.FirstOnPole
ELSE
  !   [2.2.2] Limited Area code.

  DO i = -2, Len1In + 3
    DataExt(i,-2)         = DataExt(i,1)
    DataExt(i,-1)         = DataExt(i,1)
    DataExt(i,0)          = DataExt(i,1)
    DataExt(i,Len2In + 1) = DataExt(i,Len2In)
    DataExt(i,Len2In + 2) = DataExt(i,Len2In)
    DataExt(i,Len2In + 3) = DataExt(i,Len2In)
  END DO
END IF                    ! END IF LGlobal

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_HorizontalExtend
