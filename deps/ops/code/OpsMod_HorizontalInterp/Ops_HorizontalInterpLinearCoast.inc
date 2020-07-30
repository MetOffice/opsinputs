!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Performs linear interpolation of a 2-d input field to a 2-d set of points
! defined by IOut, JOut and WtLambda, WtPhi taking into account missing data.
! If any of the surrounding points has missing data then the value of the
! nearest point is taken.
!
! This routine is a modified version of the routine tri_linear written by Mark
! Mawson and described in:
!
!                The proposed semi-Lagrangian advection scheme for the
!                   semi-Implicit Unified Model integration scheme.
!                         F.R. Division working paper No 162.
!                                    Mark H. Mawson
!
!-------------------------------------------------------------------------------

SUBROUTINE Ops_HorizontalInterpLinearCoast (LowerBound, &
                                            Len1Out,    &
                                            DataExt,    &
                                            WtLambda,   &
                                            WtPhi,      &
                                            IOut,       &
                                            JOut,       &
                                            DataOut)

USE GenMod_Constants, ONLY: &
    RMDItol

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE OpsMod_Kinds, ONLY : &
    real32

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)         :: LowerBound  ! lower bounds of DataExt
INTEGER, INTENT(IN)         :: Len1Out     ! Dimension of DataOut in i direction.
REAL(real32), INTENT(IN)    :: DataExt(LowerBound:,LowerBound:) ! Data interpolated
REAL, INTENT(IN)            :: WtLambda(:) ! A number between 0 & 1.
REAL, INTENT(IN)            :: WtPhi(:)    ! A number between 0 & 1.
INTEGER, INTENT(IN)         :: IOut(:)     ! Point such that
INTEGER, INTENT(IN)         :: JOut(:)     ! the desired output point lies between it and it+1.
REAL, INTENT(OUT)           :: DataOut(:)  ! Data interpolated to desired locations.

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_HorizontalInterpLinearCoast"
INTEGER                     :: i                 ! } Loop
REAL                        :: Wt_Near           ! } For searching coast
REAL                        :: Wt_Point          ! } points

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! [1.0]  Perform linear interpolation in i and j directions simultaneously.
! ------------------------------------------------------------------------------

DO i = 1, Len1Out

  IF (IOut(i) < LBOUND (DataExt, DIM = 1) .OR. IOut(i) + 1 > UBOUND (DataExt, DIM = 1) .OR. &
      JOut(i) < LBOUND (DataExt, DIM = 2) .OR. JOut(i) + 1 > UBOUND (DataExt, DIM = 2)) THEN

    ! Use nearest point with data.  If no point found DataOut left as RMDI

    DataOut(i) = RMDI

  ELSE IF (ABS (DataExt(IOut(i), JOut(i)) - RMDI) > RMDItol .AND. &
           ABS (DataExt(IOut(i) + 1, JOut(i)) - RMDI) > RMDItol .AND. &
           ABS (DataExt(IOut(i), JOut(i) + 1) - RMDI) > RMDItol .AND. &
           ABS (DataExt(IOut(i) + 1, JOut(i) + 1) - RMDI) > RMDItol) THEN

    ! Normal linear interpolation

    DataOut (i) = (1.0 - WtLambda(i)) * (1.0 - WtPhi(i)) * &
                    DataExt (IOut(i), JOut(i))             &
                 + WtLambda(i) * (1.0 - WtPhi(i)) *        &
                    DataExt (IOut(i) + 1, JOut(i))         &
                 + (1.0 - WtLambda(i)) * WtPhi(i) *        &
                    DataExt (IOut(i), JOut(i) + 1)         &
                 + WtLambda(i) * WtPhi(i) *                &
                    DataExt (IOut(i) + 1, JOut(i) + 1)

  ELSE

    ! Use nearest point with data.  If no point found DataOut left as RMDI

    DataOut(i) = RMDI

    Wt_Near = 0.0  ! reinitialise

    ! test for point IOut(i), JOut(i)
    Wt_Point = (1.0 - WtLambda(i)) * (1.0 - WtPhi(i))
    IF (Wt_Point > Wt_Near .AND.                            &
       ABS (DataExt (IOut(i), JOut(i)) - RMDI) > RMDItol) THEN
      Wt_Near = Wt_Point
      DataOut(i) = DataExt (IOut(i), JOut(i))
    END IF

    ! test for point IOut(i) + 1, JOut(i)
    Wt_Point = WtLambda(i) * (1.0 - WtPhi(i))
    IF (Wt_Point > Wt_Near .AND.                            &
       ABS (DataExt (IOut(i) + 1, JOut(i)) - RMDI) > RMDItol) THEN
      Wt_Near = Wt_Point
      DataOut(i) = DataExt (IOut(i) + 1, JOut(i))
    END IF

    ! test for point IOut(i), JOut(i) + 1
    Wt_Point = (1.0 - WtLambda(i)) * WtPhi(i)
    IF (Wt_Point > Wt_Near .AND.                            &
       ABS (DataExt (IOut(i), JOut(i) + 1) - RMDI) > RMDItol) THEN
      Wt_Near = Wt_Point
      DataOut(i) = DataExt (IOut(i), JOut(i) + 1)
    END IF

    ! test for point IOut(i) + 1, JOut(i) + 1
    Wt_Point = WtLambda(i) * WtPhi(i)
    IF (Wt_Point > Wt_Near .AND.                            &
       ABS (DataExt (IOut(i) + 1, JOut(i) + 1) - RMDI) > RMDItol) THEN
      Wt_Near = Wt_Point
      DataOut(i) = DataExt (IOut(i) + 1, JOut(i) + 1)
    END IF

  END IF

END DO      ! Close i loop

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_HorizontalInterpLinearCoast
