!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Calculate weights for horizontal interpolation on variable resolution grids.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_HorizontalInterp_Weights (LambdaOut,    &
                                         Lambda,       &
                                         LambdaOrigin, &
                                         LambdaLast,   &
                                         PhiOut,       &
                                         Phi,          &
                                         PhiLast,      &
                                         WtLambda,     &
                                         WtPhi,        &
                                         IOut,         &
                                         JOut)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI,                                &
    Pi,                                  &
    RMDI

USE OpsMod_Constants, ONLY: &
    deg2rad

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(IN)            :: LambdaOut(:)
REAL, INTENT(IN)            :: Lambda(:)
REAL, INTENT(IN)            :: LambdaOrigin
REAL, INTENT(IN)            :: LambdaLast
REAL, INTENT(IN)            :: PhiOut(:)
REAL, INTENT(IN)            :: Phi(:)
REAL, INTENT(IN)            :: PhiLast
REAL, INTENT(OUT)           :: WtLambda(:)
REAL, INTENT(OUT)           :: WtPhi(:)
INTEGER, INTENT(OUT)        :: IOut(:)
INTEGER, INTENT(OUT)        :: JOut(:)

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_HorizontalInterp_Weights"
INTEGER                     :: i
INTEGER                     :: j

IF (UseTrace) CALL gen_trace_entry (RoutineName)

WtLambda = RMDI
WtPhi = RMDI
IOut = IMDI
JOut = IMDI

DO i = 1, SIZE (LambdaOut)
  DO j = 1, SIZE (Lambda)
    IF (LambdaOut(i) >= LambdaOrigin) THEN
      IF (LambdaOut(i) < Lambda(j) * deg2rad) THEN
        IF (j > 1) THEN
          WtLambda(i) = (LambdaOut(i) - Lambda(j - 1) * deg2rad) / &
                          (Lambda(j) * deg2rad - Lambda(j - 1) * deg2rad)
          IOut(i) = j - 1
        ELSE
          WtLambda(i) = (Lambda(1) * deg2rad - LambdaOut(i)) / &
                            (Lambda(2) * deg2rad - Lambda(1) * deg2rad)
          IOut(i) = 1
        END IF
        EXIT
      END IF
    ELSE
      IF (LambdaOut(i) + 2 * pi < Lambda(j) * deg2rad) THEN
        IF (j > 1) THEN
          WtLambda(i) = (LambdaOut(i) + 2 * pi - Lambda(j - 1) * deg2rad) / &
                          (Lambda(j) * deg2rad - Lambda(j - 1) * deg2rad)
          IOut(i) = j - 1
        ELSE
          WtLambda(i) = (Lambda(1) * deg2rad - LambdaOut(i) + 2 * pi) / &
                            (Lambda(2) * deg2rad - Lambda(1) * deg2rad)
          IOut(i) = 1
        END IF
        EXIT
      END IF
    END IF
  END DO
END DO

WHERE (IOut == IMDI)
  WtLambda = LambdaOut - LambdaLast * deg2rad
  IOut = SIZE (Lambda)
END WHERE

DO i = 1, SIZE (PhiOut)
  DO j = 1, SIZE (Phi)
    IF (PhiOut(i) <= Phi(j) * deg2rad) THEN
      IF (j > 1) THEN
        WtPhi(i) = (PhiOut(i) - Phi(j - 1) * deg2rad) / &
                      (Phi(j) * deg2rad - Phi(j - 1) * deg2rad)
        JOut(i) = j - 1
      ELSE
        WtPhi(i) = (Phi(1) * deg2rad - PhiOut(i)) / &
                          (Phi(2) * deg2rad - Phi(1) * deg2rad)
        JOut(i) = 1
      END IF
      EXIT
    END IF
  END DO
END DO

WHERE (JOut == IMDI)
  WtPhi = PhiOut - PhiLast * deg2rad
  JOut = SIZE (Phi)
END WHERE

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_HorizontalInterp_Weights
