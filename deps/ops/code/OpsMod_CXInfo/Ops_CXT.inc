!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Calculates CX temperatures from theta
!-------------------------------------------------------------------------------

SUBROUTINE Ops_CXT (CX)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  gen_warn,            &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  PRef,                                  &
  Kappa,                                 &
  RMDI

USE OpsMod_MiscTypes, ONLY: &
  ZCodeLevelsB

USE OpsMod_ObsInfo, ONLY: &
  Ops_Alloc

IMPLICIT NONE

! Subroutine arguments:
TYPE (CX_Type), INTENT(INOUT) :: CX

! Local declarations:
REAL, POINTER                 :: PLevels(:,:)
REAL, POINTER                 :: Exner(:,:)
INTEGER                       :: I
INTEGER                       :: J
CHARACTER(len=80)             :: ErrMess
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_CXT"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (CX % Header % Theta % Present .AND. &
    .NOT. CX % Header % T % Present) THEN

  IF (CX % Header % Theta % ZCode == ZCodeLevelsB) THEN
    PLevels => CX % PLevelsB
  ELSE
    PLevels => CX % PLevelsA
  END IF

  Exner => CX % ExnerA

  IF (ASSOCIATED (PLevels)) THEN
    CALL Ops_Alloc (CX % Header % T,             &
                    "CX % T",                    &
                    CX % Header % NumLocal,      &
                    CX % T,                      &
                    HdrIn = CX % Header % Theta)
    DO I = 1, CX % Header % NumLocal
      DO J = 1, CX % Header % Theta % NumLev
        IF (PLevels(I,J) >= 0) THEN
          CX % T(I,J) = CX % Theta(I,J) * (PLevels(I,J) / pref) ** kappa
        ELSE
          CX % T(I,J) = RMDI
        END IF
      END DO
    END DO
  ELSE IF (ASSOCIATED (Exner)) THEN
    CALL Ops_Alloc (CX % Header % T,             &
                    "CX % T",                    &
                    CX % Header % NumLocal,      &
                    CX % T,                      &
                    HdrIn = CX % Header % Theta)
    DO I = 1, SIZE (CX % Theta, DIM = 1)
      DO J = 1, SIZE (CX % Theta, DIM = 2)
       IF (Exner(I,J) >= 0) THEN
         CX % T(I,J) = CX % Theta(I,J) * Exner(I,J)
       ELSE
         CX % T(I,J) = RMDI
       END IF
      END DO
    END DO
  ELSE
    WRITE (ErrMess, '(A)') 'Plevels or Exner missing'
    CALL gen_warn (RoutineName, &
                   ErrMess)
  END IF

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_CXT
