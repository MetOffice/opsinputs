!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Reads varfields and varfield levels from a varob file.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_GetVarfieldsInfo (varobs,    &
                                 varfields, &
                                 varlevels)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE GenMod_UMHeaderConstants, ONLY: &
  FH_ColDepCSize1

IMPLICIT NONE

! Subroutine arguments:
CLASS (varobs_type), INTENT(IN) :: varobs
INTEGER, POINTER, OPTIONAL      :: varfields(:)
INTEGER, POINTER, OPTIONAL      :: varlevels(:)

! Local declarations:
INTEGER                         :: num_varfields
INTEGER                         :: i
CHARACTER(len=*), PARAMETER     :: routine_name = "Ops_GetVarfieldsInfo"

IF (UseTrace) CALL gen_trace_entry (routine_name)

num_varfields = COUNT (varobs % ColDepC(3 * varobs % FixHd(FH_ColDepCSize1) + 1: &
                                        4 * varobs % FixHd(FH_ColDepCSize1)) /= IMDI)
IF (PRESENT (varfields)) THEN
  ALLOCATE (varfields(num_varfields))
  DO i = 1, varobs % FixHd(FH_ColDepCSize1)
    IF (NINT (varobs % ColDepC(3 * varobs % FixHd(FH_ColDepCSize1) + i)) /= IMDI) THEN
      varfields(NINT (varobs % ColDepC(3 * varobs % FixHd(FH_ColDepCSize1) + i))) = i
    END IF
  END DO
ELSE
  NULLIFY (varfields)
END IF

IF (PRESENT (varlevels)) THEN
  ALLOCATE (varlevels(num_varfields))
  DO i = 1, varobs % FixHd(FH_ColDepCSize1)
    IF (NINT (varobs % ColDepC(3 * varobs % FixHd(FH_ColDepCSize1) + i)) /= IMDI) THEN
      varlevels(NINT (varobs % ColDepC(3 * varobs % FixHd(FH_ColDepCSize1) + i))) = &
                  NINT (varobs % ColDepC(7 * varobs % FixHd(FH_ColDepCSize1) + i))
    END IF
  END DO
ELSE
  NULLIFY (varlevels)
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE Ops_GetVarfieldsInfo
