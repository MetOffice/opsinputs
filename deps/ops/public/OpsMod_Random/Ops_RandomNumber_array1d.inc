!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Generate random numbers, choosing the method specified by random_number_method
! in the module
!-------------------------------------------------------------------------------

RECURSIVE SUBROUTINE Ops_RandomNumber_array1d (Out,     &
                                               mapping)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(OUT)           :: Out(:)
OPTIONAL                    :: mapping
INTERFACE
  FUNCTION mapping (out_size) RESULT (out)
    INTEGER, INTENT(IN)     :: out_size
    REAL                    :: out(out_size)
  END FUNCTION mapping
END INTERFACE

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_RandomNumber_array1d"
CHARACTER(len=80)           :: Message

IF (UseTrace) CALL gen_trace_entry (RoutineName)

IF (.NOT. initialised) CALL Ops_InitRandom

IF (PRESENT (mapping)) THEN
  out = mapping (SIZE(out))
ELSE
  SELECT CASE (random_number_method)
    CASE (meto_method)
      CALL Ops_RandomNumberMeto_array1d (Out)
    CASE (intrinsic_method)
      CALL Ops_RandomNumberInt_array1d (Out)
    CASE DEFAULT
      WRITE (Message, '(A,I0)') "Invalid random number method: ", random_number_method
      CALL gen_fail (RoutineName, &
                     Message)
  END SELECT
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_RandomNumber_array1d
