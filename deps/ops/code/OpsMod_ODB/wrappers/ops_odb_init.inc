!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for odb_init.  Includes a "first time" variable to ensure it is called
! only once
!-------------------------------------------------------------------------------

#ifndef NO_ODB
SUBROUTINE ops_odb_init

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE odb_module, ONLY: &
  odb_init

USE OpsMod_ODBKinds, ONLY: &
  odb_int

IMPLICIT NONE

! Local declarations:
CHARACTER(len=*), PARAMETER :: routine_name = "ops_odb_init"
INTEGER                     :: return_code
CHARACTER(len=200)          :: message
INTEGER(kind=odb_int)       :: odb_mype
INTEGER(kind=odb_int)       :: odb_nproc
LOGICAL                     :: first_time = .TRUE.

IF (UseTrace) CALL gen_trace_entry (routine_name)

IF (first_time) THEN
  return_code = odb_init (myproc = odb_mype, nproc = odb_nproc)
  IF (return_code /= 0) THEN
    WRITE (message, '(A,I0)') "Failure in odb_init, rc = ", return_code
    CALL gen_fail (routine_name, &
                   message)
  END IF
  first_time = .FALSE.

END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_odb_init

#else

SUBROUTINE ops_odb_init

USE GenMod_Core, ONLY: &
  gen_fail

IMPLICIT NONE

! Local declarations:
CHARACTER(len=*), PARAMETER :: routine_name = "ops_odb_init"

CALL gen_fail (routine_name,                                   &
               "ODB support not compiled for this executable")

END SUBROUTINE ops_odb_init

#endif
