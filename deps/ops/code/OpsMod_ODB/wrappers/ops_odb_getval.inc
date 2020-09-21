!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Wrapper for odb_getval.
!-------------------------------------------------------------------------------

#ifndef NO_ODB
FUNCTION ops_odb_getval (session, &
                         var)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE odb_module, ONLY: &
  odb_getval

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Function arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
CHARACTER(len=*), INTENT(IN)       :: var

! Function result:
REAL                               :: ops_odb_getval

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_getval"
CHARACTER(len=200)                 :: message

IF (UseTrace) CALL gen_trace_entry (routine_name)

IF (session % handle == IMDI) THEN
  ops_odb_getval = ops_odb_getval_hardwired (var)
ELSE
  ops_odb_getval = odb_getval (session % handle, TRIM (var))
END IF
IF (ops_odb_getval <= 0) THEN
  WRITE (message, '(A, F20.2)') "Cannot find value for var " // TRIM (var) // ", rc = ", ops_odb_getval
  CALL gen_fail (routine_name, &
                 message)
END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END FUNCTION ops_odb_getval

#else

FUNCTION ops_odb_getval (session, &
                         var)

USE GenMod_Core, ONLY: &
  gen_fail

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Function arguments:
TYPE (ODBSession_type), INTENT(IN) :: session
CHARACTER(len=*), INTENT(IN)       :: var

! Function result:
REAL                               :: ops_odb_getval

! Local declarations:
CHARACTER(len=*), PARAMETER        :: routine_name = "ops_odb_getval"

ops_odb_getval = 0.0

CALL gen_fail (routine_name,                                   &
               "ODB support not compiled for this executable")

END FUNCTION ops_odb_getval

#endif
