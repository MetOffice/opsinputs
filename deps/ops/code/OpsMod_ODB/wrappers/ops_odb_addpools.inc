!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Wrapper for odb_addpools that updates the session object with the new pool
! information.
!-------------------------------------------------------------------------------

#ifndef NO_ODB

SUBROUTINE ops_odb_addpools (session,          &
                             new_pools_to_add)

USE GenMod_Control, ONLY: &
  nproc

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE odb_module, ONLY: &
  odb_addpools

USE odbshared, ONLY: &
  db

USE OpsMod_ODBKinds, ONLY: &
  odb_int

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(INOUT) :: session
INTEGER, OPTIONAL, INTENT(IN)         :: new_pools_to_add

! Local declarations:
CHARACTER(len=*), PARAMETER           :: routine_name = "ops_odb_addpools"
INTEGER(kind=odb_int)                 :: local_new_pools_to_add
CHARACTER(len=200)                    :: message
INTEGER                               :: i

IF (UseTrace) CALL gen_trace_entry (routine_name)

IF (PRESENT (new_pools_to_add)) THEN
  local_new_pools_to_add = new_pools_to_add
ELSE
  local_new_pools_to_add = nproc
END IF

IF (session % just_opened) THEN

  session % just_opened = .FALSE.

ELSE

  session % npools = odb_addpools (session % handle, local_new_pools_to_add, session % npools_old)
  IF (session % npools <= 0) THEN
    WRITE (message, '(A,I0,A,I0)') "Error in odb_addpools, requesting ", local_new_pools_to_add, &
                                   " pools, rc = ", session % npools
    CALL gen_fail (routine_name, &
                   message)
  END IF

  IF (ASSOCIATED (session % pool_write_list)) DEALLOCATE (session % pool_write_list)

  ALLOCATE (session % pool_write_list((session % npools - session % npools_old) / nproc))
  DO i = 1, SIZE (session % pool_write_list)
    session % pool_write_list(i) = db(session % handle) % poolidx(SIZE (db(session % handle) % poolidx) - i + 1)
  END DO

END IF

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE ops_odb_addpools

#else

SUBROUTINE ops_odb_addpools (session,          &
                             new_pools_to_add)

USE GenMod_Core, ONLY: &
  gen_fail

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(INOUT) :: session
INTEGER, OPTIONAL, INTENT(IN)         :: new_pools_to_add

! Local declarations:
CHARACTER(len=*), PARAMETER           :: routine_name = "ops_odb_addpools"

CALL gen_fail (routine_name,                                   &
               "ODB support not compiled for this executable")

END SUBROUTINE ops_odb_addpools

#endif
