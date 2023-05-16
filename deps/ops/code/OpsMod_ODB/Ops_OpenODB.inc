!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Opens an ODB database.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_OpenODB (h,         &
                        open_mode, &
                        dbname,    &
                        npools)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  MessageOut,          &
  UseTrace

USE odbshared, ONLY: &
  db

USE ops_odb_wrappers, ONLY: &
  ops_odb_getval,           &
  ops_odb_init,             &
  ops_odb_open

USE OpsMod_ODBInfo, ONLY: &
  odb_all_pools,          &
  use_all_pools

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(INOUT)  :: h
CHARACTER(len=*), INTENT(IN)           :: open_mode
CHARACTER(len=*), OPTIONAL, INTENT(IN) :: dbname
INTEGER, OPTIONAL, INTENT(IN)          :: npools

! Local declarations:
CHARACTER(len=*), PARAMETER            :: RoutineName = "Ops_OpenODB"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

CALL ops_odb_init

IF (PRESENT (dbname)) THEN
  h % dbname = dbname
ELSE
  h % dbname = "ECMA"
END IF
IF (open_mode == "NEW") THEN
  IF (PRESENT (npools)) THEN
    h % npools = npools
  ELSE
    h % npools = 0
  END IF
END IF

WRITE (MessageOut, '(2A,I0,2A)') ' Call ODB_open with dbname, npools = ', TRIM (h % dbname) // " ", &
                                  h % npools, ' open_mode = ', TRIM (open_mode)
CALL ops_odb_open (h,         &
                   open_mode)

IF (open_mode /= "NEW") THEN
  h % just_opened = .FALSE.
END IF

h % odb_mdi = ops_odb_getval (h, "$mdi")

IF (use_all_pools) THEN
  ALLOCATE (h % pool_write_list(1))
  h % pool_write_list(:) = odb_all_pools
ELSE
  ALLOCATE (h % pool_write_list(SIZE (db(h % handle) % poolidx)))
  h % pool_write_list(:) = db(h % handle) % poolidx(:)
END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_OpenODB
