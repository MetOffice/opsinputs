!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Converts a set of text files into an ODB.
!-------------------------------------------------------------------------------

PROGRAM OpsProg_SimulObs

! A program to create ODBs from tabulated text-file inputs

USE GenMod_Core, ONLY: &
  gen_fail

USE ops_odb_wrappers, ONLY: &
  ops_odb_close,            &
  ops_odb_get_column_names, &
  ops_odb_get_column_types, &
  ops_odb_get_num_columns,  &
  ops_odb_init,             &
  ops_odb_open,             &
  ops_odb_put

USE OpsMod_Argument, ONLY: &
  arguments_type

USE OpsMod_IOUtils, ONLY: &
  Ops_OpenFileRead

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

INTEGER                         :: i
INTEGER                         :: i_prev
INTEGER                         :: j
INTEGER                         :: k
INTEGER                         :: jf
INTEGER                         :: jcol
INTEGER                         :: ncols_all
INTEGER                         :: nrows
INTEGER                         :: io_tmp
INTEGER                         :: ncols
CHARACTER(len=512), ALLOCATABLE :: input_file(:)
CHARACTER(len=128), ALLOCATABLE :: cvar(:)
CHARACTER(len=128)              :: cvar_file(100)
CHARACTER(len=128), ALLOCATABLE :: ctype(:)
CHARACTER(len=64)               :: tblname
INTEGER, ALLOCATABLE            :: colmap(:)
LOGICAL, ALLOCATABLE            :: is_string(:)
REAL, ALLOCATABLE               :: x(:,:)
REAL, ALLOCATABLE               :: z(:)
REAL                            :: rblank8
REAL                            :: mdi
CHARACTER(len=8), PARAMETER     :: cblank8 = ''
LOGICAL, ALLOCATABLE            :: LL_offset(:)
CHARACTER(len=300)              :: messages(3)
TYPE (arguments_type)           :: args
TYPE (ODBSession_type)          :: session
CHARACTER(len=10000)            :: buffer
INTEGER                         :: iostat
LOGICAL                         :: in_data
CHARACTER(len=8)                :: char_tmp
INTEGER                         :: unit
CHARACTER(len=128), ALLOCATABLE :: constant_columns(:)
INTEGER                         :: num_constant_columns
INTEGER                         :: constant_column_number
REAL, ALLOCATABLE               :: constant_column_values(:)
LOGICAL                         :: preset_num_rows
LOGICAL                         :: constant_rows_only

CALL ops_odb_init

!----------------------------------------------------------------------------------
!   Get program arguments
!----------------------------------------------------------------------------------

CALL args % process (input_file)

!-- Open database
!   -------------

session % dbname = 'ECMA'
session % npools = 1

CALL ops_odb_open (session, &
                   'NEW')

!-- Loop over input files and fill appropriate database table
!   When the same table appears multiple times, 
!   increment the pool number (modulo npools)

preset_num_rows = .FALSE.
constant_rows_only = .FALSE.
DO jf = 1, SIZE (input_file)
  CALL Ops_OpenFileRead (input_file(jf), &
                         unit)
  cvar_file = ""
  iostat = 0
  nrows = 0
  ncols = 0
  in_data = .FALSE.
  num_constant_columns = 0
  DO
    READ (unit, '(A)', IOSTAT = iostat) buffer
    IF (iostat == 0) THEN
      IF (buffer(1:1) == '#') THEN
        tblname = buffer(2:)
      ELSE IF (buffer(1:1) == '$') THEN
        IF (buffer(2:INDEX (buffer, "=") - 1) == "rows") THEN
          READ (buffer(INDEX (buffer, '=') + 1:), *) nrows
        END IF
        preset_num_rows = .TRUE.
        constant_rows_only = .TRUE.
      ELSE IF (INDEX (buffer, "=") > 0) THEN
        num_constant_columns = num_constant_columns + 1
      ELSE IF (.NOT. in_data) THEN
        i_prev = 1
        DO i = 1, LEN_TRIM (buffer)
          IF (buffer(i:i) == "") THEN
            IF (buffer(i+1:i+1) == "") CYCLE
            ncols = ncols + 1
            cvar_file(ncols) = buffer(i_prev:i)
            IF (INDEX (cvar_file(ncols), ".len") > 0) THEN
              cvar_file(ncols) = "LINKLEN(" // cvar_file(ncols)(1:INDEX (cvar_file(ncols), ".len") - 1) // ")"
            END IF
            IF (INDEX (cvar_file(ncols), ".offset") > 0) THEN
              cvar_file(ncols) = "LINKOFFSET(" // cvar_file(ncols)(1:INDEX (cvar_file(ncols), ".offset") - 1) // ")"
            END IF
            i_prev = i + 1
          END IF
        END DO
        ncols = ncols + 1
        cvar_file(ncols) = buffer(i_prev:)
        IF (INDEX (cvar_file(ncols), ".len") > 0) THEN
          cvar_file(ncols) = "LINKLEN(" // cvar_file(ncols)(1:INDEX (cvar_file(ncols), ".len") - 1) // ")"
        END IF
        IF (INDEX (cvar_file(ncols), ".offset") > 0) THEN
          cvar_file(ncols) = "LINKOFFSET(" // cvar_file(ncols)(1:INDEX (cvar_file(ncols), ".offset") - 1) // ")"
        END IF
        in_data = .TRUE.
      ELSE
        IF (preset_num_rows) THEN
          constant_rows_only = .FALSE.
          EXIT
        END IF
        nrows = nrows + 1
      END IF
    ELSE
      EXIT
    END IF
  END DO

  ALLOCATE (constant_columns(num_constant_columns))
  ALLOCATE (constant_column_values(num_constant_columns))

  REWIND (unit)

  in_data = .FALSE.
  constant_column_number = 0
  DO
    READ (unit, '(A)', IOSTAT = iostat) buffer
    IF (iostat == 0) THEN
      IF (buffer(1:1) == '#' .OR. buffer(1:1) == '$') THEN
      ELSE IF (INDEX (buffer, "=") > 0) THEN
        constant_column_number = constant_column_number + 1
        constant_columns(constant_column_number) = buffer(1:INDEX (buffer, "=") - 1)
        IF (INDEX (constant_columns(constant_column_number), ".len") > 0) THEN
          constant_columns(constant_column_number) = "LINKLEN(" // &
            constant_columns(constant_column_number)(1:INDEX (constant_columns(constant_column_number), ".len") - 1) // ")"
        END IF
        IF (INDEX (constant_columns(constant_column_number), ".offset") > 0) THEN
          constant_columns(constant_column_number) = "LINKOFFSET(" // &
            constant_columns(constant_column_number)(1:INDEX (constant_columns(constant_column_number), ".offset") - 1) // ")"
        END IF
        READ (buffer(INDEX (buffer, "=") + 1:), *) constant_column_values(constant_column_number)
      ELSE IF (.NOT. in_data) THEN
        EXIT
      END IF
    ELSE
      EXIT
    END IF
  END DO

  IF (tblname(1:1) /= '@') tblname = '@' // TRIM (tblname)

  ncols_all = ops_odb_get_num_columns (session, tblname)

  ALLOCATE (cvar(ncols_all))
  ALLOCATE (ctype(ncols_all))
  ALLOCATE (LL_offset(ncols_all))
  ALLOCATE (is_string(ncols_all))

  CALL ops_odb_get_column_names (session, &
                                 tblname, &
                                 cvar)

  CALL ops_odb_get_column_types (session, &
                                 tblname, &
                                 ctype)

  LL_offset(:) = cvar(:)(1:11) == 'LINKOFFSET('
  is_string(:) = .FALSE.
  WHERE (ctype(:) == 'string') is_string(:) = .TRUE.

  ALLOCATE (x(nrows,0:ncols_all))
  x(:,:) = 0
  rblank8 = TRANSFER (cblank8,rblank8)
  DO j = 1, ncols_all
    IF (ctype(j) == 'string') THEN
      DO i = 1, nrows
        x(i,j) = rblank8
      END DO
    END IF
  END DO

  ALLOCATE (colmap(ncols)) ! column mapping
  colmap(:) = 0
  DO i = 1, ncols
    DO j = 1, ncols_all
      IF (TRIM (cvar_file(i)) // tblname == cvar(j)) THEN
        colmap(i) = j
        EXIT
      END IF
    END DO
  END DO

  mdi = session % odb_mdi

  IF (ANY (colmap(:) < 0 .OR. colmap(:) > ncols_all)) THEN
    WRITE (messages(1), '(A,I0,A)') 'Error: Some column ids out of range [1:',ncols_all,']'
    CALL gen_fail ('MAIN',      &
                   messages(1))
  END IF

  DO i = 1, num_constant_columns
    DO j = 1, ncols_all
      IF (cvar(j) == TRIM (constant_columns(i)) // tblname) THEN
        x(:,j) = constant_column_values(i)
        EXIT
      END IF
    END DO
  END DO

  DO j = 1, ncols
    jcol = colmap(j)
    IF (is_string(jcol)) colmap(j) = -colmap(j)
  END DO

  IF (.NOT. constant_rows_only) THEN
    ALLOCATE (z(ncols_all))  ! temporary buffer
    DO i = 1, nrows
      READ (unit, '(A)') buffer
      buffer = ADJUSTL (buffer)
      i_prev = 1
      k = 1
      z(:) = 0
      DO j = 1, LEN_TRIM (buffer)
        IF (buffer(j:j) == "") THEN
          IF (buffer(j + 1:j + 1) == "") CYCLE
          IF (ADJUSTL (buffer(i_prev:i_prev + 7)) == 'NULL') THEN
            z(k) = mdi
          ELSE IF (is_string(ABS (colmap(k)))) THEN
            READ (buffer(i_prev:i_prev + 7), *) char_tmp
            z(k) = TRANSFER (ADJUSTR (char_tmp), z(k))
          ELSE IF (INDEX (buffer(i_prev:j), "b") > 0) THEN
            READ (buffer(i_prev + INDEX (buffer(i_prev:), "b"):j), '(B32)') io_tmp
            z(k) = io_tmp
          ELSE
            READ (buffer(i_prev:j), *) z(k)
          END IF
          k = k + 1
          i_prev = j
        END IF
      END DO
      IF (ADJUSTL (buffer(i_prev:)) == 'NULL') THEN
        z(k) = mdi
      ELSE IF (is_string(ABS (colmap(k)))) THEN
        READ (buffer(i_prev:), *) char_tmp
        z(k) = TRANSFER (ADJUSTR (char_tmp), z(k))
      ELSE IF (INDEX (buffer(i_prev:), "b") > 0) THEN
        READ (buffer(i_prev + INDEX (buffer(i_prev:), "b"):), '(B32)') io_tmp
        z(k) = io_tmp
      ELSE
        READ (buffer(i_prev:), *) z(k)
      END IF
      DO j = 1, ncols
        jcol = ABS (colmap(j))
        x(i,jcol) = z(j)
      END DO
    END DO
  END IF

  DO j = 1, ncols_all ! Handle missing offsets
    IF (INDEX (cvar(j), "LINKOFFSET") > 0 .AND. (ALL (x(1:nrows,j) == 0 .or. x(1:nrows,j) == mdi))) THEN
      k = 0
      DO i = 1, nrows
        x(i,j) = k
        k = k + x(i,j + 1) 
      END DO
    END IF
  END DO


  CALL ops_odb_put (session, &
                    tblname, &
                    x)

  IF (ALLOCATED (x)) DEALLOCATE (x)
  IF (ALLOCATED (z)) DEALLOCATE (z)
  IF (ALLOCATED (cvar)) DEALLOCATE (cvar)
  IF (ALLOCATED (ctype)) DEALLOCATE (ctype)
  IF (ALLOCATED (colmap)) DEALLOCATE (colmap)
  IF (ALLOCATED (LL_offset)) DEALLOCATE (LL_offset)
  IF (ALLOCATED (is_string)) DEALLOCATE (is_string)
  IF (ALLOCATED (constant_columns)) DEALLOCATE (constant_columns)
  IF (ALLOCATED (constant_column_values)) DEALLOCATE (constant_column_values)
END DO

CALL ops_odb_close (session)

END PROGRAM OpsProg_SimulObs
