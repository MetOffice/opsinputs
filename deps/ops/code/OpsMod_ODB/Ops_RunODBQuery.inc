!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Execute an ODB query.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_RunODBQuery (h,             &
                            query,         &
                            data,          &
                            query_strings, &
                            query_values,  &
                            colnames,      &
                            poolno)

USE GenMod_Control, ONLY: &
  GeneralMode,            &
  mype,                   &
  VerboseMode

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_message,         &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE ops_odb_wrappers, ONLY: &
  ops_odb_get,              &
  ops_odb_get_column_names, &
  ops_odb_get_num_columns,  &
  ops_odb_get_query_size,   &
  ops_odb_is_query_valid

USE OpsMod_ODBInfo, ONLY: &
  verbose_odb

USE OpsMod_ODBKinds, ONLY: &
  odb_real

USE OpsMod_ODBTypes, ONLY: &
  ODBSession_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (ODBSession_type), INTENT(IN)        :: h
CHARACTER(len=*), INTENT(IN)              :: query
REAL(kind=odb_real), POINTER              :: data(:,:)
CHARACTER(len=*), OPTIONAL, INTENT(IN)    :: query_strings(:)
REAL(kind=odb_real), OPTIONAL, INTENT(IN) :: query_values(:)
CHARACTER(len=*), OPTIONAL, POINTER       :: colnames(:)
INTEGER, OPTIONAL, INTENT(IN)             :: poolno

! Local declarations:
CHARACTER(len=*), PARAMETER               :: RoutineName = "Ops_RunODBQuery"
INTEGER                                   :: nrows
INTEGER                                   :: ncols
CHARACTER(len=200), ALLOCATABLE           :: Message(:)
INTEGER                                   :: i
LOGICAL                                   :: verbose_odb_select

!-------------------------------------------------------------------------------
! 0. Tracing and initialisations
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName,            &
                                    message = TRIM (query))

NULLIFY (data)

verbose_odb_select = verbose_odb .AND. mype == 0

!-------------------------------------------------------------------------------
! 1. Determine whether the query is valid or not.  In this way incorrect queries
!    can be handled more gracefully than ODB software handles them.
!-------------------------------------------------------------------------------

IF (.NOT. ops_odb_is_query_valid (h, query)) THEN
  CALL gen_fail (RoutineName,                                   &
                 "Query " // TRIM (query) // " does not exist")
ELSE

  !-------------------------------------------------------------------------------
  ! 1.1. If the colnames argument is present we get the list of query columns.
  !-------------------------------------------------------------------------------

  IF (PRESENT (colnames)) THEN
    NULLIFY (colnames)
    ncols = ops_odb_get_num_columns (h,     &
                                     query)
    ALLOCATE (colnames(ncols))
    CALL ops_odb_get_column_names (h,        &
                                   query,    &
                                   colnames)
  END IF

  !-------------------------------------------------------------------------------
  ! 1.2. Run query.  ODB_select returns the number of rows/columns that will be
  !      extracted.  This is needed for array dimensioning.
  !-------------------------------------------------------------------------------

  IF (GeneralMode >= VerboseMode .AND. mype == 0) THEN
    IF (PRESENT (query_strings) .AND. PRESENT (query_values)) THEN
      ALLOCATE (message(1 + SIZE (query_strings)))
      WRITE (Message(1), '(A)') "About to run ODB_select for " // TRIM (query)
      DO i = 1, SIZE (query_strings)
        WRITE (Message(i + 1), '(A50,A,F20.3)') TRIM (query_strings(i)), " = ", query_values(i)
      END DO
      CALL gen_message (RoutineName, &
                        Message)
    ELSE
      ALLOCATE (message(1))
      WRITE (Message(1), '(A)') "About to run query " // TRIM (query)
      CALL gen_message (RoutineName, &
                        Message)
    END IF
    DEALLOCATE (message)
  END IF
  CALL ops_odb_get_query_size (h,                                   &
                               query,                               &
                               nrows,                               &
                               ncols,                               &
                               query_strings = query_strings,       &
                               query_values = query_values,         &
                               verbose_output = verbose_odb_select, &
                               pool_number = poolno)
  IF (nrows > 0) THEN

    !-------------------------------------------------------------------------------
    ! 1.2.1. The query data is placed in data.  ODB requires the 0'th column for
    !        internal purposes.
    !-------------------------------------------------------------------------------

    ALLOCATE (data(nrows,0:ncols))
    CALL ops_odb_get (h,                    &
                      query,                &
                      data,                 &
                      pool_number = poolno)
  END IF

END IF

!-------------------------------------------------------------------------------
! 2. Finish
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_RunODBQuery
