!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Skip namelist comments starting with a '#' or '!'.  Return the type of the
! next namelist, with the unit position set to the start of this namelist.
!
! Inputs: Unit of namelist file
!
! Outputs: Name of next stationlist
!-------------------------------------------------------------------------------

SUBROUTINE Ops_FindNextNamelist (Unit, &
                                 Name)

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE OpsMod_Kinds, ONLY: &
  integer_default

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)           :: Unit
CHARACTER(len=*), INTENT(OUT) :: Name

! Local declarations:
CHARACTER(len=80)             :: Line
CHARACTER(len=80)             :: ErrorMessage
INTEGER(kind=integer_default) :: Status
INTEGER                       :: pos
CHARACTER(len=*), PARAMETER   :: RoutineName = 'Ops_FindNextNamelist'

IF (UseTrace) CALL gen_trace_entry (RoutineName)

Name = ''
DO
  READ (Unit, '(A)', IOSTAT = Status) Line
  Line = ADJUSTL (Line)
  IF (Status < 0) THEN
    EXIT
  ELSE IF (Status > 0) THEN
    WRITE (ErrorMessage, '(A,I0)') "Error reading namelist, status ", status
    CALL gen_fail (RoutineName,  &
                   ErrorMessage)
  ELSE IF (Line(1:1) == '&') THEN
    BACKSPACE (Unit)
    pos = INDEX (line(2:80),' ')
    IF (pos == 0) pos = 80
    Name = Line(2:pos + 1)
    EXIT
  ELSE IF (Line /= '' .AND. Line(1:1) /= '#' .AND. Line(1:1) /= '!') THEN
    WRITE (ErrorMessage,'(A,I0,A)') 'Unidentified line in namelist file (unit number ', Unit,'):'
    CALL gen_fail (RoutineName,           &
                   (/ErrorMessage,Line/))
  END IF
END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_FindNextNamelist
