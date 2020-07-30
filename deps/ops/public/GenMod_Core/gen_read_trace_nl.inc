!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Reads the GEN Trace namelist.
!
! The relevant information is read using a NAMELIST read. If a problem is
! encountered a flag is raised and a report issued.
!
! INPUTS
!   Name of the control file to read (via environment variable)
!
! OUTPUTS
!   The values in the namelist in that control file
!-------------------------------------------------------------------------------

SUBROUTINE gen_read_trace_nl

USE GenMod_Control, ONLY: &
    GeneralMode,          &
    mype,                 &
    ProduceHTML,          &
    DebugMode,            &
    VerboseMode

IMPLICIT NONE

! Local declarations:
INTEGER                     :: file_unit
CHARACTER(len=*), PARAMETER :: RoutineName = "gen_read_trace_nl"

NAMELIST / Gen_TraceNL / &
  BackTrace,             &
  ActiveCallTree,        &
  FlowTrace,             &
  MaxNoRoutines,         &
  TraceIndentAmount,     &
  TraceMaxDepth,         &
  TraceMaxStackPointer,  &
  TraceMemory,           &
  TraceRelMemory,        &
  TracePEnum,            &
  TraceRepeatBody,       &
  TraceRepeatHead,       &
  TraceFlush,            &
  TraceAllPEs,           &
  TraceTimeStamp,        &
  TraceRelTimeStamp,     &
  TraceRoutinesSpecial,  &
  TraceDisplayStride,    &
  TraceTimes,            &
  max_dr_hook_handles

CALL gen_get_unit_from_env (file_unit,          &
                            (/'GEN_TRACE_NL'/), &
                            'gen_trace.nl')
IF (file_unit /= -1) THEN
    READ (file_unit, NML = Gen_TraceNL)
    IF (GeneralMode >= DebugMode .AND. mype == 0) WRITE (MessageOut, NML = Gen_TraceNL)
    CLOSE (file_unit)
    CALL gen_free_unit (file_unit)
ELSE IF (GeneralMode >= VerboseMode) THEN
    CALL gen_message (RoutineName,                        &
                      "Gen_TraceNL: use default values.")
END IF

IF (FlowTrace .AND. .NOT. ProduceHTML) THEN
    CALL gen_warn (RoutineName,                                                  &
                   "Cannot flow trace without producing HTML output at present")
    FlowTrace = .FALSE.
END IF

END SUBROUTINE gen_read_trace_nl
