!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Common variables and procedures
!-------------------------------------------------------------------------------

MODULE GenMod_Core

USE parkind1, ONLY: &
  JPRB

IMPLICIT NONE

SAVE

PRIVATE

INTEGER, PARAMETER   :: TraceNameLen            = 80 ! Length of trace name
INTEGER, PARAMETER   :: TraceRoutinesSpecialMax = 10 ! Maximum special routines

! Namelist Variables
! ------------------------------------------------------------------------------
INTEGER :: MaxNoRoutines        = 1024 ! Maximum number of procedures
INTEGER :: TraceIndentAmount    = 2    ! Default indent
INTEGER :: TraceMaxDepth        = 30   ! Maximum depth for printing trace
INTEGER :: TraceRepeatHead      = 50   ! Number of times trace header details
                                       ! can be printed
INTEGER :: TraceRepeatBody      = 50   ! Number of times trace body details
                                       ! can be printed
INTEGER :: TracePEnum           = 0    ! PE number for tracing
LOGICAL :: TraceMemory          = .TRUE. ! Should memory usage info be collected?
LOGICAL :: TraceRelMemory       = .TRUE. ! Should memory usage info be relative, not absolute
LOGICAL :: BackTrace            = .FALSE. ! Produce call stack for calls to gen_fail and gen_warn
LOGICAL :: FlowTrace            = .FALSE. ! Produce trace of subroutine calls
INTEGER :: TraceMaxStackPointer = 50      ! Maximum call depth for BackTrace
INTEGER :: max_dr_hook_handles  = 50      ! Maximum call depth for BackTrace
LOGICAL :: TraceFlush           = .FALSE. ! Call flush routine after each trace write.
LOGICAL :: TraceAllPEs          = .FALSE. ! Produce tracefile for all PEs.
LOGICAL :: TraceTimeStamp       = .FALSE. ! Timestamp trace lines.
LOGICAL :: TraceRelTimeStamp    = .TRUE.  ! Lines are relative, not absolute
INTEGER :: TraceDisplayStride   = 1
LOGICAL :: TraceTimes           = .TRUE.  ! Report on time taken in various places.
CHARACTER(LEN=TraceNameLen) :: TraceRoutinesSpecial(TraceRoutinesSpecialMax)=""
LOGICAL :: ActiveCallTree       = .FALSE. ! Output a heirarchy for calling tree?

! Other variables
!-------------------------------------------------------------------------------
INTEGER :: NoTraceNames       = 0     ! Number of routines to initiate trace
INTEGER :: TraceDepth         = 0     ! Current depth of trace
INTEGER :: OldTraceDepth      = 0     ! Previous depth of trace
INTEGER :: OldOldTraceDepth   = 0     ! Previous, previous depth of trace
INTEGER :: TraceUnit                  ! File unit number
INTEGER :: CallTreeUnit               ! Unit number for calling tree file
INTEGER :: NoRoutines         = 0     ! Number of routines so far
INTEGER :: Pointer            = 0     ! Pointer to routine arrays in TIMER.
INTEGER :: BaseElapsedTime    = 0     ! System clock at initiation
INTEGER :: BaseCPUTime        = 0     ! CPU time at initiation
INTEGER :: BaseMemory         = 0     ! Memory at initiation
INTEGER :: LastSpace                  ! Last memory usage

INTEGER, ALLOCATABLE :: NoCalls    (:) ! Number of calls to each routine
INTEGER, ALLOCATABLE :: NoCallsBody(:) ! Number of calls in body of each routine
INTEGER, ALLOCATABLE :: CalledBy   (:) ! Index of calling procedure
INTEGER, ALLOCATABLE :: MaxHeap    (:) ! Maximum heap usage by a procedure
INTEGER, ALLOCATABLE :: EntryHeap  (:) ! Heap usage at entry to a procedure

! All CPU times in seconds
REAL    :: CPUTimeLocalStart
REAL, ALLOCATABLE    :: CPUTimeStart   (:)
REAL, ALLOCATABLE    :: CPUTime        (:)
REAL, ALLOCATABLE    :: CPUTimeLocal   (:)
REAL, ALLOCATABLE    :: CPUTimeThisCall(:)

! All Elapsed times based on wall clock in seconds
REAL    :: ElapsedTimeLocalStart
REAL, ALLOCATABLE    :: ElapsedTimeStart   (:)
REAL, ALLOCATABLE    :: ElapsedTime        (:)
REAL, ALLOCATABLE    :: ElapsedTimeLocal   (:)
REAL, ALLOCATABLE    :: ElapsedTimeThisCall(:)

LOGICAL :: TraceActive = .FALSE. ! Is it active in this routine?
LOGICAL :: UseTrace    = .TRUE. ! Is tracing switched on?

CHARACTER (LEN=TraceNameLen) :: TraceStartedBy            ! Starting procedure
CHARACTER (LEN=TraceNameLen), ALLOCATABLE :: TimerNames(:) ! List of procedures
CHARACTER (LEN=TraceNameLen), ALLOCATABLE :: TraceNames(:) ! For timing/tracing
CHARACTER (len=TraceNameLen), ALLOCATABLE :: TraceStack(:)
INTEGER :: TraceStackPointer = 0

integer :: count0

REAL(kind=jprb), ALLOCATABLE :: dr_hook_handle_stack(:,:)
INTEGER, ALLOCATABLE :: dr_hook_pointer(:)

CHARACTER(len=*), PARAMETER :: ColourWarning = 'maroon' ! colour: warning
CHARACTER(len=*), PARAMETER :: ColourFatal = 'red'    ! colour: fatal error

! Error status codes known to the system are listed by name below.
! StatusOK StatusWarning and StatusFatal are for general use.
! Other specific codes may be added to the list,
! N.B. Warning codes must be <0, Fatal codes must be >0.
INTEGER, PARAMETER          :: StatusFatal = 1 ! Fatal error ErrorStatus value.
INTEGER, PARAMETER          :: StatusOK = 0 ! Good ErrorStatus value.
INTEGER, PARAMETER          :: StatusWarning = -1 ! Non fatal error ErrorStatus value.
INTEGER, PARAMETER          :: MessageIn = 5    ! unit number for standard input
INTEGER, PARAMETER          :: MessageOut = 6   ! unit number for standard output
INTEGER, PARAMETER          :: ErrorOut = 0     ! unit number for standard error output
INTEGER, PARAMETER          :: StatsOut = 7     ! unit number for statistics output
INTEGER                     :: WarningCount = 0 ! number of warning messages

INTEGER, PARAMETER            :: MaxFileNameLen = 255 ! maximum file name length
INTEGER, PARAMETER            :: MAX_UNITS = 300
INTEGER, PARAMETER            :: LB = 10
INTEGER, PARAMETER            :: UB = 64
LOGICAL                       :: UnitsInUse(LB:MAX_UNITS) = .FALSE.
CHARACTER(len=MaxFileNameLen) :: gen_stats_file_name

#if defined(GEN_LEN_MESSAGE_OUT)
INTEGER, PARAMETER          :: LenMessageOut = GEN_LEN_MESSAGE_OUT  ! Max len (std out)
#elif defined(SX6)||defined(SX8)
INTEGER, PARAMETER          :: LenMessageOut = 134                  ! Max len (std out)
#else
INTEGER, PARAMETER          :: LenMessageOut = 0                    ! Max len (std out)
#endif

#if defined(GEN_LEN_ERROR_OUT)
INTEGER, PARAMETER          :: LenErrorOut = GEN_LEN_ERROR_OUT      ! Max len (std err)
#elif defined(SX6)||defined(SX8)
INTEGER, PARAMETER          :: LenErrorOut = 134                    ! Max len (std err)
#else
INTEGER, PARAMETER          :: LenErrorOut = 0                      ! Max len (std err)
#endif

INTERFACE gen_fail
  MODULE PROCEDURE gen_fail_array
  MODULE PROCEDURE gen_fail_scalar
END INTERFACE gen_fail

INTERFACE gen_message
  MODULE PROCEDURE gen_message_array
  MODULE PROCEDURE gen_message_scalar
END INTERFACE gen_message

INTERFACE gen_warn
  MODULE PROCEDURE gen_warn_array
  MODULE PROCEDURE gen_warn_scalar
END INTERFACE gen_warn

PUBLIC :: gen_close_stats_file
PUBLIC :: gen_do_trace
PUBLIC :: gen_error_report
PUBLIC :: gen_free_unit
PUBLIC :: gen_get_env
PUBLIC :: gen_get_env_a
PUBLIC :: gen_get_unit
PUBLIC :: gen_get_unit_from_env
PUBLIC :: gen_fail
PUBLIC :: gen_message
PUBLIC :: gen_message_report_sync
PUBLIC :: gen_open_stats_file
PUBLIC :: Gen_ReadConfigNL
PUBLIC :: gen_replace_text
PUBLIC :: Gen_StringSplit
PUBLIC :: gen_summary
PUBLIC :: gen_trace_entry
PUBLIC :: gen_trace_exit
PUBLIC :: gen_trace_init
PUBLIC :: gen_trace_report
PUBLIC :: gen_trace_sync
PUBLIC :: gen_warn

PUBLIC :: ColourWarning
PUBLIC :: ErrorOut
PUBLIC :: MaxFileNameLen
PUBLIC :: MessageIn
PUBLIC :: MessageOut
PUBLIC :: StatsOut
PUBLIC :: StatusFatal
PUBLIC :: StatusOK
PUBLIC :: StatusWarning
PUBLIC :: UB
PUBLIC :: UseTrace

CONTAINS

INCLUDE 'gen_cpu_time.inc'
INCLUDE 'gen_do_trace.inc'
INCLUDE 'gen_error_report.inc'
INCLUDE 'gen_fail.inc'
INCLUDE 'gen_free_unit.inc'
INCLUDE 'gen_get_unit.inc'
INCLUDE 'gen_get_unit_from_env.inc'
INCLUDE 'gen_message.inc'
INCLUDE 'gen_read_trace_nl.inc'
INCLUDE 'gen_replace_text.inc'
INCLUDE 'Gen_ReadConfigNL.inc'
INCLUDE 'Gen_StringSplit.inc'
INCLUDE 'gen_summary.inc'
INCLUDE 'gen_trace_entry.inc'
INCLUDE 'gen_trace_exit.inc'
INCLUDE 'gen_trace_init.inc'
INCLUDE 'gen_trace_initialise.inc'
INCLUDE 'gen_trace_int_sort.inc'
INCLUDE 'gen_trace_real_sort.inc'
INCLUDE 'gen_trace_report.inc'
INCLUDE 'gen_trace_sync.inc'
INCLUDE 'gen_warn.inc'
INCLUDE 'Gen_WriteMessageBody.inc'
INCLUDE 'Gen_WriteMessageHeader.inc'
INCLUDE 'gen_write_trace_js_file.inc'

#include "gen_fortran_version.h"
#include "gen_close_stats_file.inc"
#include "gen_get_env.inc"
#include "gen_get_env_a.inc"
#include "gen_message_report_sync.inc"
#include "gen_open_stats_file.inc"

END MODULE GenMod_Core
