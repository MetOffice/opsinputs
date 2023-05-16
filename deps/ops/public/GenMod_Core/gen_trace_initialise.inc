!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Set up trace routines.
!
! Open the trace output file, and set up the trace priority level, and
! initial indent level. Set up the timing information. See GTDP 5.
!
! The trace control variables are read from environment variables:
!     GEN_TRACEDIR
!     GEN_TRACENAMES
!-------------------------------------------------------------------------------

SUBROUTINE gen_trace_initialise (Name)

USE GenMod_Control, ONLY: &
  DocumentationURL,       &
  mype

USE GenMod_Platform, ONLY: &
  Gen_FlushUnit,           &
  Gen_Memory,              &
  Gen_SetupCPUTime,        &
  GenFn_ThreadMax

USE parkind1, ONLY: &
  jpim

USE yomhook, ONLY: &
  dr_hook,         &
  lhook

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN), OPTIONAL :: Name

! Local declarations:
CHARACTER(len=*), PARAMETER            :: gen_trace_dir_env = 'GEN_TRACEDIR'
CHARACTER(len=*), PARAMETER            :: gen_trace_file_env = 'GEN_TRACEFILE'
CHARACTER(len=*), PARAMETER            :: gen_trace_file_prefix_env = 'GEN_TRACE_FILE_PREFIX'
CHARACTER(len=*), PARAMETER            :: rose_task_log_root_env = 'ROSE_TASK_LOG_ROOT'
CHARACTER(len=*), PARAMETER            :: Trace_Names_env = 'GEN_TRACENAMES'
CHARACTER(len=*), PARAMETER            :: RoutineName = "gen_trace_initialise"
INTEGER, PARAMETER                     :: RecordLength = 1000     ! Record length on json file (lines must not wrap)
INTEGER                                :: IOStatus             ! I/O return code
INTEGER                                :: len_env
INTEGER                                :: CountRate
INTEGER                                :: FileLoc              ! Location in file name
CHARACTER(len=MaxFileNameLen)          :: trace_dir          ! trace directory
CHARACTER(len=MaxFileNameLen)          :: trace_file_base    ! trace file basename
CHARACTER(len=MaxFileNameLen)          :: trace_file         ! trace filename
CHARACTER(len=MaxFileNameLen)          :: trace_file_prefix  ! trace file prefix
CHARACTER(len=MaxFileNameLen)          :: trace_js           ! file name for calling tree
CHARACTER(len=MaxFileNameLen)          :: trace_js_base      ! base file name for calling tree
CHARACTER(len=MaxFileNameLen)          :: JSFileName         ! Nmae of java-script file
CHARACTER(len=MaxFileNameLen)          :: JSFileNameBase     ! Nmae of java-script file

!===========================================================================

! Start of code

IOStatus = 0

!-----------------------------------------------------------------
! 2. Read the trace control namelist.
!-----------------------------------------------------------------

CALL gen_read_trace_nl

!-----------------------------------------------------------------
! 3. If there is no tracing, go home.
!-----------------------------------------------------------------

IF (lhook .AND. .NOT. ALLOCATED (dr_hook_handle_stack)) THEN
  ALLOCATE (dr_hook_handle_stack(max_dr_hook_handles,0:GenFn_ThreadMax () - 1))
END IF
IF (lhook .AND. .NOT. ALLOCATED (dr_hook_pointer)) THEN
  ALLOCATE (dr_hook_pointer(0:GenFn_ThreadMax () - 1))
  dr_hook_pointer(:) = 1
END IF

IF (lhook) THEN
  CALL Dr_Hook (Name,                                       &
                0_jpim,                                     &
                dr_hook_handle_stack(dr_hook_pointer(0),0))
  dr_hook_pointer(0) = dr_hook_pointer(0) + 1
END IF

!-----------------------------------------------------------------
! 4. Set up flow tracing.
!-----------------------------------------------------------------

IF (FlowTrace) THEN

  ALLOCATE (NoCalls(MaxNoRoutines))
  ALLOCATE (NoCallsBody(MaxNoRoutines))
  ALLOCATE (CalledBy(MaxNoRoutines))
  ALLOCATE (MaxHeap(MaxNoRoutines))
  ALLOCATE (EntryHeap(MaxNoRoutines))
  ALLOCATE (CPUTimeStart(MaxNoRoutines))
  ALLOCATE (CPUTime(MaxNoRoutines))
  ALLOCATE (CPUTimeLocal(MaxNoRoutines))
  ALLOCATE (CPUTimeThisCall(MaxNoRoutines))
  ALLOCATE (ElapsedTimeStart(MaxNoRoutines))
  ALLOCATE (ElapsedTime(MaxNoRoutines))
  ALLOCATE (ElapsedTimeLocal(MaxNoRoutines))
  ALLOCATE (ElapsedTimeThisCall(MaxNoRoutines))
  ALLOCATE (TimerNames(MaxNoRoutines))
  ALLOCATE (TraceNames(MaxNoRoutines))

  !-----------------------------------------------------------------
  ! Get list of routines that make trace active (optional)
  !-----------------------------------------------------------------

  CALL gen_get_env_a (Trace_Names_env, & ! I:
                      NoTraceNames,    & ! O:
                      TraceNames)        ! O:

  IF (NoTraceNames <1) THEN
    ! allow to trigger on first call
    NoTraceNames = 0
  END IF

  !-----------------------------------------------------------------
  ! Open trace output file.
  !-----------------------------------------------------------------

  IF (mype == TracePEnum .OR. TraceAllPEs) THEN
    CALL gen_get_unit (TraceUnit)

    IF (ActiveCallTree) THEN
      CALL gen_get_unit (CallTreeUnit)
    END IF

    CALL gen_get_env (gen_trace_dir_env, &
                      len_env,           &
                      trace_dir)
    CALL gen_get_env (gen_trace_file_env, &
                      len_env,            &
                      trace_file_base)
    trace_file_prefix = ""
    IF (trace_dir /= '' .OR. trace_file_base /= '') THEN
      IF (trace_dir == '') trace_dir = '.'
      IF (trace_file_base == '') trace_file_base = 'trace.html'
      IF (trace_file_base(1:1) == '/') THEN ! GEN_TRACEFILE is an absolute path
        IF (TraceAllPEs) THEN
          ! It is too much work for now to break up a full path trying to generate
          ! processor-specific trace filenames; for runs generating multiple
          ! randomly named trace files it may be difficult to guarantee that our
          ! new filename does not conflict with one that already exists.
          WRITE (ErrorOut, '(A)') "Cannot trace on all PEs when setting a " // &
                                  "full path for the trace output filename"
          TraceAllPEs = .FALSE.
        END IF
        trace_file = TRIM (trace_file_base)
      ELSE
        IF (TraceAllPEs) THEN
          WRITE (trace_file, '(2A,I0,2A)') TRIM (trace_dir), '/', mype, '-', TRIM (trace_file_base)
        ELSE
          trace_file = TRIM (trace_dir) // '/' // TRIM (trace_file_base)
        END IF
      END IF
    ELSE
      IF (trace_dir == '') trace_dir = '.'
      CALL gen_get_env (gen_trace_file_prefix_env, &
                        len_env,                   &
                        trace_file_prefix)
      IF (len_env <= 0) THEN
        CALL gen_get_env (rose_task_log_root_env, &
                          len_env,                &
                          trace_file_prefix)
      END IF
      IF (len_env <= 0) THEN
        trace_file_prefix = './gen'
      END IF
      IF (TraceAllPEs) THEN
        WRITE (trace_file, '(2A,I0,A)') TRIM (trace_file_prefix), '.trace.', mype, '.html'
      ELSE
        trace_file = TRIM (trace_file_prefix) // '.trace.html'
      END IF
    END IF

    ! Have to re-check incase TraceAllPEs was reset (see above)
    IF (mype == TracePEnum .OR. TraceAllPEs) THEN
      OPEN (UNIT = TraceUnit,   & ! I:
            FILE = trace_file,  & ! I:
            STATUS = "REPLACE", & ! I:
            ACTION = "WRITE",   & ! I:
            IOSTAT = IOStatus)    ! O:

      IF (IOStatus /= 0) THEN
        CALL gen_fail (RoutineName,                                   &
                       "Cannot open trace file "// TRIM (trace_file))
      END IF

      IF (ActiveCallTree) THEN

        trace_js = trace_file(1:INDEX (trace_file, '.html') - 1) // '.js'   ! Create a new file name for the calling tree
        IF (trace_file_prefix /= "") THEN
          JSFileName = TRIM (trace_file_prefix) // ".calling_tree.js"
          JSFileNameBase = JSFileName(INDEX (trace_file, '/', BACK = .TRUE.) + 1:LEN_TRIM (JSFileName))
        ELSE
          JSFileName = trace_file(1:INDEX (trace_file, '.html') - 1) // ".calling_tree.js"
          JSFileNameBase = JSFileName(INDEX (JSFileName, '/', BACK = .TRUE.) + 1:LEN_TRIM (JSFileName))
        END IF
        FileLoc = INDEX (trace_js, '/', BACK = .TRUE.)
        IF (FileLoc >= 0) THEN
          trace_js_base = TRIM (trace_js(FileLoc + 1:MaxFileNameLen))
        ELSE
          trace_js_base = TRIM (trace_js)
        END IF
        ! Replace % signs with %25 so that url link works correctly

        trace_js_base = gen_replace_text (MaxFileNameLen, trace_js_base, '%', '%25')
        JSFileNameBase = gen_replace_text (MaxFileNameLen, JSFileNameBase, '%', '%25')
        OPEN (UNIT = CallTreeUnit,   & ! I:
              FILE = trace_js,       & ! I:
              STATUS = "REPLACE",    & ! I:
              ACTION = "WRITE",      & ! I:
              RECL = RecordLength,   & ! I:
              IOSTAT = IOStatus)       ! O:

        IF (IOStatus /= 0) THEN
          CALL gen_fail (RoutineName,                                    &
                         "Cannot open trace js file "// TRIM (trace_js))
        END IF

        IF ((TraceAllPEs .AND. mype == 0) .OR. .NOT. TraceAllPEs) THEN
          CALL gen_write_trace_js_file (JSFileName)
        END IF
      END IF

    END IF
  END IF ! mype==TracePEnum .OR. TraceAllPEs

  !-----------------------------------------------------------------
  ! Set up timing and memory usage
  !-----------------------------------------------------------------

  CPUTimeStart(:) = 0.0
  ElapsedTimeStart(:) = 0.0
  ElapsedTime(:) = 0.0
  ElapsedTimeLocal(:) = 0.0
  CPUTime(:) = 0.0
  CPUTimeLocal(:) = 0.0
  NoCalls(:) = 0
  NoCallsBody(:) = 0
  CalledBy(:) = 0
  MaxHeap(:) = 0
  TimerNames(:) = ""

  CALL SYSTEM_CLOCK (COUNT = BaseElapsedTime, &
                     COUNT_RATE = CountRate)

  IF (CountRate == 0) THEN
    ! Override whatever the user chose if the system clock isn't available.
    TraceTimes = .FALSE.
  ELSE
    Count0 = BaseElapsedTime
  END IF

  IF (TraceTimes) THEN
    CALL Gen_SetupCPUTime
    BaseCPUTime = gen_cpu_time ()
  END IF

  IF (TraceMemory .AND. TraceRelMemory) THEN
    CALL Gen_Memory (BaseMemory)
  END IF

  ! start trace output here so memory calculations aren't distorted
  ! by IO buffer being grabbed later

  IF (mype == TracePEnum .OR. TraceAllPEs) THEN
    WRITE (TraceUnit, '(A)') "<html><head><title>Trace Output</title></head>"
    IF (ActiveCallTree) THEN
      WRITE (TraceUnit, '(A)') '<link rel="stylesheet" type="text/css" href="http://www-nwp/stylesheet.css"/>'
      WRITE (TraceUnit, '(A)') '<script type="text/javascript" src="http://www-nwp/include/jquery-1.min.js"></script>'
      WRITE (TraceUnit, '(A)') '<script type="text/javascript" src="' // TRIM (JSFileNameBase) // '"></script>'
      WRITE (TraceUnit, '(A)') '<script type="text/javascript" src="' // TRIM (trace_js_base) // '"></script>'
    END IF
    WRITE (TraceUnit, '(A)') "<body><h1>Trace Output</h1>"
    WRITE (TraceUnit, '(A)') "<ul>"
    WRITE (TraceUnit, '(A)') "<li><a href=#tree>Calling Tree</a>"
    IF (TraceTimes) THEN
      WRITE (TraceUnit, '(A)') "<li><a href=#local>Local Timing Summary</a>"
      WRITE (TraceUnit, '(A)') "<li><a href=#overall>Overall Timing Summary</a>"
    ELSE
      WRITE (TraceUnit, '(A)') "<li><a href=#local>Call Frequency</a>"
    END IF
    WRITE (TraceUnit, '(A)') "<li><a href=#memory>Maximum Memory Usage Summary</a>"
    WRITE (TraceUnit, '(A)') "<li><a href=#special>Special Statistics for Named Routines</a>"
    WRITE (TraceUnit, '(A)') "</ul>"
    WRITE (TraceUnit, '(A)') "Links are to documentation set"
    WRITE (TraceUnit, '(A)') "<a href=",TRIM(DocumentationURL),">",TRIM(DocumentationURL),"</a>"
    WRITE (TraceUnit, '(A)') ", <strong>NOT</strong> to exact code under test.<p>"
    WRITE (TraceUnit, '(A)') "Times are in seconds, memory in bytes.<p>"
    IF (TraceAllPEs) THEN
      WRITE (TraceUnit, '(A)') " Tracing is being done on all PEs"
    ELSE
      WRITE (TraceUnit, '(A,I0)') " Tracing is being done on PE ",TracePEnum
    END IF
    WRITE (TraceUnit, '(A)') "<a name=tree><h2>Calling Tree</h2></a><pre>"
    IF (ActiveCallTree) THEN
      WRITE (TraceUnit, '(A)') '<div id="calling-tree"></div>'
      WRITE (CallTreeUnit, '(A)') "CallingTree.Hierarchy ="
    END IF
    IF (TraceFlush) CALL Gen_FlushUnit (TraceUnit)
  END IF ! mype==TracePEnum .OR. TraceAllPEs
END IF

!-----------------------------------------------------------------
! 5. Set up back tracing.
!-----------------------------------------------------------------

IF (BackTrace) THEN
  ALLOCATE (TraceStack(TraceMaxStackPointer))
END IF

IF (lhook) THEN
  dr_hook_pointer(0) = dr_hook_pointer(0) - 1
  CALL Dr_Hook (Name,                                       &
                1_jpim,                                     &
                dr_hook_handle_stack(dr_hook_pointer(0),0))
END IF

UseTrace = lhook .OR. FlowTrace .OR. BackTrace

END SUBROUTINE gen_trace_initialise
