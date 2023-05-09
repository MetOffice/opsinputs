!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Reports trace results.
!
! Adapted from the UM TIMER routine, this subroutine displays the time spent
! locally in each subroutine, and in the calling tree below the subroutine.
! The timing information is collected in the trace routines.
!
! Sort the routines both by time spent locally in a subroutine, and by time
! in the calling tree below a subroutine, and output a report. See GTDP 5
! for more details.
!-------------------------------------------------------------------------------

SUBROUTINE gen_trace_report

USE GenMod_Control, ONLY: &
    GlobalGroup,          &
    DocumentationURL,     &
    mype,                 &
    nproc

USE GenMod_Platform, ONLY: &
    ClockTicksPerSecond

USE OpsMod_Gcom, ONLY: &
    gcg_cbcast,        &
    gcg_ibcast,        &
    gcg_isum,          &
    gcg_rsum

IMPLICIT NONE

! Local declarations:
INTEGER                     :: Loop1                        ! loop counter
INTEGER                     :: Loop2                        ! loop counter
INTEGER                     :: Loop3                        ! loop counter
INTEGER                     :: CountRate
INTEGER                     :: CountMax
INTEGER                     :: MasterNoRoutines(1)
INTEGER                     :: istat
INTEGER                     :: temp
INTEGER                     :: MinElapsedPos
INTEGER                     :: MaxElapsedPos
INTEGER                     :: MinCPUPos
INTEGER                     :: MaxCPUPos
INTEGER                     :: itemp1(MaxNoRoutines)
INTEGER                     :: MasterMaxHeap(0:nproc-1,MaxNoRoutines)
INTEGER                     :: MasterNoCalls(0:nproc-1,MaxNoRoutines)
INTEGER                     :: OverallNoCalls(MaxNoRoutines)
INTEGER, ALLOCATABLE        :: Index(:)
REAL                        :: TotalElapsedTime             !
REAL                        :: TotalElapsedTimeRecip        !
REAL                        :: TotalCPUTime(1)              !
REAL                        :: TotalCPUTimeRecip            !
REAL                        :: SpeedUp                      ! speed up factor
REAL                        :: PercentCPUTime               ! percentage in CPU time
REAL                        :: PercentElapsedTime           ! percentage in elapsed time
REAL                        :: rtemp1(MaxNoRoutines)
REAL                        :: MasterElapsedTime(0:nproc-1,MaxNoRoutines)
REAL                        :: MasterElapsedTimeLocal(0:nproc-1,MaxNoRoutines)
REAL                        :: MasterCPUTime(0:nproc-1,MaxNoRoutines)
REAL                        :: MasterCPUTimeLocal(0:nproc-1,MaxNoRoutines)
REAL                        :: OverallElapsedTime(MaxNoRoutines)
REAL                        :: OverallCPUTime(MaxNoRoutines)
CHARACTER(len=TraceNameLen) :: MasterTimerNames(MaxNoRoutines)

!==========================================================================
! start of code
!==========================================================================

IF (FlowTrace) THEN
  IF (TraceTimes) THEN
    CALL SYSTEM_CLOCK (COUNT = temp,           &
                       COUNT_MAX = CountMax,   &
                       COUNT_RATE = CountRate)
    IF (temp < BaseElapsedTime) THEN
      ! wrapped
      temp = temp + CountMax
    END IF

    TotalElapsedTime = temp - BaseElapsedTime ! on PE TracePEnum

    TotalCPUTime(1) = gen_cpu_time () - BaseCPUTime

    IF (.NOT. TraceAllPEs) THEN
      CALL gcg_rsum (1,                 &
                     GlobalGroup,       &
                     istat,             &
                     TotalCPUTime(1:1))
    END IF

    MasterElapsedTime(:,:) = 0.0
    MasterCPUTime(:,:) = 0.0
    MasterElapsedTimeLocal(:,:) = 0.0
    MasterCPUTimeLocal(:,:) = 0.0

  END IF

  MasterNoCalls(:,:) = 0
  MasterMaxHeap(:,:) = 0

  ! Ensure the lists from each PE match. Use the routine list from TracePEnum
  ! as the master copy

  MasterTimerNames(:) = TimerNames(:)
  MasterNoRoutines(1) = NoRoutines

  IF (.NOT. TraceAllPEs) THEN
    CALL gcg_ibcast (1000,             &
                     1,                &
                     TracePEnum,       &
                     GlobalGroup,      &
                     istat,            &
                     MasterNoRoutines)

    DO loop1 = 1, MasterNoRoutines(1)
      CALL gcg_cbcast (1001,                    &
                       TraceNameLen,            &
                       TracePEnum,              &
                       GlobalGroup,             &
                       istat,                   &
                       MasterTimerNames(loop1))
    END DO
  END IF

  DO Loop1 = 1, MasterNoRoutines(1)
    DO Loop2 = 1, NoRoutines
      IF (TimerNames(Loop2) == MasterTimerNames(Loop1)) THEN
        IF (TraceTimes) THEN
          MasterElapsedTime(mype,Loop1) = ElapsedTime(Loop2)
          MasterCPUTime(mype,Loop1) = CPUTime(Loop2)
          MasterElapsedTimeLocal(mype,Loop1) = ElapsedTimeLocal(Loop2)
          MasterCPUTimeLocal(mype,Loop1) = CPUTimeLocal(Loop2)
        END IF
        MasterNoCalls(mype,Loop1) = NoCalls(Loop2)
        MasterMaxHeap(mype,Loop1) = MaxHeap(Loop2)
        CYCLE
      END IF
    END DO
  END DO

  IF (.NOT. TraceAllPEs) THEN
    IF (TraceTimes) THEN
      CALL gcg_rsum (MasterNoRoutines(1) * nproc, &
                     GlobalGroup,                 &
                     istat,                       &
                     MasterElapsedTime(:,:))

      CALL gcg_rsum (MasterNoRoutines(1) * nproc, &
                     GlobalGroup,                 &
                     istat,                       &
                     MasterCPUTime(:,:))

      CALL gcg_rsum (MasterNoRoutines(1) * nproc, &
                     GlobalGroup,                 &
                     istat,                       &
                     MasterElapsedTimeLocal(:,:))

      CALL gcg_rsum (MasterNoRoutines(1) * nproc, &
                     GlobalGroup,                 &
                     istat,                       &
                     MasterCPUTimeLocal(:,:))
    END IF

    CALL gcg_isum (MasterNoRoutines(1) * nproc, &
                   GlobalGroup,                 &
                   istat,                       &
                   MasterNoCalls(:,:))
  END IF

  IF (TraceTimes) THEN
    !==========================================================================
    ! Sort subroutines into time order based on local Elapsed Time.
    ! All PEs should have the same sort order after the sum.
    !==========================================================================
    rtemp1(:) = SUM (MasterElapsedTimeLocal(:,:), DIM = 1)
  ELSE
    !==========================================================================
    ! Sort subroutines into time order based on number of calls.
    ! All PEs should have the same sort order after the sum.
    !==========================================================================
    rtemp1(:) = SUM (MasterNoCalls(:,:), DIM = 1)
  END IF

  ALLOCATE (Index(MasterNoRoutines(1)))

  CALL gen_trace_real_sort (rtemp1,              &
                            MasterNoRoutines(1), &
                            index)

  OverallNoCalls(:) = SUM (MasterNoCalls(:,:), DIM = 1)

  IF (TraceTimes) THEN
    OverallElapsedTime(:) = SUM (MasterElapsedTimeLocal(:,:), DIM = 1)
    OverallCPUTime(:) = SUM (MasterCPUTimeLocal(:,:), DIM = 1)

    IF (mype == TracePEnum .OR. TraceAllPEs) THEN

      ! Output timing information

      WRITE (TraceUnit, '("</pre><hr><a name=local><h2>Local Timing Summary</h2></a>")')

      WRITE (TraceUnit, '("<table border>")')
      IF (TraceAllPEs) THEN
        WRITE (TraceUnit, '("<tr><th>Routine Name<th>Calls<th colspan=2>Elapsed Time (seconds)")')
        WRITE (TraceUnit, '("<th colspan=2>CPU Time (seconds)<th>Speed")')
        WRITE (TraceUnit, '("<tr><th></th><th></th><th>Actual<th>%<th>Actual<th>%")')
        WRITE (TraceUnit, '("<th>1 PE")')
      ELSE
        WRITE (TraceUnit, '("<tr><th>Routine Name<th>Calls<th colspan=4>Elapsed Time (seconds)")')
        WRITE (TraceUnit, '("<th colspan=4>CPU Time (seconds)<th>Speed up")')
        WRITE (TraceUnit, &
           '("<tr><th></th><th>per PE</th><th>Average per PE<th>%<th>Minimum<th>Maximum <th>Total<th>%<th>Minimum<th>Maximum")')
        WRITE (TraceUnit, '("<th>",I0," PE")') nproc
      END IF

      !===================================================================================
      ! Ensure divide by zero checks are done outside of loops when dealing with loop
      ! invariant variables. Otherwise the divide may be optimised out of the loop and a
      ! 1.0 / 0.0 may occur.
      !===================================================================================

      IF (TotalCPUTime(1) > 0.0) THEN
        TotalCPUTimeRecip = 1.0 / TotalCPUTime(1)
      ELSE
        TotalCPUTimeRecip = 0.0
      END IF

      IF (TotalElapsedTime > 0.0) THEN
        TotalElapsedTimeRecip = 1.0 / TotalElapsedTime
      ELSE
        TotalElapsedTimeRecip = 0.0
      END IF

      DO loop1 = MasterNoRoutines(1), 1, -1
        Pointer = index(loop1)

        PercentCPUTime = 100.0 * OverallCPUTime(Pointer) * TotalCPUTimeRecip

        IF (TraceAllPEs) THEN
          PercentElapsedTime = 100.0 * OverallElapsedTime(Pointer) * TotalElapsedTimeRecip
        ELSE
          PercentElapsedTime = 100.0 * OverallElapsedTime(Pointer) * TotalElapsedTimeRecip / nproc
        END IF

        IF (OverallElapsedTime(Pointer) > 0.0) THEN
          IF (TraceAllPEs) THEN
            SpeedUp = (OverallCPUTime(Pointer)/ClockTicksPerSecond) / (OverallElapsedTime(Pointer)/CountRate)
          ELSE
            SpeedUp = (OverallCPUTime(Pointer)/ClockTicksPerSecond) / (OverallElapsedTime(Pointer) / REAL (CountRate * nproc))
          END IF
        ELSE
          SpeedUp = 0.0
        END IF

        ! This horrible solution as MINLOC does not take a DIM argument, so SUM
        ! is needed to convert the array to an integer

        MinElapsedPos = SUM (MINLOC (MasterElapsedTimeLocal(:,Pointer))) - 1
        MaxElapsedPos = SUM (MAXLOC (MasterElapsedTimeLocal(:,Pointer))) - 1
        MinCPUPos = SUM (MINLOC (MasterCPUTimeLocal(:,Pointer))) - 1
        MaxCPUPos = SUM (MAXLOC (MasterCPUTimeLocal(:,Pointer))) - 1

        WRITE (TraceUnit, '(7A)')           &
          "<tr><td><a href=",               &
          TRIM (DocumentationURL),          &
          "/",                              &
          TRIM (MasterTimerNames(Pointer)), & ! Subroutine name
          ".html>",                         &
          TRIM (MasterTimerNames(Pointer)), & ! Subroutine name
          "</a>"
        IF (TraceAllPEs) THEN
          WRITE (TraceUnit, '("<td>",I0,2("<td>",F10.2,"<td>",F10.2),"<td>",F10.2)') &
            OverallNoCalls(Pointer),                                     & ! Num calls on this PE
            OverallElapsedTime(Pointer)/REAL(CountRate),                 & ! Elapsed Time
            PercentElapsedTime,                                          & ! Percent Elapsed Time
            OverallCPUTime(Pointer)/ClockTicksPerSecond,                 & ! CPU time on this PE
            PercentCPUTime,                                              & ! Percent CPU time
            SpeedUp                                                        ! Speedup
        ELSE
          WRITE (TraceUnit, '("<td>",F10.2,2("<td>",F10.2,"<td>",F10.2,2("<td>",F10.2," on ",I0)),"<td>",F10.2)') &
            REAL (OverallNoCalls(Pointer)) / REAL (nproc),                      & ! Average number of calls per PE
            OverallElapsedTime(Pointer) / (nproc * REAL (CountRate)),           & ! Average Elapsed Time
            PercentElapsedTime,                                                 & ! Percent Elapsed Time
            MasterElapsedTimeLocal(MinElapsedPos,Pointer) / REAL (CountRate),   & ! Min average Elapsed Time
            MinElapsedPos,                                                      & ! Which PE
            MasterElapsedTimeLocal(MaxElapsedPos,Pointer) / REAL (CountRate),   & ! Max average Elapsed Time
            MaxElapsedPos,                                                      & ! Which PE
            OverallCPUTime(Pointer) / ClockTicksPerSecond,                      & ! CPU time
            PercentCPUTime,                                                     & ! Percent CPU time
            MasterCPUTimeLocal(MinCPUPos,Pointer) / REAL (ClockTicksPerSecond), & ! Min average CPU Time
            MinCPUPos,                                                          & ! Which PE
            MasterCPUTimeLocal(MaxCPUPos,Pointer) / REAL (ClockTicksPerSecond), & ! Max average CPU Time
            MaxCPUPos,                                                          & ! Which PE
            SpeedUp                                                               ! Speedup
        END IF
      END DO

      IF (TraceAllPEs) THEN
        WRITE (TraceUnit, &
             '("<tr><td><strong>Total: </strong>",I0,"</td><td></td><td><strong>",F10.2,"</strong>&
            &<td></td><td><strong>",F10.2,"</strong></td><td></td>")') &
          MasterNoRoutines(1), TotalElapsedTime / REAL (CountRate), &
          TotalCPUTime(1) / ClockTicksPerSecond
      ELSE
        WRITE (TraceUnit, &
              '("<tr><td><strong>Total: </strong>",I0,"</td><td><td><strong>",F10.2,"</strong><td></td><td></td><td></td>&
            &<td><strong>",F10.2,"</strong><td></td><td></td><td></td>")') &
        MasterNoRoutines(1), TotalElapsedTime / REAL (CountRate), &
        TotalCPUTime(1) / ClockTicksPerSecond
      END IF
      WRITE (TraceUnit, '("<td><strong>",F10.2,"</strong>")') &
        TotalElapsedTimeRecip * CountRate * TotalCPUTime(1) / ClockTicksPerSecond
      WRITE (TraceUnit, '("</table><p><p>")')

    END IF ! mype==TracePEnum .OR. TraceAllPEs

    !===================================================================================
    ! Sort subroutines into time order based on overall Elapsed Time.
    ! All PEs should have the same sort order after the sum.
    !===================================================================================

    rtemp1(:) = SUM (MasterElapsedTime(:,:), DIM = 1)

    CALL gen_trace_real_sort (rtemp1,              &
                              MasterNoRoutines(1), &
                              index)

    OverallElapsedTime(:) = SUM (MasterElapsedTime(:,:), DIM = 1)
    OverallCPUTime(:) = SUM (MasterCPUTime(:,:), DIM = 1)

    IF (mype == TracePEnum .OR. TraceAllPEs) THEN

      ! Output timing information

      WRITE (TraceUnit, '("</pre><hr><a name=overall><h2>Overall Timing Summary</h2></a>")')

      WRITE (TraceUnit, '("<table border>")')
      IF (TraceAllPEs) THEN
        WRITE (TraceUnit, '("<tr><th>Routine Name<th>Calls<th colspan=2>Elapsed Time (seconds)")')
        WRITE (TraceUnit, '("<th colspan=2>CPU Time (seconds)<th>Speed")')
        WRITE (TraceUnit, '("<tr><th></th><th></th><th>Actual<th>%<th>Actual<th>%")')
        WRITE (TraceUnit, '("<th> 1 PE")')
      ELSE
        WRITE (TraceUnit, '("<tr><th>Routine Name<th>Calls<th colspan=4>Elapsed Time (seconds)")')
        WRITE (TraceUnit, '("<th colspan=4>CPU Time (seconds)<th>Speed up")')
        WRITE (TraceUnit, &
         '("<tr><th></th><th>per PE</th><th>Average per PE<th>%<th>Minimum<th>Maximum<th>Total<th>%<th>Minimum<th>Maximum")')
        WRITE (TraceUnit, '("<th>",I0," PE")') nproc
      END IF

      DO loop1 = MasterNoRoutines(1), 1, -1
        Pointer = index(loop1)

        PercentCPUTime = 100.0 * OverallCPUTime(Pointer) * TotalCPUTimeRecip

        IF (TraceAllPEs) THEN
          PercentElapsedTime = 100.0 * OverallElapsedTime(Pointer) * TotalElapsedTimeRecip
        ELSE
          PercentElapsedTime = 100.0 * OverallElapsedTime(Pointer) * TotalElapsedTimeRecip / nproc
        END IF

        IF (OverallElapsedTime(Pointer) > 0.0) THEN
          IF (TraceAllPEs) THEN
            SpeedUp = (OverallCPUTime(Pointer) / ClockTicksPerSecond) / (OverallElapsedTime(Pointer) / CountRate)
          ELSE
            SpeedUp = (OverallCPUTime(Pointer) / ClockTicksPerSecond) / (OverallElapsedTime(Pointer) / REAL (nproc * CountRate))
          END IF
        ELSE
          SpeedUp = 0.0
        END IF

        ! This horrible solution as MINLOC does not take a DIM argument, so SUM
        ! is needed to convert the array to an integer

        MinElapsedPos = SUM (MINLOC (MasterElapsedTime(:,Pointer))) - 1
        MaxElapsedPos = SUM (MAXLOC (MasterElapsedTime(:,Pointer))) - 1
        MinCPUPos = SUM (MINLOC (MasterCPUTime(:,Pointer))) - 1
        MaxCPUPos = SUM (MAXLOC (MasterCPUTime(:,Pointer))) - 1

        WRITE (TraceUnit, '(7A)')           &
          "<tr><td><a href=",               &
          TRIM (DocumentationURL),          &
          "/",                              &
          TRIM (MasterTimerNames(Pointer)), &                  ! Subroutine name
          ".html>",                         &
          TRIM (MasterTimerNames(Pointer)), &                  ! Subroutine name
          "</a>"
        IF (TraceAllPEs) THEN
          WRITE (TraceUnit, '("<td>",I0,2("<td>",F10.2,"<td>",F10.2),"<td>",F10.2)') &
            OverallNoCalls(Pointer),                                                 & ! Num calls on this PE
            OverallElapsedTime(Pointer) / REAL (CountRate),                          & ! Elapsed Time
            PercentElapsedTime,                                                      & ! Percent Elapsed Time
            OverallCPUTime(Pointer) / ClockTicksPerSecond,                           & ! CPU time on this PE
            PercentCPUTime,                                                          & ! Percent CPU time
            SpeedUp                                                                    ! SpeedUp
        ELSE
          WRITE (TraceUnit, '("<td>",F10.2,2("<td>",F10.2,"<td>",F10.2,2("<td>",F10.2," on ",I0)),"<td>",F10.2)') &
            REAL (OverallNoCalls(Pointer)) / REAL (nproc),                  & ! Number of calls per PE
            OverallElapsedTime(Pointer) / (REAL (nproc * CountRate)),       & ! Average Elapsed Time
            PercentElapsedTime,                                             & ! Percent Elapsed Time
            MasterElapsedTime(MinElapsedPos,Pointer) / REAL (CountRate),    & ! Min average Elapsed Time
            MinElapsedPos,                                                  & ! Which PE
            MasterElapsedTime(MaxElapsedPos,Pointer) / REAL (CountRate),    & ! Max average Elapsed Time
            MaxElapsedPos,                                                  & ! Which PE
            OverallCPUTime(Pointer) / REAL (ClockTicksPerSecond),           & ! CPU time
            PercentCPUTime,                                                 & ! Percent CPU time
            MasterCPUTime(MinCPUPos,Pointer) / REAL (ClockTicksPerSecond),  & ! Min average CPU Time
            MinCPUPos,                                                      & ! Which PE
            MasterCPUTime(MaxCPUPos,Pointer) / REAL (ClockTicksPerSecond),  & ! Max average CPU Time
            MaxCPUPos,                                                      & ! Which PE
            SpeedUp                                                           ! SpeedUp
        END IF
      END DO

      IF (TraceAllPEs) THEN
        WRITE (TraceUnit, &
             '("<tr><td><strong>Total: </strong>",I0,"</td><td></td><td><strong>",F10.2,"</strong></td>&
            &<td></td><td><strong>",F10.2,"</strong></td><td></td>")') &
          MasterNoRoutines(1), TotalElapsedTime / REAL (CountRate), &
          TotalCPUTime(1) / ClockTicksPerSecond
      ELSE
        WRITE (TraceUnit, &
             '("<tr><td><strong>Total: </strong>",I0,"</td><td><td><strong>",F10.2,"</strong><td></td><td></td><td></td>&
            &<td><strong>",F10.2,"</strong><td></td><td></td><td></td>")') &
          MasterNoRoutines(1), TotalElapsedTime / REAL (CountRate), &
          TotalCPUTime(1) / ClockTicksPerSecond
      END IF

      WRITE (TraceUnit, '("<td><strong>",F10.2,"</strong>")') &
        TotalElapsedTimeRecip * TotalCPUTime(1) * CountRate / ClockTicksPerSecond

      WRITE (TraceUnit, '("</table>")')
    END IF ! mype==TracePEnum .OR. TraceAllPEs
  ELSE
    IF (mype == TracePEnum .OR. TraceAllPEs) THEN
      ! Output routines in order of called frequency
      WRITE (TraceUnit, '("</pre><hr><a name=local><h2>Call Frequency</h2></a>")')
      WRITE (TraceUnit, '("<table border>")')
      WRITE (TraceUnit, '("<tr><th>Routine Name<th>Calls")')

      DO loop1 = MasterNoRoutines(1), 1, -1
        Pointer = index(loop1)
        WRITE (TraceUnit, '(7A)')           &
          "<tr><td><a href=",               &
          TRIM (DocumentationURL),          &
          "/",                              &
          TRIM (MasterTimerNames(Pointer)), & ! Subroutine name
          ".html>",                         &
          TRIM (MasterTimerNames(Pointer)), & ! Subroutine name
          "</a>"
        WRITE (TraceUnit, '("<td>",I0)') OverallNoCalls(Pointer)
      END DO

      WRITE (TraceUnit, '("<tr><td><strong>Total: </strong>",I0)') MasterNoRoutines(1)
      WRITE (TraceUnit, '("</table><p><p>")')
    END IF
  END IF ! TraceTimes

  !===================================================================================
  ! Sort subroutines into memory use order by max memory on one PE
  !===================================================================================

  IF (TraceMemory) THEN

    IF (.NOT. TraceAllPEs) THEN
      CALL gcg_isum (MasterNoRoutines(1) * nproc, &
                     GlobalGroup,                 &
                     istat,                       &
                     MasterMaxHeap(:,:))
    END IF

    itemp1(:) = MAXVAL (MasterMaxHeap(:,:), DIM = 1)

    CALL gen_trace_int_sort (itemp1,              &
                             MasterNoRoutines(1), &
                             index)

    IF (mype == TracePEnum .OR. TraceAllPEs) THEN

      WRITE (TraceUnit, '("<hr><a name=memory><h2>Maximum Memory Usage Summary</h2></a>")')
      IF (TraceRelMemory) THEN
        WRITE (TraceUnit, '("All figures include a base memory value of ",I0," bytes")') BaseMemory
      END IF
      WRITE (TraceUnit, '("<table border>")')
      WRITE (TraceUnit, '("<tr><th>Routine<th>Max in any PE (bytes)")')
      WRITE (TraceUnit, '("<th>Overall (bytes)<th>Average per PE (bytes)")')

      DO loop1 = MasterNoRoutines(1), 1, -1
        Pointer = index(loop1)
        WRITE (TraceUnit, '("<tr><td><a href=",A,"/",A,".html>",A,"</a><td>",I0,"<td>",I0,"<td>",I0)') &
          TRIM (DocumentationURL), TRIM (MasterTimerNames(Pointer)), TRIM (MasterTimerNames(Pointer)), &
          MAXVAL (MasterMaxHeap(:,Pointer)), SUM (MasterMaxHeap(:,Pointer)), SUM (MasterMaxHeap(:,Pointer)) / nproc
      END DO
      WRITE (TraceUnit, '("</table>")')
    END IF
  ELSE
    IF (mype == TracePEnum .OR. TraceAllPEs) THEN

      WRITE (TraceUnit, '("<hr><a name=memory><h2>Maximum memory usage (in bytes) for routines</h2></a>")')
      WRITE (TraceUnit, '("Memory tracing is not active")')

    END IF
  END IF

  !===================================================================================
  ! Show detailed statistics for named routines
  !===================================================================================

  IF (mype == TracePEnum .OR. TraceAllPEs) THEN
    WRITE (TraceUnit, '("<hr><a name=special><h2>Special Statistics for Named Routines</h2></a>")')
    DO loop1 = 1, SIZE (TraceRoutinesSpecial)
      DO loop2 = 1, MasterNoRoutines(1)
        IF (TRIM (MasterTimerNames(loop2)) == TRIM (TraceRoutinesSpecial(loop1))) THEN
          WRITE (TraceUnit, '("<h2>Calls for ",A,"</h2>")') TRIM (TraceRoutinesSpecial(loop1))
          WRITE (TraceUnit, '(A)') "<table border>"
          DO loop3 = 0, nproc - 1, TraceDisplayStride
            WRITE (TraceUnit, '("<tr><th>",I0," - ",I0,"</th>",("<td>",I0,"</td>"))') &
              loop3, loop3 + TraceDisplayStride - 1, MasterNoCalls(loop3:loop3 + TraceDisplayStride - 1,loop2)
            WRITE (TraceUnit, '(A)') "</tr>"
          END DO
          WRITE (TraceUnit, '(A)') "</table>"

          IF (TraceTimes) THEN
            WRITE (TraceUnit, '("<h2>Inclusive elapsed time for ",A,"</h2>")') &
              TRIM (TraceRoutinesSpecial(loop1))
            WRITE (TraceUnit, '(A)') "<table border>"
            DO loop3 = 0, nproc - 1, TraceDisplayStride
              WRITE (TraceUnit, '("<tr><th>",I0," - ",I0,"</th>",("<td>",F10.2,"</td>"))') &
                loop3, loop3 + TraceDisplayStride - 1,                                     &
                MasterElapsedTime(loop3:loop3 + TraceDisplayStride - 1,loop2) / REAL (CountRate)
              WRITE (TraceUnit, '(A)') "</tr>"
            END DO
            WRITE (TraceUnit, '(A)') "</table>"

            WRITE (TraceUnit, '("<h2>Inclusive CPU time for ",A,"</h2>")') &
              TRIM (TraceRoutinesSpecial(loop1))
            WRITE (TraceUnit, '(A)') "<table border>"
            WRITE (TraceUnit, '(A)') "<table border>"
            DO loop3 = 0, nproc - 1, TraceDisplayStride
              WRITE (TraceUnit, '("<tr><th>",I0," - ",I0,"</th>",("<td>",F10.2,"</td>"))') &
                loop3, loop3 + TraceDisplayStride - 1,                                     &
                MasterCPUTime(loop3:loop3 + TraceDisplayStride - 1,loop2) / REAL (ClockTicksPerSecond)
              WRITE (TraceUnit, '(A)') "</tr>"
            END DO
            WRITE (TraceUnit, '(A)') "</table>"
          END IF ! TraceTimes
        END IF ! routine name matches
      END DO ! traced routine list
    END DO ! special routine list
  END IF ! I'm writing out

  IF (mype == TracePEnum .OR. TraceAllPEs) THEN
    WRITE (TraceUnit, '(A)') "</body></html>"
    CLOSE (TraceUnit)
    IF (ActiveCallTree) CLOSE (CallTreeUnit)
  END IF
  DEALLOCATE (index)

  DEALLOCATE (NoCalls)
  DEALLOCATE (NoCallsBody)
  DEALLOCATE (CalledBy)
  DEALLOCATE (MaxHeap)
  DEALLOCATE (EntryHeap)
  DEALLOCATE (CPUTimeStart)
  DEALLOCATE (CPUTime)
  DEALLOCATE (CPUTimeLocal)
  DEALLOCATE (CPUTimeThisCall)
  DEALLOCATE (ElapsedTimeStart)
  DEALLOCATE (ElapsedTime)
  DEALLOCATE (ElapsedTimeLocal)
  DEALLOCATE (ElapsedTimeThisCall)
  DEALLOCATE (TimerNames)
  DEALLOCATE (TraceNames)

END IF

END SUBROUTINE gen_trace_report
