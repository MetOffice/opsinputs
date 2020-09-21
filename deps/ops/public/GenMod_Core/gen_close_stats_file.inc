!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Close the file for the statistics output.
!-------------------------------------------------------------------------------

SUBROUTINE gen_close_stats_file (ErrorStatus, &
                                 content_msg)

USE GenMod_Control, ONLY: &
  ProduceHTML,            &
  mype

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(OUT)                   :: ErrorStatus
CHARACTER(len=*), INTENT(IN), OPTIONAL :: content_msg

! Local declarations:
CHARACTER(len=*), PARAMETER            :: this = 'gen_close_stats_file'
CHARACTER(len=255)                     :: io_msg
CHARACTER(len=255)                     :: read_msg
CHARACTER(len=6)                       :: close_status = 'KEEP'

IF (UseTrace) CALL gen_trace_entry (this)

ErrorStatus = 0
IF (mype == 0) THEN

  ! Set flag to delete the file if its only contents are "content_msg"
  IF (PRESENT (content_msg)) THEN
    BACKSPACE (UNIT = StatsOut, IOSTAT = ErrorStatus)
    IF (ErrorStatus /= 0) THEN
      WRITE (io_msg, '(A,I0)') 'IOSTAT=', ErrorStatus
      CALL gen_warn (this,                                                                                 &
                     TRIM (gen_stats_file_name) //                                                         &
                         ': cannot backspace, stats file will not be deleted if empty: ' // TRIM (io_msg))
    ELSE
      READ (StatsOut, '(A)') read_msg
      IF (ProduceHTML) THEN
        IF (TRIM (read_msg) == '<h3 style="color: red;">' // TRIM (content_msg) // '</h3>') THEN
          close_status = 'DELETE'
        END IF
      ELSE
        IF (TRIM (read_msg) == TRIM (content_msg)) close_status = 'DELETE'
      END IF
    END IF
  END IF

  ! Close the file
  CLOSE (UNIT = StatsOut,       &
         STATUS = close_status, &
#if FORTRAN_VERSION >= 2003
         IOMSG = io_msg, &
#endif
         IOSTAT = ErrorStatus)
  IF (ErrorStatus /= 0) THEN
#if FORTRAN_VERSION < 2003
    WRITE (io_msg, '(A,I0)') 'IOSTAT=', ErrorStatus
#endif
    CALL gen_fail (this,                                                              &
                   TRIM (gen_stats_file_name) // ': cannot close: ' // TRIM (io_msg))
  END IF
END IF

IF (UseTrace) CALL gen_trace_exit (this)

END SUBROUTINE gen_close_stats_file
