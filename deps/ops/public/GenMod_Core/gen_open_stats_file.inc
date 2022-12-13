!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Open the file for the statistics output.
!-------------------------------------------------------------------------------

SUBROUTINE gen_open_stats_file (Message,     &
                                ErrorStatus)

USE GenMod_Control, ONLY: &
  ProduceHTML,            &
  mype,                   &
  StatsAppend

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)          :: Message
INTEGER, INTENT(OUT)                  :: ErrorStatus

! Local declarations:
CHARACTER(len=*), PARAMETER           :: this = 'gen_open_stats_file'
CHARACTER(len=*), PARAMETER           :: gen_stats_file_env = 'GEN_STDSTATS_FILE'
CHARACTER(len=*), PARAMETER           :: rose_task_log_root_env = 'ROSE_TASK_LOG_ROOT'
CHARACTER(len=*), PARAMETER           :: gen_stats_file_name_default = 'stats'
#ifndef REC_LEN
#define REC_LEN 2000
#endif
INTEGER, PARAMETER                    :: rec_len = REC_LEN

#if FORTRAN_VERSION >= 2003
INTEGER, PARAMETER                    :: len_rose_task_log_root = LEN (gen_stats_file_name) - LEN (gen_stats_file_name_default) - 1
CHARACTER(len=len_rose_task_log_root) :: rose_task_log_root
#else
CHARACTER(len=1000)                   :: rose_task_log_root
#endif

CHARACTER(len=255)                    :: io_msg
INTEGER                               :: env_len
INTEGER                               :: i_status

IF (UseTrace) CALL gen_trace_entry (this)

ErrorStatus = 0
IF (mype == 0) THEN
  CALL gen_get_env (gen_stats_file_env,  &
                    env_len,             &
                    gen_stats_file_name, &
                    i_status)
  IF (i_status /= 0 .OR. env_len <= 0) THEN
    CALL gen_get_env (rose_task_log_root_env, &
                      env_len,                &
                      rose_task_log_root,     &
                      i_status)
    IF (i_status == 0 .OR. env_len <= 0) THEN
      gen_stats_file_name = TRIM (rose_task_log_root) // '.' // TRIM (gen_stats_file_name_default)
    ELSE
      gen_stats_file_name = gen_stats_file_name_default
    END IF
  END IF

  IF (StatsAppend) THEN
    OPEN (UNIT = StatsOut,            &
          FILE = gen_stats_file_name, &
          POSITION = 'APPEND',        &
          RECL = rec_len,             &
#if FORTRAN_VERSION >= 2003
          IOMSG = io_msg,             &
#endif
          IOSTAT = ErrorStatus)
  ELSE
    OPEN (UNIT = StatsOut,    &
          FILE = "fort.7",    &
          STATUS = 'REPLACE', &
          RECL = rec_len,     &
#if FORTRAN_VERSION >= 2003
          IOMSG = io_msg,     &
#endif
          IOSTAT = ErrorStatus)
  END IF

  IF (ErrorStatus /= 0) THEN
#if FORTRAN_VERSION < 2003
    WRITE (io_msg, '(A,I0)') 'IOSTAT=', ErrorStatus
#endif
    CALL gen_fail (this,                                                             &
                   TRIM (gen_stats_file_name) // ': cannot open: ' // TRIM (io_msg))
  END IF

  ! Initial message:
  ! FIXME: why?
  IF (ProduceHTML) THEN
    WRITE (StatsOut, '(A)') '<h3 style="color: red;">' // TRIM (Message) // '</h3>'
  ELSE
    WRITE (StatsOut, '(A)') TRIM (Message)
  END IF
END IF

IF (UseTrace) CALL gen_trace_exit (this)

END SUBROUTINE gen_open_stats_file
