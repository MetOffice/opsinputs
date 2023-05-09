!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Output a message for a procedure from all PE's in PE order.
!-------------------------------------------------------------------------------

SUBROUTINE gen_message_report_sync (calling_routine, &
                                    messages,        &
                                    unit)

#ifndef SERIAL_SRC

USE GenMod_Control, ONLY: &
  mype,                   &
  nproc

USE mpl, ONLY:    &
  gc_int_kind,    &
  mpl_character,  &
  mpl_comm_world, &
  mpl_integer,    &
  mpl_success

USE OpsMod_MPLInterface, ONLY: &
  ops_mpl_gatherv

#endif

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)               :: calling_routine
CHARACTER(len=*), INTENT(IN)               :: messages(:)
INTEGER, OPTIONAL, INTENT(IN)              :: unit

#ifndef SERIAL_SRC

! Local declarations:
INTEGER                                    :: num_messages
INTEGER                                    :: num_messages_array(0:nproc - 1)
INTEGER                                    :: mpl_len(0:nproc - 1)
INTEGER                                    :: mpl_displacements(0:nproc - 1)
INTEGER                                    :: i
INTEGER                                    :: err_stat
CHARACTER(len=80)                          :: err_mess
CHARACTER(len=*), PARAMETER                :: RoutineName = "gen_message_report_sync"
CHARACTER(len=LEN (messages)), ALLOCATABLE :: all_messages(:)
INTEGER                                    :: len_strings

num_messages = SIZE (messages)
len_strings = LEN (messages)
mpl_len = 1
DO i = 0, nproc - 1
  mpl_displacements(i) = i
END DO

CALL ops_mpl_gatherv ((/num_messages/),   &
                      1_gc_int_kind,      &
                      mpl_integer,        &
                      num_messages_array, &
                      mpl_len,            &
                      mpl_displacements,  &
                      mpl_integer,        &
                      0_gc_int_kind,      &
                      mpl_comm_world,     &
                      err_stat)
IF (err_stat /= mpl_success) THEN
  WRITE (err_mess, '(A,I0)') "Error in mpl_gatherv (message lengths), err_stat = ", err_stat
  CALL gen_fail (RoutineName, &
                 err_mess)
END IF

IF (mype == 0) THEN
  ALLOCATE (all_messages(SUM (num_messages_array)))
  mpl_displacements(0) = 0
  DO i = 1, nproc - 1
    mpl_displacements(i) = mpl_displacements(i - 1) + num_messages_array(i - 1) * len_strings
  END DO
ELSE
  ALLOCATE (all_messages(0))
  mpl_displacements = 0
END IF

CALL ops_mpl_gatherv (messages,                         &
                      num_messages * len_strings,       &
                      mpl_character,                    &
                      all_messages,                     &
                      num_messages_array * len_strings, &
                      mpl_displacements,                &
                      mpl_character,                    &
                      0_gc_int_kind,                    &
                      mpl_comm_world,                   &
                      err_stat)
IF (err_stat /= mpl_success) THEN
  WRITE (err_mess, '(A,I0)') "Error in mpl_gatherv (messages), err_stat = ", err_stat
  CALL gen_fail (RoutineName, &
                 err_mess)
END IF

IF (mype == 0) THEN
  CALL gen_message (calling_routine, &
                    all_messages,    &
                    unit = unit)
END IF

#else

CALL gen_message (calling_routine, &
                  messages,        &
                  unit = unit)

#endif

END SUBROUTINE gen_message_report_sync
