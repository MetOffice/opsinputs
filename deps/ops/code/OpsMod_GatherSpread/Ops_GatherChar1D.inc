!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Gather character date from PEs.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_GatherChar1D (Obs,           &
                             LocalCharData, &
                             TotalCharData)

USE GenMod_Control, ONLY: &
    nproc,                &
    mype

USE GenMod_Core, ONLY: &
    gen_fail,          &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE mpl, ONLY:      &
    mpl_character,  &
    mpl_comm_world, &
    mpl_success

#ifndef SERIAL_SRC
USE OpsMod_MPLInterface, ONLY: &
    ops_mpl_alltoallv
#endif

USE OpsMod_ObsInfo, ONLY: &
    OB_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (OB_type), INTENT(IN)  :: Obs
CHARACTER(len=*), POINTER   :: LocalCharData(:)
CHARACTER(len=*), POINTER   :: TotalCharData(:)

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_GatherChar1D"
INTEGER                     :: i
INTEGER                     :: send_counts(0:nproc - 1)
INTEGER                     :: receive_counts(0:nproc - 1)
INTEGER                     :: send_displacements(0:nproc - 1)
INTEGER                     :: receive_displacements(0:nproc - 1)
INTEGER                     :: Errstat
CHARACTER(len=80)           :: ErrMess
INTEGER                     :: NumObsTotal
INTEGER                     :: LenChar
INTEGER                     :: Batch

IF (UseTrace) CALL gen_trace_entry (RoutineName)

NumObsTotal = Obs % Header % NumObsTotal

lenchar = LEN (LocalCharData)

ALLOCATE (TotalCharData(NumObsTotal))
TotalCharData = ""

#ifndef SERIAL_SRC

IF (gather_by_batch) THEN
  DO batch = 1, Obs % Header % NumCXBatches
    send_counts = Obs % Header % ObsPerBatchPerPE(batch,mype) * lenchar
    receive_counts(:) = Obs % Header % ObsPerBatchPerPE(batch,:) * lenchar
    IF (batch > 1) THEN
      send_displacements = SUM (Obs % Header % ObsPerBatchPerPE(1:batch - 1,mype)) * lenchar
      receive_displacements = SUM (Obs % Header % ObsPerBatchPerPE(1:batch - 1,:)) * lenchar
    ELSE
      send_displacements = 0
      receive_displacements = 0
    END IF
    DO i = 1,nproc - 1
      receive_displacements(i) = receive_displacements(i - 1) + Obs % Header % ObsPerBatchPerPE(batch,i - 1) * lenchar
    END DO

    CALL ops_mpl_alltoallv (LocalCharData,         &
                            send_counts,           &
                            send_displacements,    &
                            mpl_character,         &
                            TotalCharData,         &
                            receive_counts,        &
                            receive_displacements, &
                            mpl_character,         &
                            mpl_comm_world,        &
                            ErrStat)
    IF (Errstat /= mpl_success) THEN
      WRITE (ErrMess, '(A,I0)') "Error in mpl_alltoallv, Errstat = ", Errstat
      CALL gen_fail (RoutineName, &
                     ErrMess)
    END IF
  END DO
ELSE
  send_counts = Obs % Header % ObsPerPE(mype) * lenchar
  receive_counts(:) = Obs % Header % ObsPerPE(:) * lenchar
  send_displacements = 0
  receive_displacements = 0
  DO i = 1,nproc - 1
    receive_displacements(i) = receive_displacements(i - 1) + Obs % Header % ObsPerPE(i - 1) * lenchar
  END DO

  CALL ops_mpl_alltoallv (LocalCharData,         &
                          send_counts,           &
                          send_displacements,    &
                          mpl_character,         &
                          TotalCharData,         &
                          receive_counts,        &
                          receive_displacements, &
                          mpl_character,         &
                          mpl_comm_world,        &
                          ErrStat)
  IF (Errstat /= mpl_success) THEN
    WRITE (ErrMess, '(A,I0)') "Error in mpl_alltoallv, Errstat = ", Errstat
    CALL gen_fail (RoutineName, &
                   ErrMess)
  END IF
END IF

#else

TotalCharData(1:NumObsTotal) = LocalCharData(1:NumObsTotal)

#endif

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_GatherChar1D
