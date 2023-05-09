!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Gathers a real 2D array from all PE's to all PE's.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_GatherReal2D (Obs,       &
                             LocalReal, &
                             TotalReal)

USE GenMod_Control, ONLY: &
    nproc,                &
    mype

USE GenMod_Core, ONLY: &
    gen_fail,          &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE mpl, ONLY:      &
    mpl_comm_world, &
    mpl_real,       &
    mpl_success

#ifndef SERIAL_SRC
USE OpsMod_MPLInterface, ONLY: &
    ops_mpl_alltoallv
#endif

USE OpsMod_ObsInfo, ONLY: &
    OB_type

IMPLICIT NONE

! Subroutine arguments:
TYPE(OB_type), INTENT(IN)   :: Obs         ! observation data
REAL, TARGET, INTENT(IN)    :: LocalReal(:,:)
REAL, POINTER               :: TotalReal(:,:)

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_GatherReal2D"
INTEGER                     :: i
INTEGER                     :: send_counts(0:nproc - 1)
INTEGER                     :: receive_counts(0:nproc - 1)
INTEGER                     :: send_displacements(0:nproc - 1)
INTEGER                     :: receive_displacements(0:nproc - 1)
INTEGER                     :: Errstat
CHARACTER(len=80)           :: ErrMess
INTEGER                     :: NumObsTotal
INTEGER                     :: Batch

IF (UseTrace) CALL gen_trace_entry (RoutineName)

NumObsTotal = Obs % Header % NumObsTotal

ALLOCATE (TotalReal(NumObsTotal,SIZE (LocalReal,dim = 2)))

TotalReal(:,:) = 0.0

#ifndef SERIAL_SRC

IF (gather_by_batch) THEN
  DO batch = 1, Obs % Header % NumCXBatches
    send_counts = Obs % Header % ObsPerBatchPerPE(batch,mype)
    receive_counts(:) = Obs % Header % ObsPerBatchPerPE(batch,:)
    IF (batch > 1) THEN
      send_displacements = SUM (Obs % Header % ObsPerBatchPerPE(1:batch - 1,mype))
      receive_displacements = SUM (Obs % Header % ObsPerBatchPerPE(1:batch - 1,:))
    ELSE
      send_displacements = 0
      receive_displacements = 0
    END IF
    DO i = 1,nproc - 1
      receive_displacements(i) = receive_displacements(i - 1) + Obs % Header % ObsPerBatchPerPE(batch,i - 1)
    END DO

    DO i = 1,SIZE (TotalReal,dim = 2)
      CALL ops_mpl_alltoallv (LocalReal(:,i),        &
                              send_counts,           &
                              send_displacements,    &
                              mpl_real,              &
                              TotalReal(:,i),        &
                              receive_counts,        &
                              receive_displacements, &
                              mpl_real,              &
                              mpl_comm_world,        &
                              ErrStat)
      IF (Errstat /= mpl_success) THEN
        WRITE (ErrMess, '(A,I0)') "Error in mpl_alltoallv, Errstat = ", Errstat
        CALL gen_fail (RoutineName, &
                       ErrMess)
      END IF
    END DO
  END DO
ELSE
  send_counts = Obs % Header % ObsPerPE(mype)
  receive_counts(:) = Obs % Header % ObsPerPE(:)
  send_displacements = 0
  receive_displacements = 0
  DO i = 1,nproc - 1
    receive_displacements(i) = receive_displacements(i - 1) + Obs % Header % ObsPerPE(i - 1)
  END DO

  DO i = 1,SIZE (TotalReal,dim = 2)
    CALL ops_mpl_alltoallv (LocalReal(:,i),        &
                            send_counts,           &
                            send_displacements,    &
                            mpl_real,              &
                            TotalReal(:,i),        &
                            receive_counts,        &
                            receive_displacements, &
                            mpl_real,              &
                            mpl_comm_world,        &
                            ErrStat)
    IF (Errstat /= mpl_success) THEN
      WRITE (ErrMess, '(A,I0)') "Error in mpl_alltoallv, Errstat = ", Errstat
      CALL gen_fail (RoutineName, &
                     ErrMess)
    END IF
  END DO
END IF

#else

TotalReal(1:NumObsTotal,:) = LocalReal(1:NumObsTotal,:)

#endif

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END  SUBROUTINE Ops_GatherReal2D
