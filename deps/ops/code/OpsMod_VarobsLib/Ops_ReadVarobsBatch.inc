!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Reads in one batch from a varobs file starting at batch number field.  The
! data is returned in the out_buf array.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_ReadVarobsBatch (varobs,  &
                                field,   &
                                out_buf)

USE GenMod_CLookAdd, ONLY: &
  LBEGIN,                  &
  LBLREC

USE GenMod_Core, ONLY: &
  gen_fail,            &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  RMDI

USE OpsMod_Portio_Wrappers, ONLY: &
  Ops_Buffin_Wrapper,             &
  Ops_Setpos_Wrapper

IMPLICIT NONE

! Subroutine arguments:
CLASS (varobs_type), INTENT(IN) :: varobs           ! UM header from file
INTEGER, INTENT(IN)             :: field            ! first field to read in
REAL, ALLOCATABLE, INTENT(OUT)  :: out_buf(:)       ! array to return the data in

! Local declarations:
CHARACTER(len=500)              :: CMessage(2)        ! Error message
INTEGER                         :: field_start        ! location of field on disk
INTEGER                         :: data_read_size     ! number of words to read from disk
CHARACTER(len=*), PARAMETER     :: RoutineName = "Ops_ReadVarobsBatch"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! Set up the location of the field on disk

field_start = varobs % lookup(LBEGIN,field) ! position of field in file
IF (field_start <= 0) THEN
  WRITE (CMessage(1), '(A,I0)') 'Start address =', field_start
  CMessage(2) = 'Start address of field not given'
  CALL gen_fail (RoutineName,   &
                 CMessage(1:2))
END IF

data_read_size = varobs % lookup(LBLREC,field)

IF (data_read_size < 0) THEN
  WRITE (CMessage(1), '(A,I0)') 'Number of words to read =', data_read_size
  CMessage(2) = 'Number of words to read not given'
  CALL gen_fail (RoutineName,   &
                 CMessage(1:2))
END IF

ALLOCATE (out_buf(data_read_size))
out_buf = RMDI

! Move file pointer to the start of the field
CALL Ops_Setpos_Wrapper (varobs % unit_num, &
                         field_start)

! ops_buffin the data

CALL Ops_Buffin_Wrapper (varobs % unit_num, &
                         out_buf,           &
                         data_read_size)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_ReadVarobsBatch
