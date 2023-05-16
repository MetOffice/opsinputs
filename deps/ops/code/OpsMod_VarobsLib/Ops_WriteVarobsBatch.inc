!-----------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-----------------------------------------------------------------------
! Writes out one batch to a varobs starting at batch field.
!-----------------------------------------------------------------------

SUBROUTINE Ops_WriteVarobsBatch (varobs, &
                                 field,  &
                                 D1)

USE GenMod_CLookAdd, ONLY: &
  LBEGIN,                  &
  LBLREC,                  &
  LBNREC

USE GenMod_Control, ONLY: &
  UM_SECTOR_SIZE

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
  RMDI

USE OpsMod_Portio_Wrappers, ONLY: &
  Ops_Buffout_Wrapper,            &
  Ops_Setpos_Wrapper

IMPLICIT NONE

! Subroutine arguments:
CLASS (varobs_type), INTENT(IN) :: varobs
INTEGER, INTENT(IN)             :: field
REAL, INTENT(IN)                :: D1(:)

! Local declarations:
INTEGER                         :: i
INTEGER                         :: WordAddress    ! Address from which to begin I/O.
INTEGER                         :: um_sector_ipts ! No. of words to write, rounded up.
REAL                            :: Buf(((SIZE (D1) + um_sector_size - 1) / um_sector_size) * um_sector_size)
CHARACTER(len=*), PARAMETER     :: routine_name = "Ops_WriteVarobsBatch"

!----------------------------------------------------------------------
! [1]: Initialize.
!----------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (routine_name)

!----------------------------------------------------------------------
! [2]: Write fields.
!----------------------------------------------------------------------

WordAddress = varobs % Lookup(LBEGIN,field)

! Use the stored rounded-up value:
um_sector_ipts = varobs % Lookup(LBNREC,field)

! Position file pointer:
CALL ops_setpos_wrapper (varobs % unit_num, &
                         WordAddress)

!----------------------------------------------------------------------
! [2.1]: Non-MPP write.
!----------------------------------------------------------------------

Buf = RMDI
DO i = 1, varobs % Lookup(LBLREC,field)
  Buf(i) = D1(i)
END DO

CALL Ops_Buffout_Wrapper (varobs % unit_num, &
                          Buf(:),            &
                          um_sector_ipts)

IF (UseTrace) CALL gen_trace_exit (routine_name)

END SUBROUTINE Ops_WriteVarobsBatch
