!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Prints the contents of a CX file batch by batch.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_PrintCXBatches(Header,      &
                              batch_list,  &
                              column_list, &
                              out_unit)

USE GenMod_CLookAdd, ONLY: &
  LBNPT

USE GenMod_ModelIO, ONLY: &
  UM_Header_Type

USE GenMod_UMHeaderConstants, ONLY: &
  FH_LookupSize2

IMPLICIT NONE

! Subroutine arguments:
TYPE (UM_Header_Type), INTENT(IN) :: Header
INTEGER, INTENT(IN)               :: batch_list(:)
INTEGER, INTENT(IN)               :: column_list(:)
INTEGER, INTENT(IN)               :: out_unit

! Local declarations:
REAL, POINTER                     :: BatchOfObs(:,:)
INTEGER                           :: i
INTEGER                           :: j
INTEGER                           :: cx_batch

DO i = 1,SIZE (batch_list)
  cx_batch = batch_list(i)
  WRITE (out_unit, '(A,I0,A)') "Batch number ", cx_batch, ":"
  IF (cx_batch <= Header % Fixhd(FH_LookupSize2)) THEN
    CALL Ops_ReadCXBatch (Header,     &
                          cx_batch,   &
                          BatchOfObs)

    DO j = 1,SIZE (column_list)
      WRITE (out_unit, '(A,I0,A)') "Column number ", column_list(j), ":"
      IF (column_list(j) <= Header % Lookup(LBNPT,cx_batch)) THEN
        WRITE (out_unit, '(10F10.2)') BatchOfObs(:,column_list(j))
      ELSE
        WRITE (out_unit, '(A)') "doesn't exist"
      END IF
    END DO
    DEALLOCATE (BatchOfObs)
  ELSE
    WRITE (out_unit, '(A)') "doesn't exist"
  END IF

END DO

END SUBROUTINE Ops_PrintCXBatches
