!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Prints the contents of an obstore batch ob by ob.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_PrintBatchObByOb (Header,     &
                                 batch_list, &
                                 ob_list,    &
                                 out_unit)

USE GenMod_CLookAdd, ONLY: &
  MOT

USE GenMod_ModelIO, ONLY: &
  UM_Header_Type

USE GenMod_UMHeaderConstants, ONLY: &
  FH_LevDepCSize1,                  &
  IC_NumVarBatches

USE OpsMod_Constants, ONLY: &
  CMDI

USE OpsMod_Index, ONLY: &
  ElementDescrip,       &
  IndexCharData

IMPLICIT NONE

! Subroutine arguments:
TYPE (UM_Header_Type), INTENT(IN) :: Header
INTEGER, INTENT(IN)               :: batch_list(:)
INTEGER, INTENT(IN)               :: ob_list(:)
INTEGER, INTENT(IN)               :: out_unit

! Local declarations:
REAL, POINTER                     :: BatchOfObs(:,:)
INTEGER                           :: i
INTEGER                           :: j
INTEGER                           :: k
INTEGER                           :: l
INTEGER                           :: m
INTEGER                           :: char_start
INTEGER                           :: char_len
INTEGER                           :: ob_batch
CHARACTER(len=200)                :: element_name
CHARACTER(len=25)                 :: char_data
INTEGER                           :: num_indices
INTEGER                           :: creq_index
CHARACTER(len=32)                 :: element_string

num_indices = Header % Fixhd(FH_LevDepCSize1)

DO i = 1, SIZE (batch_list)
  ob_batch = batch_list(i)
  IF (ob_batch > Header % IntC(IC_NumVarBatches)) CYCLE
  CALL Ops_ReadObstoreBatch (Header,     &
                             ob_batch,   &
                             BatchOfObs)
  DO j = 1, SIZE (ob_list)
    IF (ob_list(j) <= SIZE (BatchOfObs, DIM = 2)) THEN
      WRITE (out_unit, '(3(A,I0))') "Batch Number: ", ob_batch, " Observation Type: ", &
                                    Header % Lookup(MOT,ob_batch), " Ob number = ", ob_list(j)
      DO k = (ob_batch - 1) * num_indices + 1, ob_batch * num_indices
        creq_index = k - ((k - 1) /  num_indices) * num_indices
        IF (Header % ColDepC(k) > 0) THEN
          IF (creq_index <= SIZE (ElementDescrip, DIM = 1)) THEN
            element_string = ElementDescrip(creq_index,1)
          ELSE
            element_string = CMDI
          END IF
          IF (creq_index /= IndexCharData) THEN
            DO l = 1, NINT (Header % LevDepC(k))
              IF (NINT (Header % LevDepC(k)) > 1) THEN
                WRITE (element_name, '(A,I0,A)') TRIM (element_string) // ' (', l, ')'
              ELSE
                element_name = element_string
              END IF
              IF (element_name == "CALL_SIGN" .OR. &
                  element_name == "BECN_RPRT_NAME" .OR. &
                  element_name == "COLTN_CNTR" .OR. &
                  element_name == "RGSRN_NMBR" .OR. &
                  element_name == "SITE_NAME") THEN
                element_name = TRIM (element_name) // " (char)"
                char_data = ""
                char_start = IBITS (NINT (BatchOfObs(NINT (Header % ColDepC(k)) + ((l - 1) * NINT (Header % RowDepC(k))), &
                                   ob_list(j))),0,16)
                char_len = IBITS (NINT (BatchOfObs(NINT (Header % ColDepC(k)) + ((l - 1) * NINT (Header % RowDepC(k))), &
                                   ob_list(j))),16,16)
                DO m = 1, char_len
                  char_data(25 - char_len + m:25 - char_len + m) = &
                    CHAR (NINT (BatchOfObs(NINT (Header % ColDepC((ob_batch - 1) * num_indices + IndexCharData)) + &
                    ((char_start + m - 2)),ob_list(j))))
                END DO
                WRITE (out_unit, '(A40,A,A25)') element_name, " = ",char_data
              ELSE
                WRITE (out_unit, '(A40,A,F25.2)') element_name, " = ", &
                       BatchOfObs(NINT (Header % ColDepC(k)) + ((l - 1) * NINT (Header % RowDepC(k))),ob_list(j))
              END IF
            END DO
          END IF
        END IF
      END DO
      WRITE (out_unit, '(A)')
    END IF
  END DO
  DEALLOCATE (BatchOfObs)
END DO

END SUBROUTINE Ops_PrintBatchObByOb
