!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Prints the contents of an obstore batch field by field.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_PrintBatchFieldByField (Header,     &
                                       batch_list, &
                                       ob_list,    &
                                       out_unit)

USE GenMod_CLookAdd, ONLY: &
  LBNPT

USE GenMod_MiscUMScienceConstants, ONLY: &
  RMDI

USE GenMod_ModelIO, ONLY: &
  UM_Header_Type

USE GenMod_UMHeaderConstants, ONLY: &
  FH_LevDepCSize1,                  &
  IC_NumVarBatches

USE OpsMod_Constants, ONLY: &
  CMDI

USE OpsMod_Extract, ONLY: &
  CharStrLen

USE OpsMod_Index, ONLY: &
  ElementDescrip,       &
  IndexCharData

USE OpsMod_UtilsProgs, ONLY: &
  ModelObType

IMPLICIT NONE

! Subroutine arguments:
TYPE (UM_Header_Type), INTENT(IN)      :: Header
INTEGER, INTENT(IN)                    :: batch_list(:)
INTEGER, INTENT(IN)                    :: ob_list(:)
INTEGER, INTENT(IN)                    :: out_unit

! Local declarations:
REAL, POINTER                          :: BatchOfObs(:,:)
INTEGER                                :: i
INTEGER                                :: j
INTEGER                                :: k
INTEGER                                :: l
INTEGER                                :: char_start
INTEGER                                :: char_len
CHARACTER(len=CharStrLen), ALLOCATABLE :: CharString(:)
INTEGER                                :: ob_batch
INTEGER                                :: num_indices
INTEGER                                :: creq_index
CHARACTER(len=32)                      :: element_string

num_indices = Header % Fixhd(FH_LevDepCSize1)

DO i = 1,SIZE (batch_list)
  ob_batch = batch_list(i)
  IF (ob_batch > Header % IntC(IC_NumVarBatches)) CYCLE
  CALL Ops_ReadObstoreBatch (Header,     &
                             ob_batch,   &
                             BatchOfObs)

  WRITE (out_unit, '(A,I0,A,I0,A)') "For Batch ", ob_batch, " ModelObType = ", Header % Lookup(ModelObType,ob_batch), ":"
  DO j = (ob_batch - 1) * num_indices + 1, ob_batch * num_indices
    creq_index = j - ((j - 1) /  num_indices) * num_indices
    IF (Header % ColDepC(j) > 0) THEN
      IF (creq_index <= SIZE (ElementDescrip, DIM = 1)) THEN
        element_string = ElementDescrip(creq_index,1)
      ELSE
        element_string = CMDI
      END IF
      IF (creq_index /= IndexCharData) THEN
        IF (element_string == "CALL_SIGN" .OR. &
            element_string == "BECN_RPRT_NAME" .OR. &
            element_string == "COLTN_CNTR" .OR. &
            element_string == "RGSRN_NMBR" .OR. &
            element_string == "SITE_NAME") THEN
          WRITE (out_unit,'(3A)') "For Element ", TRIM (element_string), &
                                  " values = "
          char_start = IBITS (NINT (BatchOfObs(NINT (Header % ColDepC(j)),1)),0,16)
          char_len = IBITS (NINT (BatchOfObs(NINT (Header % ColDepC(j)),1)),16,16)
          WRITE (out_unit,'(A,I0)') "Char starts: ", char_start
          WRITE (out_unit,'(A,I0)') "Char lengths: ", char_len

          ALLOCATE (CharString(Header % Lookup(LBNPT, ob_batch)))
          CharString = ""
          DO k = 1,SIZE (ob_list)
            IF (ob_list(k) < SIZE (BatchOfObs,DIM = 2)) THEN
              DO l = 1,char_len
                IF (BatchOfObs(l + NINT (Header % ColDepC((ob_batch - 1) * num_indices + IndexCharData)) + &
                    char_start - 2,ob_list(k)) == RMDI) THEN
                  EXIT
                ELSE
                  CharString(ob_list(k))(l:l) = CHAR (NINT (BatchOfObs(l + NINT (Header % ColDepC((ob_batch - 1) * &
                    num_indices + IndexCharData)) + char_start - 2,ob_list(k))))
                END IF
              END DO
              WRITE (out_unit, '(A)') CharString(ob_list(k))(1:char_len)
            END IF
          END DO
          DEALLOCATE (CharString)
        ELSE
          WRITE (out_unit, '(3A)') "For Element ", TRIM (element_string), &
                                   " values = "
          DO k = 1,SIZE (ob_list)
            IF (ob_list(k) < SIZE (BatchOfObs, DIM = 2)) THEN
              WRITE (out_unit, '(F20.5)') BatchOfObs(NINT (Header % ColDepC(j)),ob_list(k))
            END IF
          END DO
        END IF
      END IF
    END IF
  END DO
  DEALLOCATE (BatchOfObs)
END DO

END SUBROUTINE Ops_PrintBatchFieldByField
