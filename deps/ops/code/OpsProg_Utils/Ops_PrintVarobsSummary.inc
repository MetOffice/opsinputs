!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Prints the contents of a varob header.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_PrintVarobsSummary (varobs,     &
                                   batch_list, &
                                   ob_list,    &
                                   out_unit)

USE GenMod_CLookAdd, ONLY: &
  VarobsLookupNumObs

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI

USE GenMod_UMHeaderConstants, ONLY: &
  FH_LookupSize2,                   &
  IC_LenObCol

USE OpsMod_VarobsLib, ONLY: &
  varobs_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (varobs_type), INTENT(IN) :: varobs
INTEGER, INTENT(IN)            :: batch_list(:)
INTEGER, INTENT(IN)            :: ob_list(:)
INTEGER, INTENT(IN)            :: out_unit

! Local declarations:
INTEGER                        :: i
INTEGER                        :: j
INTEGER                        :: k
INTEGER                        :: obtype_list(100)
INTEGER                        :: obtype_count(100)
INTEGER                        :: obtype
INTEGER                        :: num_distinct_obtypes
INTEGER                        :: batch_num
INTEGER                        :: ob_num
INTEGER                        :: len_ob_col
REAL, ALLOCATABLE              :: BatchOfObs(:)

obtype_list = IMDI
obtype_count = IMDI

len_ob_col = varobs % IntC(IC_LenObCol)

DO i = 1, SIZE (batch_list)
  batch_num = batch_list(i)
  IF (batch_num <= varobs % Fixhd(FH_LookupSize2) - 2) THEN
    CALL varobs % read_batch (batch_num,  &
                              BatchOfObs)
    DO j = 1, SIZE (ob_list)
      ob_num = ob_list(j)
      IF (ob_num <= varobs % Lookup(VarobsLookupNumObs,batch_num)) THEN
        obtype = NINT (BatchOfObs((ob_list(j) - 1) * len_ob_col + 2))
        DO k = 1, SIZE (obtype_list)
          IF (obtype_list(k) == IMDI) THEN
            obtype_list(k) = obtype
            obtype_count(k) = 1
            EXIT
          ELSE IF (obtype_list(k) == obtype) THEN
            obtype_count(k) = obtype_count(k) + 1
            EXIT
          END IF
        END DO
      END IF
    END DO
    DEALLOCATE (BatchOfObs)
  END IF
END DO

num_distinct_obtypes = COUNT (obtype_list /= IMDI)
WRITE (out_unit, '(2A10)') "ObsType", "Count"
DO i = 1, num_distinct_obtypes
  WRITE (out_unit, '(2I10)') obtype_list(i), obtype_count(i)
END DO

END SUBROUTINE Ops_PrintVarobsSummary
