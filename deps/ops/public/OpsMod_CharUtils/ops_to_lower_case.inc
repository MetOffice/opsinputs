!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Returns the input string but with all uppercase letters converted to
! lowercase.
!-------------------------------------------------------------------------------

FUNCTION ops_to_lower_case (in_string) RESULT (out_string)

IMPLICIT NONE

! Function arguments:
CHARACTER(len=*), INTENT(IN)   :: in_string

! Function result:
CHARACTER(len=LEN (in_string)) :: out_string

! Local declarations:
INTEGER                        :: i

out_string = in_string

DO i = 1, LEN_TRIM (in_string)
  IF (IACHAR (in_string(i:i)) >= IACHAR ("A") .AND. &
      IACHAR (in_string(i:i)) <= IACHAR ("Z")) THEN
    out_string(i:i) = ACHAR (32 + IACHAR (in_string(i:i)))
  ELSE IF (in_string(i:i) == "-") THEN
    out_string(i:i) = "_"
  ELSE
    out_string(i:i) = in_string(i:i)
  END IF
END DO

END FUNCTION ops_to_lower_case
