!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Utility to replace segments of a text string.  Replace occurrences of a given
! Tag string with another, Replacement string.
!-------------------------------------------------------------------------------

FUNCTION gen_replace_text (LenResult,   &
                           Template,    &
                           Tag,         &
                           Replacement) RESULT (Result)

IMPLICIT NONE

! Function arguments:
INTEGER, INTENT(IN)          :: LenResult   ! Max length of result string
CHARACTER(len=*), INTENT(IN) :: Template    ! String in which tags are located
CHARACTER(len=*), INTENT(IN) :: Tag         ! Tag string to be replaced
CHARACTER(len=*), INTENT(IN) :: Replacement ! Text to replace the tags

! Function result:
CHARACTER(len=LenResult)     :: Result      ! Output string with tags replaced

! Local declarations:
CHARACTER(len=*), PARAMETER  :: RoutineName = "gen_replace_text"
INTEGER                      :: iLoc
INTEGER                      :: i1
INTEGER                      :: ilast               ! pointers to chars in the Template
INTEGER                      :: j1
INTEGER                      :: j1n                 ! pointers to chars in the Result
CHARACTER(len=80)            :: Message(2)          ! Error message to be output

ilast = LEN_TRIM (Template) ! pointer to last char of Template to be searched
i1 = 1                      ! pointer to  1st char of Template to be searched
j1 = 1                      ! pointer to next free char of Result

DO
  iLoc = INDEX (Template(i1:ilast), Tag)

  IF (iLoc == 0) THEN            ! Tag not present in remaining bit of Template
    j1n = j1 + ilast - i1 + 1
    Result(j1:) = Template(i1:ilast) ! copy, and fill rest with spaces
    EXIT
  END IF

  j1n = j1 + iLoc - 1 + LEN (Replacement)
  IF (j1n - 1 > LenResult) EXIT   ! Too long.  Jump out then fail.

  Result(j1:j1n - 1) = Template(i1:i1 + iLoc - 2) // Replacement
  i1 = i1 + iLoc - 1 + LEN (Tag)
  j1 = j1n

END DO

IF (j1n - 1 > LenResult) THEN
  Message(1) = 'too long "' // Tag // '"' // Replacement // '" in'
  Message(2) = Template
  CALL gen_fail (RoutineName,  &
                 Message(1:2))
END IF

END FUNCTION gen_replace_text
