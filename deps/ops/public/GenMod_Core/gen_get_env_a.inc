!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Reads an environment variable, splits it into a list of strings and returns
! that list. If "separator" is omitted, split on whitespace.
!-------------------------------------------------------------------------------

SUBROUTINE gen_get_env_a (EnvName,       &
                          NoWords,       &
                          words,         &
                          separator_set)

IMPLICIT NONE

! Subroutine arguments
CHARACTER(len=*), INTENT(IN)           :: EnvName       ! variable name
INTEGER, INTENT(OUT)                   :: NoWords       ! # of words found
CHARACTER(len=*), INTENT(OUT)          :: words(:)      ! variable contents
CHARACTER(len=*), INTENT(IN), OPTIONAL :: separator_set ! separator set

! Local declarations:
INTEGER, PARAMETER                     :: len_max = 16000 ! maximum length in ac_result
CHARACTER(len=len_max)                 :: ac_result       ! as-read variable contents
CHARACTER(len=len_max), POINTER        :: p_words(:)      ! variable contents, temporary
INTEGER                                :: i
INTEGER                                :: i_len_result
INTEGER                                :: i_status

#if FORTRAN_VERSION >= 2003

CALL GET_ENVIRONMENT_VARIABLE (EnvName,      &
                               ac_result,    &
                               i_len_result, &
                               i_status)
#else

CALL gen_get_env (EnvName,      &
                  i_len_result, &
                  ac_result,    &
                  i_status)

#endif

IF (i_status /= 0) THEN
  NoWords = StatusWarning
  RETURN
END IF

CALL Gen_StringSplit (ac_result,     &
                      separator_set, &
                      p_words)
IF ((.NOT. ASSOCIATED (p_words)) .OR. SIZE (p_words) > SIZE (Words)) THEN
  NoWords = StatusWarning
  RETURN
END IF

NoWords = SIZE (p_words)
DO i = 1, NoWords
  words(i) = p_words(i)
END DO
DO i = NoWords + 1, SIZE (Words)
  words(i) = ''
END DO
DEALLOCATE (p_words)

END SUBROUTINE gen_get_env_a
