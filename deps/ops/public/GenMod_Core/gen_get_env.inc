!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Reads an environment variable and returns it.
!-------------------------------------------------------------------------------

SUBROUTINE gen_get_env (ac_name,      &
                        i_len_result, &
                        ac_result,    &
                        i_status)

#if FORTRAN_VERSION < 2003
USE OpsMod_UMInterface, ONLY: &
  ops_fort_get_env
#endif

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN)   :: ac_name      ! variable name
INTEGER, INTENT(OUT)           :: i_len_result ! content length
CHARACTER(len=*), INTENT(OUT)  :: ac_result    ! variable content
INTEGER, INTENT(OUT), OPTIONAL :: i_status     ! return code

! Local declarations:
INTEGER                        :: i_status_opt ! i_status option

#if FORTRAN_VERSION >= 2003

CALL GET_ENVIRONMENT_VARIABLE (ac_name,      &
                               ac_result,    &
                               i_len_result, &
                               i_status_opt)
IF (i_status_opt /= 0) THEN
  i_len_result = -1
END IF

#else

! Local declarations:
INTEGER, PARAMETER             :: len_max = 16000 ! maximum length in contents (guess)
CHARACTER(len=len_max)         :: contents

contents = ''
CALL ops_fort_get_env (TRIM (ac_name),     &
                       LEN_TRIM (ac_name), &
                       contents,           &
                       len_max,            &
                       i_status_opt)
i_len_result = LEN_TRIM (contents)
ac_result = ''
IF (i_status_opt == 0) THEN
  ac_result = contents
  IF (i_len_result > LEN (ac_result)) THEN
    i_status_opt = -1
    i_len_result = LEN (ac_result)
  END IF
ELSE
  i_status_opt = 1
  i_len_result = -1
END IF

#endif

IF (PRESENT (i_status)) i_status = i_status_opt

END SUBROUTINE gen_get_env
