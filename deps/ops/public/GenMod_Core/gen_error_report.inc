!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Report a warning or a fatal error. Terminate the execution in the event of
! a fatal error.
!
! For implementation see gen_fail, gen_warn and gen_message.
!-------------------------------------------------------------------------------

SUBROUTINE gen_error_report (NameOfRoutine, &
                             Message,       &
                             ErrorStatus,   &
                             Local)

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT (IN)  :: NameOfRoutine ! calling procedure
CHARACTER(len=*), INTENT (IN)  :: Message(:)    ! message to report
INTEGER, INTENT (IN)           :: ErrorStatus   ! error code
LOGICAL, OPTIONAL, INTENT (IN) :: Local         ! is message local to the PE?

SELECT CASE (ErrorStatus)
  CASE (StatusOK)
    CALL gen_message (NameOfRoutine,     &
                      Message,           &
                      Unit = MessageOut, &
                      Local = Local)
  CASE (StatusWarning)
    CALL gen_warn (NameOfRoutine, &
                   Message,       &
                   Local = Local)
  CASE DEFAULT
    CALL gen_fail (NameOfRoutine, &
                   Message,       &
                   Local = Local)
END SELECT

END SUBROUTINE gen_error_report
