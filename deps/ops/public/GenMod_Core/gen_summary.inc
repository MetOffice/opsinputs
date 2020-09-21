!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Print number of warnings encountered by PE 0.
! Fail if this is greater than some tolerance.
!-------------------------------------------------------------------------------

RECURSIVE SUBROUTINE gen_summary

USE GenMod_Control, ONLY: &
    mype,                 &
    ProduceHTML,          &
    WarningsMax

USE GenMod_MiscUMScienceConstants, ONLY: &
    IMDI

IMPLICIT NONE

! Local declarations:
CHARACTER(len=*), PARAMETER :: html_s = '<span style="color: ' // ColourWarning // '">'
CHARACTER(len=*), PARAMETER :: html_e = '</span>'
CHARACTER(len=*), PARAMETER :: RoutineName = "gen_summary"
CHARACTER(len=32)           :: mesg_s
CHARACTER(len=24)           :: mesg_e
CHARACTER(len=80)           :: Message(2)

!-------------------------------------------------------------------------------
! Set up output message, plural and singular

IF (WarningCount > 1) THEN
  mesg_s = 'Error Summary: There have been '
  mesg_e = ' WARNING messages.'
ELSE
  mesg_s = 'Error Summary: There has been '
  mesg_e = ' WARNING message.'
END IF

WRITE (Message(1), '(A,1X,I0,A)') TRIM (mesg_s), WarningCount, TRIM (mesg_e)

IF (WarningsMax /= IMDI .AND. WarningCount > WarningsMax) THEN
  WRITE (Message(2), '(A,I0)') "WarningsMax is: ", WarningsMax

  CALL gen_fail (RoutineName,            &
                 Message,                &
                 ErrorSummary = .FALSE.)
END IF

! Output only on PE zero and there have been warnings
IF (mype == 0 .AND. WarningCount > 0) THEN
  ! Print HTML or plain text summary of warnings
  IF (ProduceHTML) THEN
    WRITE (ErrorOut, '(A)') html_s // TRIM (Message(1)) // html_e

  ELSE
    WRITE (ErrorOut, '(A)') TRIM (Message(1))
  END IF
END IF

END SUBROUTINE gen_summary
