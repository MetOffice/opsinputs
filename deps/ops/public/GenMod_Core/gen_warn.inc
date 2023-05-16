!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Report a warning.
!
! Implementation should follow the logic below:
!
!   Local = .true.
!         Print out on all PEs
!   Local = .false.
!         Print out on PE 0
!   Local = not present
!      Default to Local = .false.
!-------------------------------------------------------------------------------

SUBROUTINE gen_warn_array (NameOfRoutine, &
                           Message,       &
                           Local)

USE GenMod_Control, ONLY: &
  ProduceHTML,            &
  WarningToError

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT (IN)   :: NameOfRoutine ! calling procedure
CHARACTER(len=*), INTENT (IN)   :: Message(:)    ! message to report
LOGICAL, OPTIONAL, INTENT (IN)  :: Local         ! is message local to the PE?

! Local declarations:
CHARACTER(len=256), ALLOCATABLE :: mesg(:)  ! header + Message(:)
CHARACTER(len=32)               :: html_s   ! HTML start
CHARACTER(len=16)               :: label    ! fatal or warning
CHARACTER(len=8)                :: html_e   ! HTML end
CHARACTER(len=8)                :: colour   ! HTML colour

IF (WarningToError) THEN
  CALL gen_fail (NameOfRoutine, &
                 Message(:),    &
                 Local)
END IF

! Warning
WarningCount = WarningCount + 1
colour = ColourWarning
label = 'WARNING'

IF (ProduceHTML) THEN
  html_s = '<span style="color: ' // TRIM (colour) // '">'
  html_e = '</span>'
ELSE
  html_s = ''
  html_e = ''
END IF

! Set up the message to report
ALLOCATE (mesg (SIZE (Message) + 1))

mesg(2:) = Message(1:)

! Warning heading
mesg(1) = TRIM (html_s) // TRIM (label) // TRIM (html_e)

! Write modified message to both stdout and stderr
CALL gen_message (NameOfRoutine,   &
                  mesg,            &
                  Unit = ErrorOut, &
                  Local = Local)
CALL gen_message (NameOfRoutine,     &
                  mesg,              &
                  Unit = MessageOut, &
                  Local = Local)

END SUBROUTINE gen_warn_array

SUBROUTINE gen_warn_scalar (NameOfRoutine, &
                            Message,       &
                            Local)

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT (IN)  :: NameOfRoutine ! name of calling procedure
CHARACTER(len=*), INTENT (IN)  :: Message       ! message to output
LOGICAL, OPTIONAL, INTENT (IN) :: Local         ! message local to this PE?

CALL gen_warn_array (NameOfRoutine, &
                     (/Message/),   &
                     Local = Local)

END SUBROUTINE gen_warn_scalar
