!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Output a message for a procedure.
!-------------------------------------------------------------------------------

SUBROUTINE gen_message_array (NameOfRoutine, &
                              Message,       &
                              Unit,          &
                              Local)

USE GenMod_Control, ONLY: &
  DocumentationURL,       &
  mype,                   &
  nproc,                  &
  ProduceHTML,            &
  LenDocumentationURL

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT (IN)  :: NameOfRoutine ! name of calling procedure
CHARACTER(len=*), INTENT (IN)  :: Message(:)    ! message to output
INTEGER, OPTIONAL, INTENT (IN) :: Unit          ! unit number
LOGICAL, OPTIONAL, INTENT (IN) :: Local         ! message local to this PE?

! Local declarations:
INTEGER, PARAMETER             :: len_html = 21
INTEGER, PARAMETER             :: len_link = LenDocumentationURL + len_html + 31 * 2
CHARACTER(len=len_link)        :: link     ! name/link to calling procedure
CHARACTER(len=8)               :: tmp_str  ! temporary string
INTEGER                        :: len_max  ! maximum length of output
INTEGER                        :: unit_out ! output unit number
LOGICAL                        :: is_local ! message local to this PE?

! Output to input unit if specified, otherwise output to standard output
! ------------------------------------------------------------------------------

IF (PRESENT (Unit)) THEN
  unit_out = Unit
ELSE
  unit_out = MessageOut
END IF

! Do nothing if output unit is invalid or there are no messages to print
IF (unit_out < 0 .OR. SIZE (Message) == 0) RETURN

! Is output message local to the current PE
IF (PRESENT (Local)) THEN
  is_local = nproc > 1 .AND. Local
ELSE
  is_local = .FALSE.
END IF

! Determine the maximum length each line can hold
! ------------------------------------------------------------------------------
SELECT CASE (unit_out)
  CASE (ErrorOut)
    ! If output unit is the standard error
    IF (LenErrorOut == 0) THEN
      len_max = HUGE (len_max) ! maximum integer
    ELSE
      len_max = LenErrorOut
    END IF

  CASE (MessageOut)
    ! If output unit is the standard output
    IF (LenMessageOut == 0) THEN
      len_max = HUGE (len_max) ! maximum integer
    ELSE
      len_max = LenMessageOut
    END IF

  CASE DEFAULT
    ! All other output
    len_max = HUGE (len_max) ! maximum integer
END SELECT

IF (is_local) THEN
  ! Maximum length is decreased by (4 + length of PE number)
  ! as message will be written in the form: "[<PE>: <message>]"
  WRITE (tmp_str, '(I0)') mype
  len_max = len_max - 4 - LEN_TRIM (tmp_str)
END IF

! Set up link/name to calling procedure
! ------------------------------------------------------------------------------
IF (ProduceHTML) THEN
  link = '<a href="' // TRIM (DocumentationURL) // '/' // TRIM (NameOfRoutine) // &
         '.html">' // TRIM (NameOfRoutine) // '</a>'

  IF (LEN_TRIM (link) > len_max) link = NameOfRoutine

ELSE
  link = NameOfRoutine
END IF

IF (is_local .OR. mype == 0) THEN
  CALL Gen_WriteMessageHeader (unit_out, &
                               is_local, &
                               link)
  CALL Gen_WriteMessageBody (Message,  &
                             unit_out, &
                             len_max,  &
                             is_local)
END IF

END SUBROUTINE gen_message_array

SUBROUTINE gen_message_scalar (NameOfRoutine, &
                               Message,       &
                               Unit,          &
                               Local)

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT (IN)  :: NameOfRoutine ! name of calling procedure
CHARACTER(len=*), INTENT (IN)  :: Message       ! message to output
INTEGER, OPTIONAL, INTENT (IN) :: Unit          ! unit number
LOGICAL, OPTIONAL, INTENT (IN) :: Local         ! message local to this PE?

CALL gen_message_array (NameOfRoutine, &
                        (/Message/),   &
                        Unit = Unit,   &
                        Local = Local)

END SUBROUTINE gen_message_scalar
