!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Compares String against a Template, which consists of a list of comma
! separated strings, which may include wildcards. '*' may match any or no
! characters, and '?' must match a single numeric digit.
!
! Loops through comma separated template strings. For each sub-template, the
! characters in String are looped through, while proceeding also through the
! template. If a mismatch is found against the template, then the template
! position is moved back to after the previous '*'.  At the end of the String,
! if the end of the template has been reached, the string matches.
!
!   Inputs:
!     String: a character string to be matched against template
!     Template: a template list containing wildcards
!
!   Outputs:
!     Function result: a logical indicating whether the string matches.
!-------------------------------------------------------------------------------

ELEMENTAL FUNCTION Ops_LEQWildCard (String, &
                                    Template) RESULT (Equal)

IMPLICIT NONE

! Function arguments:
CHARACTER(len=*), INTENT(IN) :: String
CHARACTER(len=*), INTENT(IN) :: Template

! Function result:
LOGICAL                      :: Equal

! Local declarations:
INTEGER                      :: Tpos
INTEGER                      :: SPos
INTEGER                      :: JumpBack
INTEGER                      :: LenTemplate
INTEGER                      :: Start
INTEGER                      :: Finish
INTEGER                      :: LenString
CHARACTER(len=1)             :: SCh
CHARACTER(len=1)             :: TCh

Start = 1

LenTemplate = LEN_TRIM (Template)
LenString = LEN_TRIM (String)

Equal = .FALSE.

DO Finish = 1, LenTemplate

  IF (Finish /= LenTemplate) THEN
    IF (Template(Finish + 1:Finish + 1) /= ',') CYCLE
  END IF

  TPos = Start
  JumpBack = 0

  DO SPos = 1, LenString + 1

    ! Find the next character in the template.
    DO TPos = TPos, Finish + 1
      Tch = ACHAR (0)
      IF (TPos <= Finish) TCh = Template(TPos:TPos)
      IF (Tch /= '*') EXIT
      JumpBack = TPos + 1
    END DO

    Sch = ACHAR (0)
    IF (SPos <= LenString) SCh = String(SPos:SPos)

    IF (TCh == SCh .OR. (Sch >= '0' .AND. Sch <= '9' .AND. Tch == '?')) THEN
      TPos = TPos + 1
    ELSE
      TPos = JumpBack
      IF (TPos == 0) EXIT
    END IF

  END DO

  Equal = Equal .OR. TPos == Finish + 2
  IF (Equal) EXIT

  Start = Finish + 2

END DO

END FUNCTION Ops_LEQWildCard
