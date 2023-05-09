!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Output the header part of a message for a procedure.
!-------------------------------------------------------------------------------

SUBROUTINE Gen_WriteMessageHeader (unit_out, &
                                   is_local, &
                                   link)

USE GenMod_Control, ONLY: &
  DocumentationURL,       &
  mype

IMPLICIT NONE

! Subroutine arguments:
INTEGER, INTENT(IN)           :: unit_out
LOGICAL, INTENT(IN)           :: is_local
CHARACTER (len=*), INTENT(IN) :: link

! Local declarations:
INTEGER                       :: i

!$OMP CRITICAL
IF (BackTrace) THEN
  IF (is_local) THEN
    DO i = 1, TraceStackPointer
      WRITE (unit_out, '(A,I0,A)') '[', mype, ': ' // REPEAT ("-", i) // &
               '<a href=" ' // TRIM (DocumentationURL) // '/' // TRIM (TraceStack(i)) // &
               '.html">' // TRIM (TraceStack(i)) // '</a>'// ']'
    END DO
  ELSE
    DO i = 1, TraceStackPointer
      WRITE (unit_out, '(A)') REPEAT ("-", i) // '<a href="' // TRIM (DocumentationURL) // '/' // TRIM (TraceStack(i)) // &
                              '.html">' // TRIM (TraceStack(i)) // '</a>'
    END DO
  END IF
ELSE
  IF (is_local) THEN
    WRITE (unit_out, '(A,I0,A)') '[', mype, ': ' // TRIM (link) // ']'
  ELSE
    WRITE (unit_out, '(A)') TRIM (link)
  END IF
END IF
!$OMP END CRITICAL

END SUBROUTINE Gen_WriteMessageHeader
