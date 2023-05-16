!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Represents CHARACTER data as REAL data using the f90 TRANSFER function.
!
! Inputs : CHARACTER string
!
! Outputs: REAL array containing character string
!
! Method : The observations in the buffer are looped over and the character data
!          in each represented as reals by the Fortran90 TRANSFER function. Null
!          characters, i.e. missing data, in the characters strings are
!          converted to spaces otherwise the TRNASFERed data would be corrupt.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_StringToReal (CharData, &
                             RealData)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(len=*), INTENT(IN) :: CharData(:)
REAL, INTENT(OUT)            :: RealData(:,:)

! Local declarations:
CHARACTER(len=*), PARAMETER  :: RoutineName = "Ops_StringToReal"
INTEGER                      :: ObsLoop       ! Loop counter over observations
INTEGER                      :: CharLoop      ! Loop counter over characters
INTEGER                      :: NumObs
INTEGER                      :: NumChars

!-------------------------------------------------------------------------------
! 0. Tracing and initialisations
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-------------------------------------------------------------------------------
! 1. Convert REALs
!-------------------------------------------------------------------------------

NumChars = LEN (CharData)
NumObs = SIZE (RealData, DIM = 1)

DO ObsLoop = 1, NumObs

  DO CharLoop = 1, NumChars
    RealData(ObsLoop,CharLoop) = REAL (ICHAR (CharData(ObsLoop)(CharLoop:CharLoop)))
  END DO

END DO

!-------------------------------------------------------------------------------
! 2. Tracing and end of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit(RoutineName)

END SUBROUTINE Ops_StringToReal
