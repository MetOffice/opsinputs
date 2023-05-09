!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Returns a set of observation co-ordinates to work with, i.e. rotates the input
! co-ordinates if working with a rotated grid. If using a regular lat long grid,
! the output is the same as the input.
!
! Inputs : Regular latitudes and longitudes. UM header.
!
! Outputs: None
!
! Note   : RETURNS ALL LONGITUDES BETWEEN +/- 180 DEGREES
!-------------------------------------------------------------------------------

SUBROUTINE Ops_RotateCoordinates (InputLat,   & ! Input latitudes
                                  InputLon,   & ! Input longitudes
                                  PoleLat,    & ! Pole Latitude
                                  PoleLon,    & ! Pole Longitue
                                  WorkingLat, & ! Output latitudes
                                  WorkingLon)   ! Output longitudes

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE GenMod_Utilities, ONLY: &
    Gen_LatLon_to_Eq

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(IN)            :: InputLat(:)
REAL, INTENT(IN)            :: InputLon(:)
REAL, INTENT(INOUT)         :: PoleLat
REAL, INTENT(INOUT)         :: PoleLon
REAL, INTENT(INOUT)         :: WorkingLat(:)
REAL, INTENT(INOUT)         :: WorkingLon(:)

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_RotateCoordinates"
INTEGER                     :: Npoints
INTEGER                     :: NpointsValid
REAL, ALLOCATABLE           :: ValidLats(:)
REAL, ALLOCATABLE           :: ValidLons(:)
REAL, ALLOCATABLE           :: ValidWorkingLat(:)
REAL, ALLOCATABLE           :: ValidWorkingLon(:)
INTEGER                     :: LatPointer
INTEGER                     :: i

!-------------------------------------------------------------------------------
! 0. Tracing and initialisation
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!-----------------------------------------------------------------------------
! 1 Rotate co-ordinates
!-----------------------------------------------------------------------------

 Npoints = SIZE (InputLat)

IF (ANY (InputLat == RMDI) .OR. ANY (InputLon == RMDI)) THEN

  NPointsValid = COUNT (InputLat /= RMDI .AND. InputLon /= RMDI)

  ALLOCATE (ValidLats(NPointsValid))
  ALLOCATE (ValidLons(NPointsValid))
  ALLOCATE (ValidWorkingLat(NPointsValid))
  ALLOCATE (ValidWorkingLon(NPointsValid))

  LatPointer = 1
  DO i = 1, NPoints
    IF (InputLat(i) /= RMDI .AND. InputLon(i) /= RMDI) THEN
      ValidLats(LatPointer) = InputLat(i)
      ValidLons(LatPointer) = InputLon(i)
      LatPointer = LatPointer + 1
    END IF
  END DO

  CALL Gen_LatLon_to_Eq (ValidLats,       &
                         ValidLons,       &
                         ValidWorkingLat, &
                         ValidWorkingLon, &
                         PoleLat,         &
                         PoleLon)

  LatPointer = 1
  DO i = 1, NPoints
    IF (InputLat(i) /= RMDI .AND. InputLon(i) /= RMDI) THEN
      WorkingLat(i) = ValidWorkingLat(LatPointer)
      WorkingLon(i) = ValidWorkingLon(LatPointer)
      LatPointer = LatPointer + 1
    ELSE
      WorkingLat(i) = RMDI
      WorkingLon(i) = RMDI
    END IF
  END DO

  DEALLOCATE (ValidWorkingLon)
  DEALLOCATE (ValidWorkingLat)
  DEALLOCATE (ValidLons)
  DEALLOCATE (ValidLats)

ELSE

  CALL Gen_LatLon_to_Eq (InputLat,   &
                         InputLon,   &
                         WorkingLat, &
                         WorkingLon, &
                         PoleLat,    &
                         PoleLon)
END IF

!-------------------------------------------------------------------------------
! 2. Ensure longitudes are between +/- 180 degrees
!-------------------------------------------------------------------------------

WHERE (WorkingLon >= 180.0) WorkingLon(:) = WorkingLon(:) - 360.0

!-------------------------------------------------------------------------------
! 3. End of routine
!-------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_RotateCoordinates
