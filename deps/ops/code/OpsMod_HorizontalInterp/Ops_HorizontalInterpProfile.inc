!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Horizontal and vertical profile interpolation.  Interpolates by linear
! remapping interpolation, a 4-d field to the locations of a set of profiles.
! Input data can be on a sphere or within a rectangular box. Requested output
! data points must lie within the region defined for the input data.
!
! i=x=lambda, j=y=phi
!
! The four model columns surrounding the lat lon of interest are specified:
!                          x i-1,j              x i, j
!                            _column(4)         _column(3)
!                            _weight(4)         _weight(3)
!                            *mp                *pp
!
!
!                                 * lon,lat
!
!                          x i-1,j-1            x i,j-1
!                            _column(1)         _column(2)
!                            _weight(1)         _weight(2)
!                            *mm                *pm
!
! The weights and column variables are listed in a counter-clockwise 
! direction.
!
! The four surrounding columns in the model field are first vertically
! interpolated to the desired z-coordinate levels, and then horizontally
! interpolated to profile location at every level.
!
! This routine is a modified version of the routine interpolation written
! for SCRIP, and a description is available in the SCRIP user guide.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_HorizontalInterpProfile (Obs,                 &
                                        is,                  &
                                        js,                  &
                                        weights,             &
                                        netcdf_depths,       &
                                        z_depths,            &
                                        in_data,             &
                                        depth_in_s_levels,   &
                                        z_level_data,        &
                                        ts_in,               &
                                        t_weights_in,        &
                                        reported_level_data)

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    RMDI

USE OpsMod_Kinds, ONLY: &
    real64

USE OpsMod_ObsGroupInfo, ONLY: &
    ObsGroupOcean

USE OpsMod_ObsInfo, ONLY: &
    Ob_type

USE OpsMod_VerticalInterp, ONLY: &
    Ops_VerticalInterp1D

IMPLICIT NONE

! Subroutine arguments:
TYPE (Ob_type), INTENT(IN)    :: Obs
INTEGER, INTENT(IN)           :: is(:)
INTEGER, INTENT(IN)           :: js(:)
REAL, INTENT(IN)              :: weights(:,:)
REAL, INTENT(IN)              :: netcdf_depths(:,:,:)
REAL, INTENT(IN)              :: z_depths(:)
REAL, INTENT(IN)              :: in_data(:,:,:,:)
LOGICAL, INTENT(IN)           :: depth_in_s_levels
REAL, INTENT(OUT)             :: z_level_data(:,:)
INTEGER, OPTIONAL, INTENT(IN) :: ts_in(:)
REAL, OPTIONAL, INTENT(IN)    :: t_weights_in(:,:)
REAL, OPTIONAL, INTENT(OUT)   :: reported_level_data(:,:)

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = "Ops_HorizontalInterpProfile"
INTEGER                       :: I
INTEGER                       :: J
INTEGER, ALLOCATABLE          :: ts(:)
REAL, ALLOCATABLE             :: t_weights(:,:)
REAL, ALLOCATABLE             :: model_columns(:,:,:) ! (time,corner,z)
REAL, ALLOCATABLE             :: profile_level_data(:)
REAL                          :: weight_pp
REAL                          :: weight_pm
REAL                          :: weight_mp
REAL                          :: weight_mm
REAL                          :: temp
LOGICAL                       :: failedflag

IF (UseTrace) CALL gen_trace_entry (RoutineName)

failedflag=.FALSE.
! ----------------------------------------------------------------------
!  Section 1.   Initialize
! ----------------------------------------------------------------------

IF (PRESENT (ts_in)) THEN
  ALLOCATE (ts(SIZE (ts_in, DIM = 1)))
  ts(:) = ts_in(:)
ELSE
  ALLOCATE (ts(Obs % header % NumObsLocal))
  ts(:) = 1
END IF

IF (PRESENT (t_weights_in)) THEN
  ALLOCATE (t_weights(SIZE (t_weights_in, DIM = 1),SIZE (t_weights_in, DIM = 2)))
  t_weights(:,:) = t_weights_in(:,:)
ELSE
  ALLOCATE (t_weights(2,Obs % header % NumObsLocal))
  t_weights(1,:) = 1
  t_weights(2,:) = 0
END IF

IF (Obs % header % ObsGroup == ObsGroupOcean .AND. PRESENT (reported_level_data)) THEN
  ! Use model_columns array for background on both reported levels and z levels,
  ! before and after the observation time, and at all four surrounding corners
  ALLOCATE (model_columns(2,4,MAX (SIZE (Obs % PLevelsA, DIM = 2), SIZE (z_depths))))
  ALLOCATE (profile_level_data(MAX (SIZE (Obs % PLevelsA, DIM = 2), SIZE (z_depths))))
ELSE
  ALLOCATE (model_columns(2,4,SIZE (z_depths)))
  ALLOCATE (profile_level_data(SIZE (z_depths)))
END IF

DO i = 1, Obs % header % NumObsLocal

  profile_level_data = RMDI

  weight_mm = weights(1,i)
  weight_pm = weights(2,i)
  weight_pp = weights(3,i)
  weight_mp = weights(4,i)

  IF (is(i) <= 0 .OR. js(i) <= 0) THEN
    CYCLE
  END IF

  ! ----------------------------------------------------------------------
  !      Interpolate to observation locations, model z-depths
  ! ----------------------------------------------------------------------

  IF (depth_in_s_levels) THEN
    CALL Ops_VerticalInterp1D (netcdf_depths(is(i) - 1,js(i) - 1,:), &
                               z_depths,                             &
                               in_data(is(i) - 1,js(i) - 1,:,ts(i)), &
                               model_columns(1,1,:))
    CALL Ops_VerticalInterp1D (netcdf_depths(is(i),js(i) - 1,:), &
                               z_depths,                         &
                               in_data(is(i),js(i) - 1,:,ts(i)), &
                               model_columns(1,2,:))
    CALL Ops_VerticalInterp1D (netcdf_depths(is(i),js(i),:), &
                               z_depths,                     &
                               in_data(is(i),js(i),:,ts(i)), &
                               model_columns(1,3,:))
    CALL Ops_VerticalInterp1D (netcdf_depths(is(i) - 1,js(i),:), &
                               z_depths,                         &
                               in_data(is(i) - 1,js(i),:,ts(i)), &
                               model_columns(1,4,:))

    IF (t_weights(2,i) > 0) THEN
      CALL Ops_VerticalInterp1D (netcdf_depths(is(i) - 1,js(i) - 1,:),   &
                                 z_depths,                               &
                                 in_data(is(i) - 1,js(i) - 1,:,ts(i)+1), &
                                 model_columns(2,1,:))
      CALL Ops_VerticalInterp1D (netcdf_depths(is(i),js(i) - 1,:),   &
                                 z_depths,                           &
                                 in_data(is(i),js(i) - 1,:,ts(i)+1), &
                                 model_columns(2,2,:))
      CALL Ops_VerticalInterp1D (netcdf_depths(is(i),js(i),:),   &
                                 z_depths,                       &
                                 in_data(is(i),js(i),:,ts(i)+1), &
                                 model_columns(2,3,:))
      CALL Ops_VerticalInterp1D (netcdf_depths(is(i) - 1,js(i),:),   &
                                 z_depths,                           &
                                 in_data(is(i) - 1,js(i),:,ts(i)+1), &
                                 model_columns(2,4,:))

    END IF
  ELSE
    ! depth is in z-levels, so netcdf_depths and z_depths are identical
    model_columns(1,1,:size(in_data,dim=3)) = in_data(is(i) - 1,js(i) - 1,:,ts(i))
    model_columns(1,2,:size(in_data,dim=3)) = in_data(is(i),js(i) - 1,:,ts(i))
    model_columns(1,3,:size(in_data,dim=3)) = in_data(is(i),js(i),:,ts(i))
    model_columns(1,4,:size(in_data,dim=3)) = in_data(is(i) - 1,js(i),:,ts(i))

    IF (t_weights(2,i) > 0) THEN
      model_columns(2,1,:size(in_data,dim=3)) = in_data(is(i) - 1,js(i) - 1,:,ts(i)+1)
      model_columns(2,2,:size(in_data,dim=3)) = in_data(is(i),js(i) - 1,:,ts(i)+1)
      model_columns(2,3,:size(in_data,dim=3)) = in_data(is(i),js(i),:,ts(i)+1)
      model_columns(2,4,:size(in_data,dim=3)) = in_data(is(i) - 1,js(i),:,ts(i)+1)
    END IF
  END IF

  !------------------------------------------------------------------------------
  !     Apply weights
  !------------------------------------------------------------------------------

  DO j = 1, SIZE (z_depths)
    IF (model_columns(1,1,j) == RMDI .OR. &
        model_columns(1,2,j) == RMDI .OR. &
        model_columns(1,3,j) == RMDI .OR. &
        model_columns(1,4,j) == RMDI) THEN
      CYCLE
    END IF
    profile_level_data(j) = OpsFn_ApplyProfileOceanWeights (weights,       &
                                                            t_weights,     &
                                                            model_columns, &
                                                            i,             &
                                                            j)

  END DO

  ! Use the model z-levels in the cx structure
  z_level_data(i,:) = profile_level_data(1:SIZE (z_level_data, DIM = 2))

  IF (PRESENT (reported_level_data)) THEN

    profile_level_data = RMDI

    ! ----------------------------------------------------------------------
    !      Interpolate to observation locations, observation depths
    ! ----------------------------------------------------------------------

    CALL Ops_VerticalInterp1D (netcdf_depths(is(i) - 1,js(i) - 1,:), &
                               Obs % PLevelsA(i,:) % value,          &
                               in_data(is(i) - 1,js(i) - 1,:,ts(i)), &
                               model_columns(1,1,:))
    CALL Ops_VerticalInterp1D (netcdf_depths(is(i),js(i) - 1,:), &
                               Obs % PLevelsA(i,:) % value,      &
                               in_data(is(i),js(i) - 1,:,ts(i)), &
                               model_columns(1,2,:))
    CALL Ops_VerticalInterp1D (netcdf_depths(is(i),js(i),:), &
                               Obs % PLevelsA(i,:) % value,  &
                               in_data(is(i),js(i),:,ts(i)), &
                               model_columns(1,3,:))
    CALL Ops_VerticalInterp1D (netcdf_depths(is(i) - 1,js(i),:), &
                               Obs % PLevelsA(i,:) % value,      &
                               in_data(is(i) - 1,js(i),:,ts(i)), &
                               model_columns(1,4,:))

    IF (t_weights(2,i) > 0) THEN
      CALL Ops_VerticalInterp1D (netcdf_depths(is(i) - 1,js(i) - 1,:),     &
                                 Obs % PLevelsA(i,:) % value,              &
                                 in_data(is(i) - 1,js(i) - 1,:,ts(i) + 1), &
                                 model_columns(2,1,:))
      CALL Ops_VerticalInterp1D (netcdf_depths(is(i),js(i) - 1,:),     &
                                 Obs % PLevelsA(i,:) % value,          &
                                 in_data(is(i),js(i) - 1,:,ts(i) + 1), &
                                 model_columns(2,2,:))
      CALL Ops_VerticalInterp1D (netcdf_depths(is(i),js(i),:),     &
                                 Obs % PLevelsA(i,:) % value,      &
                                 in_data(is(i),js(i),:,ts(i) + 1), &
                                 model_columns(2,3,:))
      CALL Ops_VerticalInterp1D (netcdf_depths(is(i) - 1,js(i),:),     &
                                 Obs % PLevelsA(i,:) % value,          &
                                 in_data(is(i) - 1,js(i),:,ts(i) + 1), &
                                 model_columns(2,4,:))
    END IF

    !------------------------------------------------------------------------------
    !     Apply weights
    !------------------------------------------------------------------------------

    DO j = 1, SIZE (z_depths)
      IF (model_columns(1,1,j) == RMDI .OR. &
          model_columns(1,2,j) == RMDI .OR. &
          model_columns(1,3,j) == RMDI .OR. &
          model_columns(1,4,j) == RMDI) THEN
        CYCLE
      END IF
    
      profile_level_data(j) = OpsFn_ApplyProfileOceanWeights (weights,       &
                                                              t_weights,     &
                                                              model_columns, &
                                                              i,             &
                                                              j)
    END DO 

    reported_level_data(i,:) = profile_level_data(1:SIZE (reported_level_data, DIM = 2))

  END IF ! Outputting reported level data

END DO

!------------------------------------------------------------------------------
! [5] Tidy up
!------------------------------------------------------------------------------

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_HorizontalInterpProfile
