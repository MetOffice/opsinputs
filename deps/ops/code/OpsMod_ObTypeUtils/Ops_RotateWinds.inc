!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Rotates Cx winds to produce true wind component values.
!
! Inputs: Cx - Cx structure with wind components wrt model grid.
!
! Outputs: Cx  - Same structure but with wind components wrt true lat-long
!
! Method: - Standard UM rotation routines.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_RotateWinds (Cx,       &
                            ObsLats,  &
                            ObsLongs)

USE GenMod_Control, ONLY: &
    DebugMode

USE GenMod_Core,ONLY: &
    gen_message,      &
    gen_trace_entry,  &
    gen_trace_exit,   &
    UseTrace

USE GenMod_Utilities, ONLY: &
    Gen_LatLon_to_Eq

USE OpsMod_CXInfo, ONLY: &
    Cx_type

USE OpsMod_ObsInfo, ONLY: &
    Ops_Alloc

USE OpsMod_Process, ONLY: &
    ProcessMode

USE OpsMod_Utilities, ONLY: &
    Ops_WCoeff,             &
    Ops_WEq_to_ll

IMPLICIT NONE

! Subroutine arguments:
TYPE (Cx_type), INTENT(INOUT) :: Cx          ! Contains data on obs
REAL, INTENT(IN)              :: ObsLats(:)  ! Array of true latitudes
REAL, INTENT(IN)              :: ObsLongs(:) ! Array of true longitudes

! Local declarations:
CHARACTER(len=*), PARAMETER   :: RoutineName = 'Ops_RotateWinds'
INTEGER                       :: Ilev         ! Loop variable
REAL, ALLOCATABLE             :: EqLat(:)     ! Latitudes on equatorial grid
REAL, ALLOCATABLE             :: EqLon(:)     ! Longitudes on equatorial grid
REAL, ALLOCATABLE             :: Coeff1(:)    ! Coefficients for rotation
REAL, ALLOCATABLE             :: Coeff2(:)    ! Coefficients for rotation
REAL, ALLOCATABLE             :: Utrue(:)     ! Array for unrotated wind u component
REAL, ALLOCATABLE             :: Vtrue(:)     ! Array for unrotated wind v component

IF (UseTrace) CALL gen_trace_entry (RoutineName)

!--------------
! 1. Initialise
!--------------

IF (Cx % header % NumLocal > 0) THEN

  ALLOCATE (Coeff1(Cx % header % NumLocal))
  ALLOCATE (Coeff2(Cx % header % NumLocal))
  ALLOCATE (EqLon(Cx % header % NumLocal))
  ALLOCATE (EqLat(Cx % header % NumLocal))
  ALLOCATE (Utrue(Cx % header % NumLocal))
  ALLOCATE (Vtrue(Cx % header % NumLocal))

  !--------------------------------------------
  ! 1.1 Calculate longitudes on equatorial grid
  !--------------------------------------------

  CALL Gen_LatLon_to_Eq (ObsLats(:),             & ! in
                         ObsLongs(:),            & ! in
                         EqLat(:),               & ! out
                         EqLon(:),               & ! out
                         Cx % header % PoleLat,  & ! in
                         Cx % header % PoleLong)   ! in

  DEALLOCATE (EqLat) ! Don't need these

  !----------------------------------------
  ! 1.2 Get rotation coefficients for winds
  !----------------------------------------

  CALL Ops_WCoeff (Coeff1(:),              & ! out
                   Coeff2(:),              & ! out
                   ObsLongs(:),            & ! in
                   EqLon(:),               & ! in
                   Cx % header % PoleLat,  & ! in
                   Cx % header % PoleLong, & ! in
                   Cx % header % NumLocal)   ! in

  DEALLOCATE (EqLon)

  !---------------------------------
  ! 2. Rotate 10m wind components
  !---------------------------------

  IF (Cx % header % u10 % Present .AND. &
      Cx % header % v10 % Present) THEN
    IF (ProcessMode >= DebugMode) THEN
      CALL gen_message (RoutineName,                    &
                        "Rotating 10m wind components")
    END IF

    CALL Ops_WEq_to_ll (Coeff1(:),              &
                        Coeff2(:),              &
                        Cx % u10(:),            &
                        Cx % v10(:),            &
                        Utrue(:),               &
                        Vtrue(:),               &
                        Cx % header % NumLocal, &
                        Cx % header % NumLocal)

    ! Put true values into Cx structure

    CALL Ops_Alloc (Cx % Header % u10_orig, &
                    'Cx % u10_orig',        &
                    Cx % header % NumLocal, &
                    Cx % u10_orig)

    CALL Ops_Alloc (Cx % Header % v10_orig, &
                    'Cx % v10_orig',        &
                    Cx % header % NumLocal, &
                    Cx % v10_orig)

    Cx % u10_orig = Cx % u10
    Cx % v10_orig = Cx % v10

    Cx % u10(:) = Utrue(:)
    Cx % v10(:) = Vtrue(:)

  END IF

  !--------------------------------------
  ! 2. Rotate model level wind components
  !--------------------------------------

  IF (Cx % header % u % Present .AND. &
      Cx % header % v % Present) THEN
    IF (ProcessMode >= DebugMode) THEN
      CALL gen_message (RoutineName,                            &
                        "Rotating upper level wind components")
    END IF

    CALL Ops_Alloc (Cx % Header % u_orig,                  &
                    'Cx % u_orig',                         &
                    Cx % header % NumLocal,                &
                    Cx % u_orig,                           &
                    num_levels = Cx % header % u % NumLev)

    CALL Ops_Alloc (Cx % Header % v_orig,                  &
                    'Cx % v_orig',                         &
                    Cx % header % NumLocal,                &
                    Cx % v_orig,                           &
                    num_levels = Cx % header % v % NumLev)

    Cx % u_orig = Cx % u
    Cx % v_orig = Cx % v

    DO Ilev = 1, Cx % Header % u % NumLev

      CALL Ops_WEq_to_ll (Coeff1(:),              &
                          Coeff2(:),              &
                          Cx % u(:,Ilev),         &
                          Cx % v(:,Ilev),         &
                          Utrue(:),               &
                          Vtrue(:),               &
                          Cx % header % NumLocal, &
                          Cx % header % NumLocal)

      ! Put true values into Cx structure
      Cx % u(:,ILev) = Utrue(:)
      Cx % v(:,Ilev) = Vtrue(:)

    END DO

  END IF

  !-------------------------------
  ! 3. Tidy up and exit subroutine
  !-------------------------------

  DEALLOCATE (Vtrue)
  DEALLOCATE (Utrue)
  DEALLOCATE (Coeff2)
  DEALLOCATE (Coeff1)

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END  SUBROUTINE Ops_RotateWinds
