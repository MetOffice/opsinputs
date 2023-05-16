!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Generate Refractivity on model levels (theta levels)
!-------------------------------------------------------------------------------

SUBROUTINE Ops_Refractivity (nNwet,     &
                             nNdry,     &
                             zInter,    &
                             pN,        &
                             pInter,    &
                             qN,        &
                             refracerr, &
                             refrac)

USE GenMod_Constants, ONLY: &
    RMDITol

USE GenMod_Core, ONLY: &
    gen_trace_entry,   &
    gen_trace_exit,    &
    gen_warn,          &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    Epsilon,                             &
    C_virtual,                           &
    g,                                   &
    Pref,                                &
    Kappa,                               &
    Cp,                                  &
    RMDI

USE OpsMod_Constants, ONLY: &
    Nalpha,                 &
    Nbeta

IMPLICIT NONE

! Subroutine arguments:

INTEGER, INTENT(IN)         :: nNwet        ! no. of levels of wet refractivity required
INTEGER, INTENT(IN)         :: nNdry        ! no. of levels of dry refractivity required
REAL, INTENT(IN)            :: zInter(:)    ! heights of pressure levels
REAL, INTENT(IN)            :: pN(:)        ! pressure on refractivity (theta) levels
REAL, INTENT(IN)            :: pInter(:)    ! pressure on zInter levels
REAL, INTENT(IN)            :: qN(:)        ! humidity on refractivity (theta) levels
LOGICAL, INTENT(OUT)        :: refracerr    ! errors in refractivity calculation
REAL, INTENT(INOUT)         :: refrac(:)    ! refrac on refractivity (theta) levels


! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_Refractivity"
INTEGER                     :: i
REAL, ALLOCATABLE           :: ExnerN(:)      ! Exner on refractivity (theta) levels
REAL, ALLOCATABLE           :: ExnerInter(:)  ! Exner on pressure (rho) levels
REAL                        :: T           ! Temperature on refractivity (theta) levels
REAL                        :: Tv          ! Tv on refractivity (theta) level
REAL                        :: Ndry        ! Dry refractivity
REAL                        :: Nwet        ! Wet refractivity
REAL                        :: aval        ! scaled alpha constant
REAL                        :: bval        ! scaled beta constant
INTEGER                     :: numPlevs    ! Number of levels of pInter
INTEGER                     :: numqlevs    ! Numbers of levels of qN
LOGICAL                     :: nonmon      ! non-monotonic pressure warning
LOGICAL                     :: unphys      ! zero or negative pressure warning
LOGICAL                     :: levelerr    ! nlevq greater than nlevp error

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! Get constants into right units

aval = 0.01 * Nalpha    ! Pa
bval = 0.01 * Nbeta     ! kg/kg

! Set up arrays
numPlevs = SIZE (pInter)
numqlevs = SIZE (pN)

ALLOCATE (ExnerN(numqlevs))
ALLOCATE (ExnerInter(numPlevs))

T = RMDI
nonmon = .FALSE.
unphys = .FALSE.
levelerr = .FALSE.
refracerr = .FALSE.

DO i = 1, numPlevs - 1
  IF (pInter(i) - RMDI < RMDITol) THEN  ! pressure missing
    refracerr = .TRUE.
    EXIT
  END IF

  IF (pInter(i) - pInter(i + 1) < 0.0) THEN  ! or non-monotonic pressure
    refracerr = .TRUE.
    nonmon = .TRUE.
    EXIT
  END IF
END DO

IF (ANY (pInter(:) <= 0.0)) THEN       ! pressure zero or negative
  refracerr = .TRUE.
  unphys = .TRUE.
END IF

IF (numqlevs >= numPlevs) THEN  ! nlevq must be < than nlevP
  refracerr = .TRUE.
  levelerr = .TRUE.
END IF

! only proceed if pressure is valid
IF (refracerr) THEN
  IF (nonmon) THEN
    CALL gen_warn (RoutineName,              &
                   "Pressure non-monotonic", &
                   Local = .TRUE.)
  ELSE IF (unphys) THEN
    CALL gen_warn (RoutineName,        &
                   "Pressure <= zero", &
                   Local = .TRUE.)
  ELSE IF (levelerr) THEN
    CALL gen_warn (RoutineName,           &
                   "Too many wet levels", &
                   Local = .TRUE.)
  ELSE
    CALL gen_warn (RoutineName,        &
                   "Pressure missing", &
                   Local = .TRUE.)
  END IF
ELSE

  ! Calculate exner on pressure levels.

  ExnerInter = (pInter / Pref) ** Kappa

  ! Calculate the refractivity on the b levels

  DO i = 1, numqlevs

    ! Calculate Exner on the refractivity level.
    ExnerN = (pN(i) / Pref) ** Kappa

    ! Calculate mean layer Tv using ND definition

    Tv = g * (zInter(i + 1) - zInter(i)) * ExnerN(i) / &
        (Cp * (ExnerInter(i) - ExnerInter(i + 1)))

    IF (i > nNwet) THEN

      T = Tv

      ! No wet component

      Nwet = 0.0

    ELSE

      T = Tv / (1.0 + C_virtual * qN(i))

      ! Wet component

      Nwet = bval * pN(i) * qN(i) / (T ** 2 * (Epsilon + (1.0 - Epsilon) * qN(i)))

    END IF

    IF (i > nNdry) THEN
      ! No dry component

      Ndry = 0.0

    ELSE
      ! Dry component

      Ndry = aval * pN(i) / T

    END IF

    refrac(i) = Ndry + Nwet

  END DO

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_Refractivity
