!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Generate Refractivity on model levels (theta levels)
!-------------------------------------------------------------------------------

SUBROUTINE Ops_RefractivityDeriv (nNwet,     &
                                  nNdry,     &
                                  zInter,    &
                                  pN,        &
                                  pInter,    &
                                  qN,        &
                                  refracerr, &
                                  refrac,    &
                                  dpN_dP,    &
                                  dref_dP,   &
                                  dref_dq)

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
REAL, INTENT(IN)            :: zInter(:)    ! heights of pressure (rho) levels
REAL, INTENT(IN)            :: pN(:)        ! pressure on refractivity (theta) levels
REAL, INTENT(IN)            :: pInter(:)    ! pressure on zInter levels
REAL, INTENT(IN)            :: qN(:)        ! humidity on refractivity (theta) levels
LOGICAL, INTENT(OUT)        :: refracerr    ! errors in refractivity calculation
REAL, INTENT(INOUT)         :: refrac(:)    ! refractivity
REAL, INTENT(IN)            :: dpN_dP(:,:)  ! p_on_theta derivatives wrt P_on_rho levels
REAL, INTENT(INOUT)         :: dref_dp(:,:) ! refrac derivatives wrt P on rho levels
REAL, INTENT(INOUT)         :: dref_dq(:,:) ! refrac derivatites wrt q on theta levels

! Local declarations:
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_RefractivityDeriv"
INTEGER                     :: i
INTEGER                     :: numPlevs    ! Number of levels of pInter
INTEGER                     :: numqlevs    ! Numbers of levels of qN
INTEGER                     :: maxlevs     ! number of levels to loop over
REAL                        :: ExnerN      ! Exner on refractivity (theta) levels
REAL, ALLOCATABLE           :: ExnerInter(:)  ! Exner on pressure (rho) levels
REAL                        :: T           ! Temperature on refractivity (theta) level
REAL                        :: Tv          ! Tv on refractivity (theta) level
REAL                        :: Ndry        ! Dry refractivity
REAL                        :: Nwet        ! Wet refractivity
REAL                        :: aval        ! scaled alpha constant
REAL                        :: bval        ! scaled beta constant
REAL,ALLOCATABLE            :: dEx_dpInter(:,:) ! derivatives of exner wrt p on rho levels
REAL,ALLOCATABLE            :: dExN_dpN(:,:) ! derivatives of exner wrt p on theta levels
REAL,ALLOCATABLE            :: dTv_dEx(:,:) ! derivatives of Tv wrt exner on rho levels
REAL,ALLOCATABLE            :: dTv_dExN(:,:) ! derivatives of Tv wrt exner on theta levels
REAL,ALLOCATABLE            :: dT_dTv(:,:) ! derivatives of T wrt Tv on theta levels
REAL,ALLOCATABLE            :: dT_dq(:,:) ! derivatives of T wrt q on theta levels
REAL,ALLOCATABLE            :: dref_dpN(:,:) ! derivatives of refrac wrt p on theta levels
REAL,ALLOCATABLE            :: dref_dT(:,:) ! derivatives of refrac wrt T on theta levels
REAL,ALLOCATABLE            :: m1(:,:)
REAL,ALLOCATABLE            :: m2(:,:)
REAL,ALLOCATABLE            :: m3(:,:)
REAL,ALLOCATABLE            :: m4(:,:)
LOGICAL                     :: nonmon      ! non-monotonic pressure warning
LOGICAL                     :: unphys      ! zero or negative pressure warning
LOGICAL                     :: levelerr    ! numqlevs greater than numplevs error

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! Get constants into right units

aval = 0.01 * Nalpha    ! Pa
bval = 0.01 * Nbeta     ! kg/kg

! initialise matrices

! Set up arrays
numPlevs = SIZE (pInter)
numqlevs = SIZE (pN)
maxlevs = MAX (nNdry, nNwet)

ALLOCATE (ExnerInter(numPlevs))
ALLOCATE (dEx_dpInter(numPlevs,numPlevs))
ALLOCATE (dExN_dpN(numqlevs,numqlevs))
ALLOCATE (dTv_dEx(numqlevs,numPlevs))
ALLOCATE (dTv_dExN(numqlevs,numqlevs))
ALLOCATE (dT_dTv(numqlevs,numqlevs))
ALLOCATE (dT_dq(numqlevs,numqlevs))
ALLOCATE (dref_dpN(numqlevs,numqlevs))
ALLOCATE (dref_dT(numqlevs,numqlevs))
ALLOCATE (m1(numqlevs,numqlevs))
ALLOCATE (m2(numqlevs,numPlevs))
ALLOCATE (m3(numqlevs,numqlevs))
ALLOCATE (m4(numqlevs,numqlevs))

ExnerInter(:) = 0.0
dEx_dpInter(:,:) = 0.0
dExN_dpN(:,:) = 0.0
dTv_dEx(:,:) = 0.0
dTv_dExN(:,:) = 0.0
dT_dTv(:,:) = 0.0
dT_dq(:,:) = 0.0
dref_dpN(:,:) = 0.0
dref_dT(:,:) = 0.0
dref_dp(:,:) = 0.0
dref_dq(:,:) = 0.0
m1(:,:) = 0.0
m2(:,:) = 0.0
m3(:,:) = 0.0
m4(:,:) = 0.0

T = RMDI
nonmon = .FALSE.
unphys = .FALSE.
levelerr = .FALSE.

DO i = 1, maxlevs - 1
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

IF (numqlevs >= numPlevs) THEN  ! numqlevs must be < than numplevs
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

  ! Calculate exner on intermediate levels.

  ExnerInter = (pInter / Pref) ** Kappa

  ! Calculate derivative w.r.t pInter
  DO i = 1, maxlevs

    dEx_dpInter(i,i) = Kappa * (pInter(i)) ** (Kappa - 1.0) / (Pref ** Kappa)

  END DO

  ! Calculate the refractivity on the b levels

  DO i = 1, maxlevs - 1

    ! Calculate Exner on the refractivity level.
    ExnerN = (pN(i) / Pref) ** Kappa

    ! Calculate derivative w.r.t pN
    dExN_dpN(i,i) = Kappa * (pN(i) ** (Kappa - 1.0)) / (Pref ** Kappa)

    ! Calculate mean layer Tv using ND definition

    Tv = g * (zInter(i + 1) - zInter(i)) * ExnerN / &
        (Cp * (ExnerInter(i) - ExnerInter(i + 1)))

    ! Calculate derivative w.r.t. Exners

    dTv_dExN(i,i) = Tv / ExnerN
    dTv_dEx(i,i) = -Tv / (ExnerInter(i) - ExnerInter(i + 1))
    dTv_dEx(i,i + 1) = Tv / (ExnerInter(i) - ExnerInter(i + 1))

    IF (i > nNwet) THEN

      T = Tv

      ! Calculate derivative w.r.t. Tv

      dT_dTv(i,i) = 1.0

      ! No wet component

      Nwet = 0.0

    ELSE

      T = Tv / (1.0 + C_virtual * qN(i))

      ! Calculate derivative w.r.t. Tv and q

      dT_dTv(i,i) = 1.0 / (1.0 + C_virtual * qN(i))
      dT_dq (i,i) = -C_virtual * T / (1.0 + C_virtual * qN(i))

      ! Wet component

      Nwet = bval * pN(i) * qN(i) / (T ** 2 * (Epsilon + (1.0 - Epsilon) * qN(i)))

      ! Calculate derivative with regards to q on theta

      dref_dq(i,i) = bval * pN(i) * Epsilon / (T * (Epsilon + (1.0 - Epsilon) * qN(i))) ** 2

    END IF

    IF (i > nNdry) THEN

      ! No dry component

      Ndry = 0.0

    ELSE

      ! Dry component

      Ndry = aval * pN(i) / T

   END IF

     refrac(i) = Ndry + Nwet

     ! Calculate derivative w.r.t p on theta and T

     dref_dpN(i,i) = refrac(i) / pN(i)
     dref_dT (i,i) = -(Ndry + 2.0 * Nwet) / T

  END DO

  dref_dp(:,:) = MATMUL (dref_dpN, dpN_dP)

  m1(:,:) = MATMUL (dref_dT, dT_dTv)
  m2(:,:) = MATMUL (m1, dTv_dEx)
  dref_dp(:,:) = dref_dp(:,:) + MATMUL (m2, dEx_dpInter)

  m3(:,:) = MATMUL (m1, dTv_dExN)
  m4(:,:) = MATMUL (m3, dExN_dpN)
  dref_dp(:,:) = dref_dp(:,:) + MATMUL (m4, dpN_dP)

  ! Account for negative pressure
  ! dref_dP = MATMUL(dref_dP,dPlocal_dPin)

  dref_dq(:,:) = dref_dq(:,:) + MATMUL (dref_dT, dT_dq)

END IF

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_RefractivityDeriv
