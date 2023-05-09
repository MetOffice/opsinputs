!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!------------------------------------------------------------------------------
! Calculates the lifting condensation level (LCL) by taking a parcel from the
! top of the surface layer.
!
! Method:
!    The calculation is based on Section 1.3.2 of UMDP27:
!    1. Assume the top of the surface layer where convective plumes originate
!       to be 0.1 times the boundary layer depth. Find the theta vertical level
!       just above this level: call the found level kplume
!    2. Find the pressure at the level, k=kplume: call this p_kplume
!    3. Find the specific humidity at the level, k=kplume: call this q_kplume
!    4. Find the temperature at k=kplume: call this T_kplume
!    5. Apply (1.3.5) of UMDP27 (or (16) of Bolton) to calculate vapour pressure
!       at k=kplume: call this e_kplume
!       N.B. UMDP27 e_kplume is in hPa whereas p_kplume is in Pa
!    6. Apply (1.3.6) of UMDP27 (or (21) of Bolton) to calculate the temperature
!       at LCL: call this T_LCL
!    7. Using (1.3.7) UMDP27 calculate pressure (on theta level) at LCL: call
!       p_LCL
!    8. Find the nearest model theta level where the pressure is closest to
!       p_LCL: call this kLCL
!    9. Find the physical height of the model level kLCL
!------------------------------------------------------------------------------

SUBROUTINE Ops_LCL (CX,            &
                    ModelOb,       &
                    Lev_theta_LCL)

USE GenMod_Core, ONLY: &
    gen_fail,          &
    gen_trace_entry,   &
    gen_trace_exit,    &
    UseTrace

USE GenMod_MiscUMScienceConstants, ONLY: &
    Epsilon,                             &
    Kappa

USE OpsMod_CXInfo, ONLY: &
    CX_Type

USE OpsMod_ModelObInfo, ONLY: &
    ModelOb_type

IMPLICIT NONE

! Subroutine arguments:
TYPE (CX_Type), INTENT(IN)      :: CX
TYPE (ModelOB_type), INTENT(IN) :: ModelOb
INTEGER, INTENT(OUT)            :: Lev_theta_LCL(:)  ! array of theta levels
                                                        ! nearest to LCL's
! Local declarations:
INTEGER                         :: NumLocal
INTEGER                         :: NumCXLev
INTEGER                         :: iLev
INTEGER                         :: iob
REAL, ALLOCATABLE               :: Height_theta_AGL(:)  ! heights of theta levels above ground
REAL                            :: Top_Surface_Layer
INTEGER                         :: kplume               ! level where ascent starts
REAL                            :: p_kplume
REAL                            :: q_kplume
REAL                            :: T_kplume
REAL                            :: e_kplume
REAL                            :: T_LCL
REAL                            :: p_LCL              ! temp and pressure at LCL
INTEGER                         :: kLCL(1)            ! theta level nearest to LCL
CHARACTER(len=256)              :: ErrorMessage(2)
CHARACTER(len=*), PARAMETER     :: RoutineName = "Ops_LCL"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

NumCxLev = CX % Header % NumLevels
NumLocal = CX % Header % NumLocal

IF (CX % Header % NewDynamics) THEN

  ALLOCATE (Height_theta_AGL(NumCxLev))

  ! Go through each observation and find LCL
  DO iob = 1, NumLocal

    Height_theta_AGL = ModelOb % z(iob,:) - ModelOb % orog(iob)

    ! top of the surface layer: this is assumed to be 0.1 x (boundary layer depth)
    Top_Surface_Layer = 0.1 * CX % BLheight(iob)

    ! theta level that coincides with or just above Top_Surface_Layer
    DO iLev = 1, NumCxLev
      IF (Height_theta_AGL(iLev) >= Top_Surface_Layer) EXIT
    END DO
    kplume = iLev

    ! pressure, specific humidity, temperature at the level, kplume
    p_kplume = CX % PLevelsB(iob,kplume)
    q_kplume = ModelOb % q(iob,kplume)
    T_kplume = CX % T_theta(iob,kplume)

    ! vapour pressure at the level, kplume
    ! (1.3.5) in UMDP27
    e_kplume = q_kplume * p_kplume / (100 * Epsilon)  ! N.B. e_kplume in hPa unlike p_kplume

    ! temperature and pressure at LCL
    ! (1.3.6) and the first equation of (1.3.7) in UMDP27
    T_LCL = 55.0 + 2840.0 / (3.5 * LOG (T_kplume) - LOG (e_kplume) - 4.805)
    p_LCL = p_kplume * (T_LCL / T_kplume) ** (1 / Kappa)

    ! find theta level at which pressure is closest to p_LCL
    kLCL = MINLOC (ABS (CX % PLevelsB(iob,:) - p_LCL))
    Lev_theta_LCL(iob) = kLCL(1)

  END DO  ! iob

ELSE
  
  WRITE (ErrorMessage(1), '(A,L1)') 'CX % Header % NewDynamics = ', CX % Header % NewDynamics
  ErrorMessage(2) = 'CX is pre-NewDynamics!'

  CALL gen_fail (RoutineName,       &
                 ErrorMessage(1:2))

END IF  ! (CX % Header % NewDynamics)

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_LCL
