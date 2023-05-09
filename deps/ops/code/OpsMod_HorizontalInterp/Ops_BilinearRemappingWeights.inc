!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Compute the weights for a bilinear remapping interpolation scheme.  This
! scheme is appropriate for bilinear interpolation on a general quadrilateral
! grid.
!
! The four model columns surrounding the lat lon of interest are specified:
!                          x i-1,j              x i, j
!                            _corner(4)         _corner(3)
!                            _weight(4)         _weight(3)
!                            *mp                *pp
!
!
!                                 * lon,lat
!
!                          x i-1,j-1            x i,j-1
!                            _corner(1)         _corner(2)
!                            _weight(1)         _weight(2)
!                            *mm                *pm
!
! The weights and corner variables are listed in a counter-clockwise 
! direction.
!-------------------------------------------------------------------------------

SUBROUTINE Ops_BilinearRemappingWeights (lat_corners, &
                                         lon_corners, &
                                         weights,     &
                                         pphi,        &
                                         plam,        &
                                         ldfail)

USE OpsMod_Kinds, ONLY: &
  real64

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(IN)     :: lat_corners(4)
REAL, INTENT(IN)     :: lon_corners(4)
REAL, INTENT(OUT)    :: weights(4)
REAL, INTENT(IN)     :: pphi
REAL, INTENT(IN)     :: plam
LOGICAL, INTENT(OUT) :: ldfail

! Local declarations:
INTEGER              :: jiter
INTEGER              :: itermax
REAL                 :: zphi           ! Geographical location of observation
REAL                 :: zlam
REAL                 :: zphimm         ! Geographical location of surrounding
REAL                 :: zphimp         ! model grid points
REAL                 :: zphipm
REAL                 :: zphipp
REAL                 :: zlammm
REAL                 :: zlammp
REAL                 :: zlampm
REAL                 :: zlampp
REAL                 :: pbiwmm         ! Interpolation weights
REAL                 :: pbiwmp
REAL                 :: pbiwpm
REAL                 :: pbiwpp
REAL                 :: zdth1
REAL                 :: zdth2
REAL                 :: zdth3
REAL                 :: zdthp
REAL                 :: zdph1
REAL                 :: zdph2
REAL                 :: zdph3
REAL                 :: zdphp
REAL                 :: zmat1
REAL                 :: zmat2
REAL                 :: zmat3
REAL                 :: zmat4
REAL                 :: zdeli
REAL                 :: zdelj
REAL                 :: ziguess
REAL                 :: zjguess
REAL                 :: zeps
REAL                 :: zdeterm
REAL                 :: z2pi
REAL                 :: zhpi

! Initialization

! Initialise weights to zero
pbiwmm = 0
pbiwmp = 0
pbiwpm = 0
pbiwpp = 0

! Conversion to radians

zphi = pphi * rad
zlam = plam * rad

zphimm = lat_corners(1) * rad
zphipm = lat_corners(2) * rad
zphipp = lat_corners(3) * rad
zphimp = lat_corners(4) * rad

zlammm = lon_corners(1) * rad
zlampm = lon_corners(2) * rad
zlampp = lon_corners(3) * rad
zlammp = lon_corners(4) * rad

ldfail = .FALSE.

zdth1 = zphipm - zphimm
zdth2 = zphimp - zphimm
zdth3 = zphipp - zphipm - zdth2

zdph1 = zlampm - zlammm
zdph2 = zlammp - zlammm
zdph3 = zlampp - zlampm

z2pi = 2.0_real64 * rpi

IF (zdph1 > 3 * rpi) zdph1 = zdph1 - z2pi
IF (zdph2 > 3 * rpi) zdph2 = zdph2 - z2pi
IF (zdph3 > 3 * rpi) zdph3 = zdph3 - z2pi
IF (zdph1 < -3 * rpi) zdph1 = zdph1 + z2pi
IF (zdph2 < -3 * rpi) zdph2 = zdph2 + z2pi
IF (zdph3 < -3 * rpi) zdph3 = zdph3 + z2pi

 zdph3 = zdph3 - zdph2

ziguess = 0.5
zjguess = 0.5

itermax = 100

zeps = 1.0e-10_real64

!------------------------------------------------------------------------
! Iterate to determine (i,j) in new coordinate system
!------------------------------------------------------------------------

jiter_loop: DO jiter = 1, itermax

  zdthp = zphi - zphimm - zdth1 * ziguess - zdth2 * zjguess - zdth3 * ziguess * zjguess
  zdphp = zlam - zlammm

  zhpi = 0.5 * rpi
  IF (zdphp > 3 * zhpi) zdphp = zdphp - z2pi
  IF (zdphp < -3 * zhpi) zdphp = zdphp + z2pi

  zdphp = zdphp - zdph1 * ziguess - zdph2 * zjguess - zdph3 * ziguess * zjguess

  zmat1 = zdth1 + zdth3 * zjguess
  zmat2 = zdth2 + zdth3 * ziguess
  zmat3 = zdph1 + zdph3 * zjguess
  zmat4 = zdph2 + zdph3 * ziguess

  ! Matrix determinant
  zdeterm = zmat1 * zmat4 - zmat2 * zmat3

  ! increments
  zdeli = (zdthp * zmat4 - zmat2 * zdphp) / zdeterm
  zdelj = (zmat1 * zdphp - zdthp * zmat3) / zdeterm

  IF (ABS (zdeli) < zeps .AND. ABS (zdelj) < zeps) EXIT jiter_loop

  ziguess = ziguess + zdeli
  zjguess = zjguess + zdelj

  ! DJL prevent ziguess and zjguess from going outside the range
  ! 0 to 1
  ! prevents interpolated value going wrong
  ! for example sea ice concentration gt 1

  IF (ziguess < 0) ziguess = 0
  IF (zjguess < 0) zjguess = 0
  IF (ziguess > 1) ziguess = 1
  IF (zjguess > 1) zjguess = 1

END DO jiter_loop

IF (jiter <= itermax) THEN

  ! Successfully found i,j, now compute the weights

  pbiwmm = (1 - ziguess) * (1 - zjguess)
  pbiwmp = (1 - ziguess) * zjguess
  pbiwpm = ziguess * (1 - zjguess)
  pbiwpp = ziguess * zjguess

ELSE IF (jiter > itermax) THEN

  ldfail = .TRUE.

END IF

weights(1) = pbiwmm
weights(2) = pbiwpm
weights(3) = pbiwpp
weights(4) = pbiwmp

END SUBROUTINE Ops_BilinearRemappingWeights
