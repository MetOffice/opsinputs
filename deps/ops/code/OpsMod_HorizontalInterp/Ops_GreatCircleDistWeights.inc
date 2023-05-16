!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Distance-weighted interpolation scheme (k2dint = 0).  The interpolation
! weights are computed as a weighted sum of the distance between the model grid
! points (A) and the observation point (B). Inherited from NEMOVAR.
!
! Distance (s) is computed using the great-circle distance formula:
!
!    s(AB) = arcos(   sin( phiA ) x sin( phiB )
!                   + cos( phiA ) x cos( phiB ) x cos( lamB - lamA )
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

SUBROUTINE Ops_GreatCircleDistWeights (lat_corners, &
                                       lon_corners, &
                                       weights,     &
                                       pphi,        &
                                       plam)

IMPLICIT NONE

! Subroutine arguments:
REAL, INTENT(IN)  :: lat_corners(4)
REAL, INTENT(IN)  :: lon_corners(4)
REAL, INTENT(OUT) :: weights(4)
REAL, INTENT(IN)  :: pphi
REAL, INTENT(IN)  :: plam

! Local declarations:
REAL              :: p2dmm
REAL              :: p2dmp
REAL              :: p2dpm
REAL              :: p2dpp
REAL              :: zphi2
REAL              :: zlam2
REAL              :: zcola
REAL              :: za2
REAL              :: zb2
REAL              :: zc2
REAL              :: zphimm2
REAL              :: zphimp2
REAL              :: zphipm2
REAL              :: zphipp2
REAL              :: zlammm2
REAL              :: zlammp2
REAL              :: zlampm2
REAL              :: zlampp2
REAL              :: za1mm
REAL              :: za1mp
REAL              :: za1pm
REAL              :: za1pp
REAL              :: zcomm
REAL              :: zcomp
REAL              :: zcopm
REAL              :: zcopp
REAL              :: zb1mm
REAL              :: zb1mp
REAL              :: zb1pm
REAL              :: zb1pp
REAL              :: zc1mm
REAL              :: zc1mp
REAL              :: zc1pm
REAL              :: zc1pp
REAL              :: zsopmpp
REAL              :: zsommmp
REAL              :: zsomm
REAL              :: zsomp
REAL              :: zsopm
REAL              :: zsopp

!------------------------------------------------------------------------
! Distance-weighted interpolation using the great circle formula
!------------------------------------------------------------------------

zphi2 = pphi * rad
zlam2 = plam * rad
zcola = COS (zphi2)
za2 = SIN (zphi2)
zb2 = zcola * COS (zlam2)
zc2 = zcola * SIN (zlam2)

zphimm2 = lat_corners(1) * rad
zphipm2 = lat_corners(2) * rad
zphipp2 = lat_corners(3) * rad
zphimp2 = lat_corners(4) * rad
                                
zlammm2 = lon_corners(1) * rad
zlampm2 = lon_corners(2) * rad
zlampp2 = lon_corners(3) * rad
zlammp2 = lon_corners(4) * rad

za1mm = SIN (zphimm2)
za1mp = SIN (zphimp2)
za1pm = SIN (zphipm2)
za1pp = SIN (zphipp2)

zcomm = COS (zphimm2)
zcomp = COS (zphimp2)
zcopm = COS (zphipm2)
zcopp = COS (zphipp2)

zb1mm = zcomm * COS (zlammm2)
zb1mp = zcomp * COS (zlammp2)
zb1pm = zcopm * COS (zlampm2)
zb1pp = zcopp * COS (zlampp2)

zc1mm = zcomm * SIN (zlammm2)
zc1mp = zcomp * SIN (zlammp2)
zc1pm = zcopm * SIN (zlampm2)
zc1pp = zcopp * SIN (zlampp2)

! Function for arcsin(sqrt(1-x^2) version of great-circle formula

zsomm = Ops_GreatCircleDistance (za1mm, za2, zb1mm, zb2, zc1mm, zc2)
zsomp = Ops_GreatCircleDistance (za1mp, za2, zb1mp, zb2, zc1mp, zc2)
zsopm = Ops_GreatCircleDistance (za1pm, za2, zb1pm, zb2, zc1pm, zc2)
zsopp = Ops_GreatCircleDistance (za1pp, za2, zb1pp, zb2, zc1pp, zc2)

zsopmpp = zsopm * zsopp
zsommmp = zsomm * zsomp

p2dmm = zsomp * zsopmpp
p2dmp = zsomm * zsopmpp
p2dpm = zsopp * zsommmp
p2dpp = zsopm * zsommmp

weights(1) = p2dmm
weights(2) = p2dpm 
weights(3) = p2dpp 
weights(4) = p2dmp 

END SUBROUTINE Ops_GreatCircleDistWeights
