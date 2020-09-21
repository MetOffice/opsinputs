!-----------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-----------------------------------------------------------------------
! Calculates u and v components of wind on standard latitude-longitude
! grid by rotating wind components on equatorial latitude-longitude (eq)
! grid.
!-----------------------------------------------------------------------

SUBROUTINE Ops_WEq_to_ll (COEFF1, &
                          COEFF2, &
                          U_EQ,   &
                          V_EQ,   &
                          U,      &
                          V,      &
                          POINTS, &
                          POINTS2)

USE GenMod_Core, ONLY: &
  gen_trace_entry,     &
  gen_trace_exit,      &
  UseTrace

IMPLICIT NONE

! Subroutine arguments:
INTEGER                     :: POINTS            ! Number of points to be processed
INTEGER                     :: POINTS2           ! Number of v points to be processed
REAL                        :: COEFF1(POINTS)    ! Coefficient of rotation no 1
REAL                        :: COEFF2(POINTS)    ! Coefficient of rotation no 2
REAL                        :: U_EQ(POINTS)      ! u component of wind on equatorial grid
REAL                        :: V_EQ(POINTS)      ! v component of wind on equatorial grid
REAL                        :: U(POINTS)         ! u component of wind on lat-lon grid
REAL                        :: V(POINTS)         ! v component of wind on lat-lon grid

! Local declarations:
INTEGER                     :: I
CHARACTER(len=*), PARAMETER :: RoutineName = "Ops_WEq_to_ll"

IF (UseTrace) CALL gen_trace_entry (RoutineName)

! 1. Transform wind components

DO I = 1, POINTS
  U(I) = COEFF1(I) * U_EQ(I) + COEFF2(I) * V_EQ(I)
END DO

! On a C grid number of u,v points processed differs by row_length

DO I = 1, POINTS2
  V(I) = COEFF1(I) * V_EQ(I) - COEFF2(I) * U_EQ(I)
END DO

IF (UseTrace) CALL gen_trace_exit (RoutineName)

END SUBROUTINE Ops_WEq_to_ll
