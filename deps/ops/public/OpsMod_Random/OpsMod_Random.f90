!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Random number variables, constants and routines.
!-------------------------------------------------------------------------------

MODULE OpsMod_Random

USE OpsMod_Kinds, ONLY: &
    integer64

IMPLICIT NONE

SAVE

PRIVATE

! Public declarations:
INTEGER, PARAMETER      :: meto_method = 1
INTEGER, PARAMETER      :: intrinsic_method = 2

! Private declarations:
INTEGER                 :: random_number_method = meto_method
LOGICAL                 :: initialise_with_clock = .FALSE.
LOGICAL                 :: initialised = .FALSE.

! Random number generator values (meto)
INTEGER(kind=integer64) :: IntRand = 0
INTEGER(kind=integer64) :: meto_m = 2147483648_integer64
INTEGER                 :: meto_a = 1103515245
INTEGER                 :: meto_c = 12345
INTEGER(kind=integer64) :: initializer = 1956027600
INTEGER                 :: junk_size = 100

INTERFACE Ops_RandomNumber
  MODULE PROCEDURE Ops_RandomNumber_array1d
END INTERFACE Ops_RandomNumber

PUBLIC                  :: Ops_InitRandom
PUBLIC                  :: Ops_RandomNumber
PUBLIC                  :: OpsFn_GetRandomSeed
PUBLIC                  :: ops_gaussian_mapping

CONTAINS

INCLUDE "Ops_InitRandom.inc"
INCLUDE "Ops_RandomSeed.inc"
INCLUDE "Ops_RandomNumber_array1d.inc"
INCLUDE "Ops_RandomNumberMeto_array1d.inc"
INCLUDE "Ops_RandomNumberInt_array1d.inc"
INCLUDE "Ops_ReadRandomControlNL.inc"
INCLUDE "OpsFn_GetRandomSeed.inc"

INCLUDE "ops_gaussian_mapping.inc"

END MODULE OpsMod_Random
