!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! ODB compatible kind values.
!-------------------------------------------------------------------------------

MODULE OpsMod_ODBKinds

USE OpsMod_Kinds, ONLY: &
  integer32,            &
  real64

IMPLICIT NONE

SAVE

! Public declarations:
INTEGER, PARAMETER :: odb_int = integer32 ! ODB Integer size
INTEGER, PARAMETER :: odb_real = real64   ! ODB Real size

END MODULE OpsMod_ODBKinds
