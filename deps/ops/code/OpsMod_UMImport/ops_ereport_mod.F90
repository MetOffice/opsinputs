! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! *****************************COPYRIGHT*******************************
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Misc.

MODULE Ops_Ereport_Mod

IMPLICIT NONE

CONTAINS

SUBROUTINE creport (routineName, &
                    message) BIND (c, NAME = "ops_f_ereport")

USE f_shum_string_conv_mod, ONLY: &
  f_shum_c2f_string

USE GenMod_Core, ONLY: &
  gen_fail

USE, INTRINSIC :: ISO_C_BINDING, ONLY: &
  C_CHAR

IMPLICIT NONE

CHARACTER(kind=C_CHAR,len=1) :: RoutineName(*)
CHARACTER(kind=C_CHAR,len=1) :: message(*)

CALL gen_fail (f_shum_c2f_string (RoutineName), &
               f_shum_c2f_string (message))

END SUBROUTINE creport

END MODULE Ops_Ereport_Mod
