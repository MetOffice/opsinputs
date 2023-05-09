!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Some utility routines for handling environment variables.
!-------------------------------------------------------------------------------

MODULE OpsMod_EnvUtils

IMPLICIT NONE

INTERFACE ops_get_env
  MODULE PROCEDURE ops_get_env_character
  MODULE PROCEDURE ops_get_env_character_array
  MODULE PROCEDURE ops_get_env_int
END INTERFACE ops_get_env

INTERFACE ops_set_env
  MODULE PROCEDURE ops_set_env_character
END INTERFACE ops_set_env

CONTAINS

include "ops_get_env_character.inc"
include "ops_get_env_character_array.inc"
include "ops_get_env_int.inc"
include "ops_env_is_false.inc"
include "ops_env_is_set.inc"
include "ops_env_is_true.inc"
include "ops_set_env_character.inc"

END MODULE OpsMod_EnvUtils
