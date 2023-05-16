!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!-------------------------------------------------------------------------------
! Utility module for handling command line arguments.
!-------------------------------------------------------------------------------

MODULE OpsMod_Argument

USE GenMod_MiscUMScienceConstants, ONLY: &
  IMDI,                                  &
  RMDI

IMPLICIT NONE

! Valid argument types:
INTEGER, PARAMETER                  :: type_integer = 1
INTEGER, PARAMETER                  :: type_real = 2
INTEGER, PARAMETER                  :: type_logical = 3
INTEGER, PARAMETER                  :: type_character = 4
INTEGER, PARAMETER                  :: type_integer_list = 5

! Individual argument type:
TYPE argument_type
  CHARACTER(len=2)                  :: short_opt = ""
  CHARACTER(len=50)                 :: long_opt = ""
  INTEGER                           :: type = 1
  INTEGER                           :: int_val = IMDI
  REAL                              :: real_val = RMDI
  LOGICAL                           :: logical_val = .FALSE.
  CHARACTER(len=256)                :: char_val = ""
  INTEGER, ALLOCATABLE              :: int_list_val(:)
  LOGICAL                           :: mandatory = .FALSE.
  LOGICAL                           :: was_set = .FALSE.
  CHARACTER(len=256)                :: description = ""
END TYPE argument_type

! Arguments collection as a whole:
TYPE arguments_type
  TYPE (argument_type), ALLOCATABLE :: args(:)
CONTAINS
  PROCEDURE                         :: OpsMod_Argument_add_arg_character
  PROCEDURE                         :: OpsMod_Argument_add_arg_integer
  PROCEDURE                         :: OpsMod_Argument_add_arg_integer_array
  PROCEDURE                         :: OpsMod_Argument_add_arg_logical
  PROCEDURE                         :: OpsMod_Argument_get_arg_character
  PROCEDURE                         :: OpsMod_Argument_get_arg_integer
  PROCEDURE                         :: OpsMod_Argument_get_arg_integer_list
  PROCEDURE                         :: OpsMod_Argument_get_arg_logical
  PROCEDURE                         :: OpsMod_Argument_get_arg_real
  PROCEDURE                         :: process => OpsMod_Argument_process_command_line
  PROCEDURE                         :: usage => OpsMod_Argument_usage
  PROCEDURE                         :: was_set => OpsMod_Argument_was_set
  GENERIC                           :: add_arg => OpsMod_Argument_add_arg_character,     &
                                                  OpsMod_Argument_add_arg_integer,       &
                                                  OpsMod_Argument_add_arg_integer_array, &
                                                  OpsMod_Argument_add_arg_logical
  GENERIC                           :: get_arg => OpsMod_Argument_get_arg_character,    &
                                                  OpsMod_Argument_get_arg_integer,      &
                                                  OpsMod_Argument_get_arg_integer_list, &
                                                  OpsMod_Argument_get_arg_logical,      &
                                                  OpsMod_Argument_get_arg_real
END TYPE arguments_type

CONTAINS

INCLUDE 'Ops_ConvertCommandLineList.inc'
INCLUDE 'Ops_GetCommandLine.inc'
INCLUDE 'OpsMod_Argument_add_arg_character.inc'
INCLUDE 'OpsMod_Argument_add_arg_integer.inc'
INCLUDE 'OpsMod_Argument_add_arg_integer_array.inc'
INCLUDE 'OpsMod_Argument_add_arg_logical.inc'
INCLUDE 'OpsMod_Argument_get_arg_character.inc'
INCLUDE 'OpsMod_Argument_get_arg_integer.inc'
INCLUDE 'OpsMod_Argument_get_arg_integer_list.inc'
INCLUDE 'OpsMod_Argument_get_arg_logical.inc'
INCLUDE 'OpsMod_Argument_get_arg_real.inc'
INCLUDE 'OpsMod_Argument_process_command_line.inc'
INCLUDE 'OpsMod_Argument_usage.inc'
INCLUDE 'OpsMod_Argument_was_set.inc'

END MODULE OpsMod_Argument
