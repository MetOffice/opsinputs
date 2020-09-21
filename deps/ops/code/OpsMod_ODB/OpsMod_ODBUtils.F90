!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Contains utility ODB code for setting ODB table values.
!-------------------------------------------------------------------------------

MODULE OpsMod_ODBUtils

IMPLICIT NONE

SAVE

! Interface blocks:

INTERFACE Ops_SetODBVal
  MODULE PROCEDURE Ops_SetODBVal_array_char
  MODULE PROCEDURE Ops_SetODBVal_array_integer
  MODULE PROCEDURE Ops_SetODBVal_array_real32
  MODULE PROCEDURE Ops_SetODBVal_array_real64
  MODULE PROCEDURE Ops_SetODBVal_scalar_char
  MODULE PROCEDURE Ops_SetODBVal_scalar_integer
  MODULE PROCEDURE Ops_SetODBVal_scalar_real
  MODULE PROCEDURE Ops_SetODBVal_s_to_a_char
  MODULE PROCEDURE Ops_SetODBVal_s_to_a_integer
  MODULE PROCEDURE Ops_SetODBVal_s_to_a_real
END INTERFACE

INTERFACE Ops_SetODBBit
  MODULE PROCEDURE Ops_SetODBBit_scalar
  MODULE PROCEDURE Ops_SetODBBit_scalar_to_array
END INTERFACE

INTERFACE Ops_AndODBBits
  MODULE PROCEDURE Ops_AndODBBits_scalar_to_array
END INTERFACE

INTERFACE Ops_OrODBBits
  MODULE PROCEDURE Ops_OrODBBits_array
  MODULE PROCEDURE Ops_OrODBBits_scalar_to_array
END INTERFACE

INTERFACE OpsFn_ConvertToODBDate
  MODULE PROCEDURE OpsFn_ConvertToODBDate_real
END INTERFACE

INTERFACE OpsFn_ConvertToODBTime
  MODULE PROCEDURE OpsFn_ConvertToODBTime_real
END INTERFACE

!-------------------------------------------------------------------------------

CONTAINS

INCLUDE 'Ops_AndODBBits_scalar_to_array.inc'
INCLUDE 'Ops_CloseODB.inc'
INCLUDE 'Ops_LockODB.inc'
#ifndef NO_ODB
#include "Ops_OpenODB.inc"
#endif
INCLUDE 'Ops_OrODBBits_array.inc'
INCLUDE 'Ops_OrODBBits_scalar_to_array.inc'
INCLUDE 'Ops_ReadODBControlNL.inc'
INCLUDE 'Ops_ReadODBMergeNL.inc'
INCLUDE 'Ops_RunODBQuery.inc'
INCLUDE 'Ops_SetODBBit_scalar.inc'
INCLUDE 'Ops_SetODBBit_scalar_to_array.inc'
INCLUDE 'Ops_SetODBVal_array_char.inc'
INCLUDE 'Ops_SetODBVal_array_integer.inc'
INCLUDE 'Ops_SetODBVal_array_real32.inc'
INCLUDE 'Ops_SetODBVal_array_real64.inc'
INCLUDE 'Ops_SetODBVal_s_to_a_char.inc'
INCLUDE 'Ops_SetODBVal_s_to_a_integer.inc'
INCLUDE 'Ops_SetODBVal_s_to_a_real.inc'
INCLUDE 'Ops_SetODBVal_scalar_char.inc'
INCLUDE 'Ops_SetODBVal_scalar_integer.inc'
INCLUDE 'Ops_SetODBVal_scalar_real.inc'
INCLUDE 'Ops_UnlockODB.inc'
INCLUDE 'Ops_WriteODBTable.inc'
INCLUDE 'OpsFn_ConvertToODBDate_real.inc'
INCLUDE 'OpsFn_ConvertToODBTime_real.inc'
INCLUDE 'OpsFn_IsTable.inc'
INCLUDE 'OpsFn_ODBExists.inc'
INCLUDE 'OpsFn_RunidToNum.inc'
INCLUDE 'OpsODB_InitODBElemDesp.inc'
INCLUDE 'OpsODB_InitODBElemDesp_Body.inc'

END MODULE OpsMod_ODBUtils
