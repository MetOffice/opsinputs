!-------------------------------------------------------------------------------
! (C) Crown copyright Met Office. All rights reserved.
!     Refer to COPYRIGHT.txt of this distribution for details.
!-------------------------------------------------------------------------------
! Module to contain code specific to NetCDF interfacing
!-------------------------------------------------------------------------------

MODULE ops_netcdf_wrappers

USE netcdf, ONLY:                          &
  netcdf_nf90_byte => nf90_byte,           &
  netcdf_nf90_char => nf90_char,           &
  netcdf_nf90_double => nf90_double,       &
  netcdf_nf90_float => nf90_float,         &
  netcdf_nf90_int => nf90_int,             &
  netcdf_nf90_real => nf90_real,           &
  netcdf_nf90_short => nf90_short,         &
  netcdf_nf90_unlimited => nf90_unlimited

USE OpsMod_Kinds, ONLY: &
  integer32

IMPLICIT NONE

SAVE

INTEGER, PARAMETER          :: nc_int_kind = integer32

INTEGER, PARAMETER          :: nf90_byte = netcdf_nf90_byte
INTEGER, PARAMETER          :: nf90_char = netcdf_nf90_char
INTEGER, PARAMETER          :: nf90_double = netcdf_nf90_double
INTEGER, PARAMETER          :: nf90_float = netcdf_nf90_float
INTEGER, PARAMETER          :: nf90_int = netcdf_nf90_int
INTEGER, PARAMETER          :: nf90_real = netcdf_nf90_real
INTEGER, PARAMETER          :: nf90_short = netcdf_nf90_short
INTEGER, PARAMETER          :: nf90_unlimited = netcdf_nf90_unlimited

INTERFACE ops_netcdf_def_var
  MODULE PROCEDURE ops_netcdf_def_var_array
  MODULE PROCEDURE ops_netcdf_def_var_scalar
  MODULE PROCEDURE ops_netcdf_def_var_single
END INTERFACE ops_netcdf_def_var

INTERFACE ops_netcdf_get_att
  MODULE PROCEDURE ops_netcdf_get_att_int_scalar
  MODULE PROCEDURE ops_netcdf_get_att_real_scalar
  MODULE PROCEDURE ops_netcdf_get_att_char
END INTERFACE ops_netcdf_get_att

INTERFACE ops_netcdf_get_var
  MODULE PROCEDURE ops_netcdf_get_var_int_array1d
  MODULE PROCEDURE ops_netcdf_get_var_int_array2d
  MODULE PROCEDURE ops_netcdf_get_var_int_array3d
  MODULE PROCEDURE ops_netcdf_get_var_real
  MODULE PROCEDURE ops_netcdf_get_var_real_array
  MODULE PROCEDURE ops_netcdf_get_var_real_array2d
  MODULE PROCEDURE ops_netcdf_get_var_real_array3d
  MODULE PROCEDURE ops_netcdf_get_var_real_array4d
END INTERFACE ops_netcdf_get_var

INTERFACE ops_netcdf_put_att
  MODULE PROCEDURE ops_netcdf_put_att_char
  MODULE PROCEDURE ops_netcdf_put_att_int16_array
  MODULE PROCEDURE ops_netcdf_put_att_int16
  MODULE PROCEDURE ops_netcdf_put_att_int32_array
  MODULE PROCEDURE ops_netcdf_put_att_int32
  MODULE PROCEDURE ops_netcdf_put_att_int64_array
  MODULE PROCEDURE ops_netcdf_put_att_int64
  MODULE PROCEDURE ops_netcdf_put_att_real32_array
  MODULE PROCEDURE ops_netcdf_put_att_real32
  MODULE PROCEDURE ops_netcdf_put_att_real64_array
  MODULE PROCEDURE ops_netcdf_put_att_real64
END INTERFACE ops_netcdf_put_att

INTERFACE ops_netcdf_put_var
  MODULE PROCEDURE ops_netcdf_put_var_char
  MODULE PROCEDURE ops_netcdf_put_var_char_array
  MODULE PROCEDURE ops_netcdf_put_var_char_array2d
  MODULE PROCEDURE ops_netcdf_put_var_char_array3d
  MODULE PROCEDURE ops_netcdf_put_var_int
  MODULE PROCEDURE ops_netcdf_put_var_int_array
  MODULE PROCEDURE ops_netcdf_put_var_int_array2d
  MODULE PROCEDURE ops_netcdf_put_var_int_array3d
  MODULE PROCEDURE ops_netcdf_put_var_real
  MODULE PROCEDURE ops_netcdf_put_var_real_array
  MODULE PROCEDURE ops_netcdf_put_var_real_array2d
  MODULE PROCEDURE ops_netcdf_put_var_real_array3d
END INTERFACE ops_netcdf_put_var

CONTAINS

INCLUDE 'ops_netcdf_close.inc'
INCLUDE 'ops_netcdf_create.inc'
INCLUDE 'ops_netcdf_def_dim.inc'
INCLUDE 'ops_netcdf_def_var_array.inc'
INCLUDE 'ops_netcdf_def_var_scalar.inc'
INCLUDE 'ops_netcdf_def_var_single.inc'
INCLUDE 'ops_netcdf_enddef.inc'
INCLUDE 'ops_netcdf_get_att_int_scalar.inc'
INCLUDE 'ops_netcdf_get_att_real_scalar.inc'
INCLUDE 'ops_netcdf_get_att_char.inc'
INCLUDE 'ops_netcdf_get_var_int_array1d.inc'
INCLUDE 'ops_netcdf_get_var_int_array2d.inc'
INCLUDE 'ops_netcdf_get_var_int_array3d.inc'
INCLUDE 'ops_netcdf_get_var_real.inc'
INCLUDE 'ops_netcdf_get_var_real_array.inc'
INCLUDE 'ops_netcdf_get_var_real_array2d.inc'
INCLUDE 'ops_netcdf_get_var_real_array3d.inc'
INCLUDE 'ops_netcdf_get_var_real_array4d.inc'
INCLUDE 'ops_netcdf_inq_dim_len.inc'
INCLUDE 'ops_netcdf_inq_libvers.inc'
INCLUDE 'ops_netcdf_inq_varid.inc'
INCLUDE 'ops_netcdf_inquire_attname.inc'
INCLUDE 'ops_netcdf_inquire_attribute.inc'
INCLUDE 'ops_netcdf_inquire_dimension.inc'
INCLUDE 'ops_netcdf_inquire_variable.inc'
INCLUDE 'ops_netcdf_open.inc'
INCLUDE 'ops_netcdf_put_att_char.inc'
INCLUDE 'ops_netcdf_put_att_int16_array.inc'
INCLUDE 'ops_netcdf_put_att_int16.inc'
INCLUDE 'ops_netcdf_put_att_int32_array.inc'
INCLUDE 'ops_netcdf_put_att_int32.inc'
INCLUDE 'ops_netcdf_put_att_int64_array.inc'
INCLUDE 'ops_netcdf_put_att_int64.inc'
INCLUDE 'ops_netcdf_put_att_real32_array.inc'
INCLUDE 'ops_netcdf_put_att_real32.inc'
INCLUDE 'ops_netcdf_put_att_real64_array.inc'
INCLUDE 'ops_netcdf_put_att_real64.inc'
INCLUDE 'ops_netcdf_put_var_char.inc'
INCLUDE 'ops_netcdf_put_var_char_array.inc'
INCLUDE 'ops_netcdf_put_var_char_array2d.inc'
INCLUDE 'ops_netcdf_put_var_char_array3d.inc'
INCLUDE 'ops_netcdf_put_var_int.inc'
INCLUDE 'ops_netcdf_put_var_int_array.inc'
INCLUDE 'ops_netcdf_put_var_int_array2d.inc'
INCLUDE 'ops_netcdf_put_var_int_array3d.inc'
INCLUDE 'ops_netcdf_put_var_real.inc'
INCLUDE 'ops_netcdf_put_var_real_array.inc'
INCLUDE 'ops_netcdf_put_var_real_array2d.inc'
INCLUDE 'ops_netcdf_put_var_real_array3d.inc'
INCLUDE 'ops_netcdf_redef.inc'

END MODULE ops_netcdf_wrappers
