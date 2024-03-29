!
! (C) Crown Copyright 2020, the Met Office. All rights reserved.
!

!>  Fortran interface to ObsDataVector.

module opsinputs_obsdatavector_mod

use oops_variables_mod, only: oops_variables
use string_f_c_mod, only: f_c_string
use, intrinsic :: iso_c_binding, only: c_char, c_int, c_float, c_ptr, c_size_t

implicit none

public opsinputs_obsdatavector_int_nlocs
public opsinputs_obsdatavector_int_has
public opsinputs_obsdatavector_int_get
public opsinputs_obsdatavector_int_varnames
public opsinputs_obsdatavector_float_nlocs
public opsinputs_obsdatavector_float_has
public opsinputs_obsdatavector_float_get
public opsinputs_obsdatavector_float_varnames
private

#include "opsinputs_obsdatavector_interface.f90"

contains

!> Return the number of observation locations in this ObsDataVector<int> object.

integer function opsinputs_obsdatavector_int_nlocs(c_vec)
  !use, intrinsic :: iso_c_binding, only: c_ptr
  implicit none
  type(c_ptr), intent(in) :: c_vec

  opsinputs_obsdatavector_int_nlocs = c_opsinputs_obsdatavector_int_nlocs(c_vec)
end function opsinputs_obsdatavector_int_nlocs

!> Return an object wrapping the list of names of variables held in this ObsDataVector<int> object.

type(oops_variables) function opsinputs_obsdatavector_int_varnames(c_vec)
  !use, intrinsic :: iso_c_binding, only: c_ptr
  !use oops_variables_mod
  implicit none
  type(c_ptr), value, intent(in) :: c_vec

  opsinputs_obsdatavector_int_varnames = oops_variables(c_opsinputs_obsdatavector_int_varnames(c_vec))
end function opsinputs_obsdatavector_int_varnames

!> Return true if this ObsDataVector<int> object contains a given variable.

logical function opsinputs_obsdatavector_int_has(c_vec, variable)
  !use, intrinsic :: iso_c_binding, only: c_ptr
  implicit none
  type(c_ptr), intent(in)      :: c_vec
  character(len=*), intent(in) :: variable

  character(kind=c_char,len=1), allocatable :: c_variable(:)

  call f_c_string(variable, c_variable)
  opsinputs_obsdatavector_int_has = c_opsinputs_obsdatavector_int_has(c_vec, c_variable)
end function opsinputs_obsdatavector_int_has

!> Get a variable from this ObsDataVector<int> object.

subroutine opsinputs_obsdatavector_int_get(c_vec, variable, vect)
  !use, intrinsic :: iso_c_binding, only: c_ptr, c_int
  implicit none
  type(c_ptr), value, intent(in) :: c_vec
  character(len=*), intent(in) :: variable
  integer(c_int), intent(inout) :: vect(:)

  character(kind=c_char,len=1), allocatable :: c_variable(:)
  integer(c_size_t) :: length

  !  Translate query from Fortran string to C++ char[].
  call f_c_string(variable, c_variable)
  length = size(vect)

  call c_opsinputs_obsdatavector_int_get(c_vec, c_variable, length, vect)
end subroutine opsinputs_obsdatavector_int_get


!> Return the number of observation locations in this ObsDataVector<float> object.

integer function opsinputs_obsdatavector_float_nlocs(c_vec)
  !use, intrinsic :: iso_c_binding
  implicit none
  type(c_ptr), intent(in) :: c_vec

  opsinputs_obsdatavector_float_nlocs = c_opsinputs_obsdatavector_float_nlocs(c_vec)
end function opsinputs_obsdatavector_float_nlocs

!> Return an object wrapping the list of names of variables held in this ObsDataVector<int> object.

type(oops_variables) function opsinputs_obsdatavector_float_varnames(c_vec)
  !use, intrinsic :: iso_c_binding
  use oops_variables_mod
  implicit none
  type(c_ptr), value, intent(in) :: c_vec

  opsinputs_obsdatavector_float_varnames = &
    oops_variables(c_opsinputs_obsdatavector_float_varnames(c_vec))
end function opsinputs_obsdatavector_float_varnames

!> Return true if this ObsDataVector<float> object contains a given variable.

logical function opsinputs_obsdatavector_float_has(c_vec, variable)
  !use, intrinsic :: iso_c_binding
  implicit none
  type(c_ptr), intent(in)      :: c_vec
  character(len=*), intent(in) :: variable

  character(kind=c_char,len=1), allocatable :: c_variable(:)

  call f_c_string(variable, c_variable)
  opsinputs_obsdatavector_float_has = c_opsinputs_obsdatavector_float_has(c_vec, c_variable)
end function opsinputs_obsdatavector_float_has

!> Get a variable from this ObsDataVector<float> object.

subroutine opsinputs_obsdatavector_float_get(c_vec, variable, vect)
  !use, intrinsic :: iso_c_binding
  implicit none
  type(c_ptr), value, intent(in) :: c_vec
  character(len=*), intent(in) :: variable
  real(c_float), intent(inout) :: vect(:)

  character(kind=c_char,len=1), allocatable :: c_variable(:)
  integer(c_size_t) :: length

  !  Translate query from Fortran string to C++ char[].
  call f_c_string(variable, c_variable)
  length = size(vect)

  call c_opsinputs_obsdatavector_float_get(c_vec, c_variable, length, vect)
end subroutine opsinputs_obsdatavector_float_get

end module opsinputs_obsdatavector_mod
