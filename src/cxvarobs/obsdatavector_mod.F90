!
! (C) Copyright 2020 Met Office UK
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

!>  Fortran interface to ObsDataVector.

module cxvarobs_obsdatavector_mod

use kinds
use string_f_c_mod

implicit none

public cxvarobs_obsdatavector_int_nlocs
public cxvarobs_obsdatavector_int_has
public cxvarobs_obsdatavector_int_get
public cxvarobs_obsdatavector_float_nlocs
public cxvarobs_obsdatavector_float_has
public cxvarobs_obsdatavector_float_get

#include "obsdatavector_interface.f90"

contains

!>  Return the number of observational locations in this ObsDataVector<int> object.

integer function cxvarobs_obsdatavector_int_nlocs(c_vec)
  use, intrinsic :: iso_c_binding
  implicit none
  type(c_ptr), intent(in) :: c_vec

  cxvarobs_obsdatavector_int_nlocs = c_cxvarobs_obsdatavector_int_nlocs(c_vec)
end function cxvarobs_obsdatavector_int_nlocs

!>  Return true if this ObsDataVector<int> object contains a given variable.

logical function cxvarobs_obsdatavector_int_has(c_vec, variable)
  use, intrinsic :: iso_c_binding
  implicit none
  type(c_ptr), intent(in)      :: c_vec
  character(len=*), intent(in) :: variable

  character(kind=c_char,len=1), allocatable :: c_variable(:)

  call f_c_string(variable, c_variable)
  cxvarobs_obsdatavector_int_has = c_cxvarobs_obsdatavector_int_has(c_vec, c_variable)
  deallocate(c_variable)
end function cxvarobs_obsdatavector_int_has

!> Get a variable from this ObsDataVector<int> object.

subroutine cxvarobs_obsdatavector_int_get(c_vec, variable, vect)
  use, intrinsic :: iso_c_binding
  implicit none
  type(c_ptr), value, intent(in) :: c_vec
  character(len=*), intent(in) :: variable
  integer(c_int), intent(inout) :: vect(:)

  character(kind=c_char,len=1), allocatable :: c_variable(:)
  integer(c_size_t) :: length

  !  Translate query from Fortran string to C++ char[].
  call f_c_string(variable, c_variable)
  length = size(vect)

  call c_cxvarobs_obsdatavector_int_get(c_vec, c_variable, length, vect)

  deallocate(c_variable)
end subroutine cxvarobs_obsdatavector_int_get


!>  Return the number of observational locations in this ObsDataVector<float> object.

integer function cxvarobs_obsdatavector_float_nlocs(c_vec)
  use, intrinsic :: iso_c_binding
  implicit none
  type(c_ptr), intent(in) :: c_vec

  cxvarobs_obsdatavector_float_nlocs = c_cxvarobs_obsdatavector_float_nlocs(c_vec)
end function cxvarobs_obsdatavector_float_nlocs

!>  Return true if this ObsDataVector<float> object contains a given variable.

logical function cxvarobs_obsdatavector_float_has(c_vec, variable)
  use, intrinsic :: iso_c_binding
  implicit none
  type(c_ptr), intent(in)      :: c_vec
  character(len=*), intent(in) :: variable

  character(kind=c_char,len=1), allocatable :: c_variable(:)

  call f_c_string(variable, c_variable)
  cxvarobs_obsdatavector_float_has = c_cxvarobs_obsdatavector_float_has(c_vec, c_variable)
  deallocate(c_variable)
end function cxvarobs_obsdatavector_float_has

!> Get a variable from this ObsDataVector<float> object.

subroutine cxvarobs_obsdatavector_float_get(c_vec, variable, vect)
  use, intrinsic :: iso_c_binding
  implicit none
  type(c_ptr), value, intent(in) :: c_vec
  character(len=*), intent(in) :: variable
  real(c_float), intent(inout) :: vect(:)

  character(kind=c_char,len=1), allocatable :: c_variable(:)
  integer(c_size_t) :: length

  !  Translate query from Fortran string to C++ char[].
  call f_c_string(variable, c_variable)
  length = size(vect)

  call c_cxvarobs_obsdatavector_float_get(c_vec, c_variable, length, vect)

  deallocate(c_variable)
end subroutine cxvarobs_obsdatavector_float_get

end module cxvarobs_obsdatavector_mod
