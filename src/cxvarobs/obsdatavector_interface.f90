!
! (C) Copyright 2020 Met Office UK
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

!> \file Interface for C++ ObsDataVector code called from Fortran

interface

integer(kind=c_int) function c_cxvarobs_obsdatavector_int_nlocs(vec) &
    bind(C,name='cxvarobs_obsdatavector_int_nlocs_f')
  use, intrinsic :: iso_c_binding
  implicit none

  type(c_ptr), value :: vec
end function c_cxvarobs_obsdatavector_int_nlocs

logical(kind=c_bool) function c_cxvarobs_obsdatavector_int_has(vec, variable) &
    bind(C,name='cxvarobs_obsdatavector_int_has_f')
  use, intrinsic :: iso_c_binding
  implicit none

  type(c_ptr), value                        :: vec
  character(kind=c_char, len=1), intent(in) :: variable(*)
end function c_cxvarobs_obsdatavector_int_has

subroutine c_cxvarobs_obsdatavector_int_get(vec, variable, length, data) &
    bind(C,name='cxvarobs_obsdatavector_int_get_f')
  use, intrinsic :: iso_c_binding, only : c_ptr,c_char,c_size_t,c_int
  implicit none
  type(c_ptr), value :: vec
  character(kind=c_char, len=1), intent(in) :: variable(*)
  integer(c_size_t), intent(in) :: length
  integer(c_int), intent(inout) :: data(length)
end subroutine c_cxvarobs_obsdatavector_int_get

integer(kind=c_int) function c_cxvarobs_obsdatavector_float_nlocs(vec) &
    bind(C,name='cxvarobs_obsdatavector_float_nlocs_f')
  use, intrinsic :: iso_c_binding
  implicit none

  type(c_ptr), value :: vec
end function c_cxvarobs_obsdatavector_float_nlocs

logical(kind=c_bool) function c_cxvarobs_obsdatavector_float_has(vec, variable) &
    bind(C,name='cxvarobs_obsdatavector_float_has_f')
  use, intrinsic :: iso_c_binding
  implicit none

  type(c_ptr), value                        :: vec
  character(kind=c_char, len=1), intent(in) :: variable(*)
end function c_cxvarobs_obsdatavector_float_has

subroutine c_cxvarobs_obsdatavector_float_get(vec, variable, length, data) &
    bind(C,name='cxvarobs_obsdatavector_float_get_f')
  use, intrinsic :: iso_c_binding, only : c_ptr,c_char,c_size_t,c_float
  implicit none
  type(c_ptr), value :: vec
  character(kind=c_char, len=1), intent(in) :: variable(*)
  integer(c_size_t), intent(in) :: length
  real(c_float), intent(inout) :: data(length)
end subroutine c_cxvarobs_obsdatavector_float_get

end interface
