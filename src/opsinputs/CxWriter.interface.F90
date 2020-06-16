!
! (C) Copyright 2020 Met Office UK
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
!
module opsinputs_cxwriter_mod_c

use iso_c_binding
use opsinputs_cxwriter_mod
use string_f_c_mod
use ufo_geovals_mod
use ufo_geovals_mod_c,   only: ufo_geovals_registry
use fckit_configuration_module, only: fckit_configuration 
implicit none
private

#define LISTED_TYPE opsinputs_cxwriter

!> Linked list interface - defines registry_t type
#include "oops/util/linkedList_i.f"

!> Global registry
type(registry_t) :: opsinputs_cxwriter_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
!> Linked list implementation
#include "oops/util/linkedList_c.f"
! ------------------------------------------------------------------------------

function opsinputs_cxwriter_create_c(c_self, c_conf, c_varlist) &
  bind(c,name='opsinputs_cxwriter_create_f90')
use oops_variables_mod
implicit none
integer(c_int), intent(inout)  :: c_self
type(c_ptr), value, intent(in) :: c_conf
type(c_ptr), intent(in), value :: c_varlist ! list of geovals variables to be requested
logical(c_bool)                :: opsinputs_cxwriter_create_c

type(opsinputs_cxwriter), pointer :: self
type(fckit_configuration) :: f_conf
type(oops_variables) :: f_varlist

call opsinputs_cxwriter_registry%setup(c_self, self)

f_conf = fckit_configuration(c_conf)
f_varlist = oops_variables(c_varlist)
opsinputs_cxwriter_create_c = opsinputs_cxwriter_create(self, f_conf, f_varlist)

end function opsinputs_cxwriter_create_c

! ------------------------------------------------------------------------------

subroutine opsinputs_cxwriter_delete_c(c_self) &
  bind(c,name='opsinputs_cxwriter_delete_f90')
implicit none
integer(c_int), intent(inout) :: c_self

type(opsinputs_cxwriter), pointer :: self

call opsinputs_cxwriter_registry%get(c_self, self)
call opsinputs_cxwriter_delete(self)
call opsinputs_cxwriter_registry%delete(c_self, self)

end subroutine opsinputs_cxwriter_delete_c

! ------------------------------------------------------------------------------

subroutine opsinputs_cxwriter_prior_c(c_self, c_obspace, c_geovals) &
  bind(c,name='opsinputs_cxwriter_prior_f90')
implicit none
integer(c_int), intent(in) :: c_self
type(c_ptr), value, intent(in) :: c_obspace
integer(c_int), intent(in) :: c_geovals

type(opsinputs_cxwriter), pointer :: self
type(ufo_geovals), pointer :: geovals

call opsinputs_cxwriter_registry%get(c_self, self)
call ufo_geovals_registry%get(c_geovals, geovals)

call opsinputs_cxwriter_prior(self, c_obspace, geovals)

end subroutine opsinputs_cxwriter_prior_c

! ------------------------------------------------------------------------------

subroutine opsinputs_cxwriter_post_c(c_self, c_obspace, c_nchannels, c_channels, &
                                        c_flags, c_obserrors, c_nvars, c_nlocs, c_hofx) &
  bind(c,name='opsinputs_cxwriter_post_f90')
implicit none
integer(c_int), intent(in) :: c_self
type(c_ptr), value, intent(in) :: c_obspace
integer(c_int), intent(in) :: c_nchannels
integer(c_int), intent(in) :: c_channels(c_nchannels)
type(c_ptr), value, intent(in) :: c_flags, c_obserrors
integer(c_int), intent(in) :: c_nvars, c_nlocs
real(c_double), intent(in) :: c_hofx(c_nvars, c_nlocs)

type(opsinputs_cxwriter), pointer :: self

call opsinputs_cxwriter_registry%get(c_self, self)

call opsinputs_cxwriter_post(self, c_obspace, c_nchannels, c_channels, &
                                c_flags, c_obserrors, c_nvars, c_nlocs, c_hofx)

end subroutine opsinputs_cxwriter_post_c

! ------------------------------------------------------------------------------

end module opsinputs_cxwriter_mod_c
