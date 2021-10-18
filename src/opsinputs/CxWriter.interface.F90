!
! (C) Crown Copyright 2020, the Met Office. All rights reserved.
!
! Refer to COPYRIGHT.txt of this distribution for details. 
!
module opsinputs_cxwriter_mod_c

use fckit_configuration_module, only: fckit_configuration
use, intrinsic :: iso_c_binding, only: &
  c_double,                            &
  c_bool,                              &
  c_int,                               &
  c_ptr
use oops_variables_mod, only: oops_variables
use opsinputs_cxwriter_mod, only:  &
  opsinputs_cxwriter,              &
  opsinputs_cxwriter_create,       &
  opsinputs_cxwriter_delete,       &
  opsinputs_cxwriter_prior,        &
  opsinputs_cxwriter_post
use ufo_geovals_mod, only: ufo_geovals
use ufo_geovals_mod_c, only: ufo_geovals_registry
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

!> Creates an opsinputs_cxwriter object. Returns 1 if the creation succeeds and 0 if it fails.
function opsinputs_cxwriter_create_c(c_self, c_conf, c_varlist) &
  bind(c,name='opsinputs_cxwriter_create_f90')
implicit none
integer(c_int), intent(inout)  :: c_self
type(c_ptr), value, intent(in) :: c_conf
type(c_ptr), intent(in), value :: c_varlist ! list of geovals variables to be requested
integer(c_int)                 :: opsinputs_cxwriter_create_c

type(opsinputs_cxwriter), pointer :: self
type(fckit_configuration) :: f_conf
type(oops_variables) :: f_varlist

call opsinputs_cxwriter_registry%setup(c_self, self)

f_conf = fckit_configuration(c_conf)
f_varlist = oops_variables(c_varlist)
if (opsinputs_cxwriter_create(self, f_conf, f_varlist)) then
  opsinputs_cxwriter_create_c = 1
else
  opsinputs_cxwriter_create_c = 0
end if

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

subroutine opsinputs_cxwriter_post_c(c_self, c_obspace, c_flags, &
                                     c_nvars, c_nlocs, c_varnames, c_hofx) &
  bind(c,name='opsinputs_cxwriter_post_f90')
implicit none
integer(c_int), intent(in)     :: c_self
type(c_ptr), value, intent(in) :: c_obspace
type(c_ptr), value, intent(in) :: c_flags
integer(c_int), intent(in)     :: c_nvars, c_nlocs
type(c_ptr), value, intent(in) :: c_varnames
real(c_double), intent(in)     :: c_hofx(c_nvars, c_nlocs)

type(opsinputs_cxwriter), pointer :: self
type(oops_variables) :: f_varnames

call opsinputs_cxwriter_registry%get(c_self, self)

! Obtain simulated variable names.
f_varnames = oops_variables(c_varnames)

call opsinputs_cxwriter_post(self, c_obspace, c_flags, &
                             c_nvars, c_nlocs, f_varnames, c_hofx)

end subroutine opsinputs_cxwriter_post_c

! ------------------------------------------------------------------------------

end module opsinputs_cxwriter_mod_c
