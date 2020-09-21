!
! (C) Crown Copyright 2020, the Met Office. All rights reserved.
!
! Refer to COPYRIGHT.txt of this distribution for details. 
!
module opsinputs_varobswriter_mod_c

use fckit_configuration_module, only: fckit_configuration
use, intrinsic :: iso_c_binding, only: &
  c_double,                            &
  c_int,                               &
  c_ptr,                               &
  c_size_t
use opsinputs_varobswriter_mod, only:  &
  opsinputs_varobswriter,              &
  opsinputs_varobswriter_create,       &
  opsinputs_varobswriter_delete,       &
  opsinputs_varobswriter_prior,        &
  opsinputs_varobswriter_post
use ufo_geovals_mod, only: ufo_geovals
use ufo_geovals_mod_c, only: ufo_geovals_registry
use mpl, only: gc_int_kind
implicit none
private

#define LISTED_TYPE opsinputs_varobswriter

!> Linked list interface - defines registry_t type
#include "oops/util/linkedList_i.f"

!> Global registry
type(registry_t) :: opsinputs_varobswriter_registry

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
!> Linked list implementation
#include "oops/util/linkedList_c.f"
! ------------------------------------------------------------------------------

!> Creates an opsinputs_varobswriter object. Returns 1 if the creation succeeds and 0 if it fails.
function opsinputs_varobswriter_create_c(c_self, c_conf, c_comm, c_varlist) &
  bind(c,name='opsinputs_varobswriter_create_f90')
use oops_variables_mod
implicit none
integer(c_int), intent(inout)  :: c_self
type(c_ptr), value, intent(in) :: c_conf
integer(c_size_t), intent(in)  :: c_comm ! MPI communicator to be used by OPS
type(c_ptr), intent(in), value :: c_varlist ! list of geovals variables to be requested
integer(c_int)                 :: opsinputs_varobswriter_create_c

type(opsinputs_varobswriter), pointer :: self
type(fckit_configuration) :: f_conf
integer(gc_int_kind) :: f_comm
type(oops_variables) :: f_varlist

call opsinputs_varobswriter_registry%setup(c_self, self)

f_conf = fckit_configuration(c_conf)
f_comm = c_comm
f_varlist = oops_variables(c_varlist)
if (opsinputs_varobswriter_create(self, f_conf, f_comm, f_varlist)) then
  opsinputs_varobswriter_create_c = 1
else
  opsinputs_varobswriter_create_c = 0
end if

end function opsinputs_varobswriter_create_c

! ------------------------------------------------------------------------------

subroutine opsinputs_varobswriter_delete_c(c_self) &
  bind(c,name='opsinputs_varobswriter_delete_f90')
implicit none
integer(c_int), intent(inout) :: c_self

type(opsinputs_varobswriter), pointer :: self

call opsinputs_varobswriter_registry%get(c_self, self)
call opsinputs_varobswriter_delete(self)
call opsinputs_varobswriter_registry%delete(c_self, self)

end subroutine opsinputs_varobswriter_delete_c

! ------------------------------------------------------------------------------

subroutine opsinputs_varobswriter_prior_c(c_self, c_obspace, c_geovals) &
  bind(c,name='opsinputs_varobswriter_prior_f90')
implicit none
integer(c_int), intent(in) :: c_self
type(c_ptr), value, intent(in) :: c_obspace
integer(c_int), intent(in) :: c_geovals

type(opsinputs_varobswriter), pointer :: self
type(ufo_geovals), pointer :: geovals

call opsinputs_varobswriter_registry%get(c_self, self)
call ufo_geovals_registry%get(c_geovals, geovals)

call opsinputs_varobswriter_prior(self, c_obspace, geovals)

end subroutine opsinputs_varobswriter_prior_c

! ------------------------------------------------------------------------------

subroutine opsinputs_varobswriter_post_c(c_self, c_obspace, c_nchannels, c_channels, &
                                        c_flags, c_obserrors, c_nvars, c_nlocs, c_hofx) &
  bind(c,name='opsinputs_varobswriter_post_f90')
implicit none
integer(c_int), intent(in) :: c_self
type(c_ptr), value, intent(in) :: c_obspace
integer(c_int), intent(in) :: c_nchannels
integer(c_int), intent(in) :: c_channels(c_nchannels)
type(c_ptr), value, intent(in) :: c_flags, c_obserrors
integer(c_int), intent(in) :: c_nvars, c_nlocs
real(c_double), intent(in) :: c_hofx(c_nvars, c_nlocs)

type(opsinputs_varobswriter), pointer :: self

call opsinputs_varobswriter_registry%get(c_self, self)

call opsinputs_varobswriter_post(self, c_obspace, c_nchannels, c_channels, &
                                c_flags, c_obserrors, c_nvars, c_nlocs, c_hofx)

end subroutine opsinputs_varobswriter_post_c

! ------------------------------------------------------------------------------

end module opsinputs_varobswriter_mod_c
