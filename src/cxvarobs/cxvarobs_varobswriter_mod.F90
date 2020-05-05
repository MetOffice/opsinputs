! (C) Copyright 2020 Met Office UK
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

!> Fortran module to implement varobswriter

module cxvarobs_varobswriter_mod

use fckit_configuration_module, only: fckit_configuration
use iso_c_binding
use kinds
use ufo_geovals_mod
use obsspace_mod
use ufo_vars_mod

use opsmod_obsinfo

implicit none
public :: cxvarobs_varobswriter_create, cxvarobs_varobswriter_delete, cxvarobs_varobswriter_prior, cxvarobs_varobswriter_post
private
integer, parameter :: max_string=800

! ------------------------------------------------------------------------------
!> TODO: fill in this type
type, public :: cxvarobs_varobswriter
private
  character(len=max_string), public, allocatable :: geovars(:)
end type cxvarobs_varobswriter

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_create(self, f_conf)
implicit none
type(cxvarobs_varobswriter), intent(inout)      :: self
type(fckit_configuration), intent(in) :: f_conf

! TODO: set self%geovars (list of variables to use from GeoVaLs) if needed

type(OB_type)                  :: obs

print *, 'Calling Ops_InitObs in constructor'
call Ops_InitObs(obs)
print *, 'Called Ops_InitObs'

end subroutine cxvarobs_varobswriter_create

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_delete(self)
implicit none
type(cxvarobs_varobswriter), intent(inout) :: self

if (allocated(self%geovars))   deallocate(self%geovars)

end subroutine cxvarobs_varobswriter_delete

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_prior(self, obspace, geovals)
implicit none
type(cxvarobs_varobswriter),  intent(in) :: self
type(c_ptr), value, intent(in) :: obspace
type(ufo_geovals),  intent(in) :: geovals

end subroutine cxvarobs_varobswriter_prior

! ------------------------------------------------------------------------------

subroutine cxvarobs_varobswriter_post(self, obspace, nvars, nlocs, hofx)
implicit none
type(cxvarobs_varobswriter),  intent(in) :: self
type(c_ptr), value, intent(in) :: obspace
integer,            intent(in) :: nvars, nlocs
real(c_double),     intent(in) :: hofx(nvars, nlocs)

type(OB_type)                  :: obs

print *, 'Calling Ops_InitObs in post'
call Ops_InitObs(obs)
print *, 'Called Ops_InitObs'

end subroutine cxvarobs_varobswriter_post

! ------------------------------------------------------------------------------

end module cxvarobs_varobswriter_mod
