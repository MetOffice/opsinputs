! (C) Copyright 2020 Met Office UK
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

!> Utilities used by both VarObsWriter and CxWriter.

module opsinputs_utils_mod

use missing_values_mod
use oops_variables_mod
use obsspace_mod
use ufo_vars_mod, only: MAXVARLEN
use opsinputs_obsdatavector_mod

use OpsMod_Kinds, only: &
    integer64,          &
    logical64,          &
    real64
use OpsMod_ObsInfo, only: FinalRejectReport

implicit none
private

public :: opsinputs_utils_fillreportflags

! Maximum length of a variable name
integer, parameter, public :: max_varname_length=MAXVARLEN
! Maximum length of a variable name with channel suffix
integer, parameter, public :: max_varname_with_channel_length=max_varname_length + 10

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

!> Populate the ReportFlags field of OB_type.
!>
!> Observations are marked as rejected if the JEDI QC flags of any or all (depending on the
!> specified options) simulated variables are set to anything different from "pass".
subroutine opsinputs_utils_fillreportflags( &
  ObsSpace, Flags, RejectObsWithAnyVariableFailingQC, RejectObsWithAllVariablesFailingQC, &
  ReportFlags)
use, intrinsic :: iso_c_binding
implicit none

! Subroutine arguments:
type(c_ptr), value, intent(in)           :: ObsSpace, Flags
logical, intent(in)                      :: RejectObsWithAnyVariableFailingQC
logical, intent(in)                      :: RejectObsWithAllVariablesFailingQC
integer(kind=integer64), intent(out)     :: ReportFlags(:)

! Local declarations:
type(oops_variables)                     :: ObsVariables
character(max_varname_length)            :: VarName
integer                                  :: NumObsVariables, iVar
integer(c_int)                           :: VarFlags(size(ReportFlags))

! Body:

ObsVariables = opsinputs_obsdatavector_int_varnames(Flags)
NumObsVariables = ObsVariables % nvars()

if (RejectObsWithAnyVariableFailingQC) then
  ReportFlags = 0

  ! Set the FinalRejectReport bit in ReportFlags for observations with a non-zero QC flag
  ! in at least one variable.
  do iVar = 1, NumObsVariables
    VarName = ObsVariables % variable(iVar)
    call opsinputs_obsdatavector_int_get(Flags, VarName, VarFlags)
    where (VarFlags > 0)
      ReportFlags = ibset(ReportFlags, FinalRejectReport)
    end where
  end do
else if (RejectObsWithAllVariablesFailingQC) then
  ReportFlags = ibset(ReportFlags, FinalRejectReport)

  ! Clear the FinalRejectReport bit in ReportFlags for observations with a zero QC flag
  ! in at least one variable.
  do iVar = 1, NumObsVariables
    VarName = ObsVariables % variable(iVar)
    call opsinputs_obsdatavector_int_get(Flags, VarName, VarFlags)
    where (VarFlags == 0)
      ReportFlags = ibclr(ReportFlags, FinalRejectReport)
    end where
  end do
end if

end subroutine opsinputs_utils_fillreportflags

end module opsinputs_utils_mod
