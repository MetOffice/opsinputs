! (C) Crown Copyright 2020, the Met Office. All rights reserved.
!
! Refer to COPYRIGHT.txt of this distribution for details.

!> Utilities used by both VarObsWriter and CxWriter.

module opsinputs_utils_mod

use, intrinsic :: iso_c_binding, only: c_int32_t
use oops_variables_mod, only: oops_variables
use ufo_vars_mod, only: MAXVARLEN
use opsinputs_obsdatavector_mod, only:    &
    opsinputs_obsdatavector_int_varnames, &
    opsinputs_obsdatavector_int_get
use opsinputs_jeditoopslayoutmapping_mod, only: opsinputs_jeditoopslayoutmapping

use OpsMod_Kinds, only: integer64
use OpsMod_ObsInfo, only: FinalRejectReport

implicit none
private

! ------------------------------------------------------------------------------

public :: opsinputs_utils_fillreportflags

! ------------------------------------------------------------------------------
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
  JediToOpsLayoutMapping, ObsSpace, Flags, &
  RejectObsWithAnyVariableFailingQC, RejectObsWithAllVariablesFailingQC, ReportFlags)
use, intrinsic :: iso_c_binding, only: c_int, c_ptr
implicit none

! Subroutine arguments:
type(opsinputs_jeditoopslayoutmapping), intent(in)       :: JediToOpsLayoutMapping
type(c_ptr), value, intent(in)               :: ObsSpace, Flags
logical, intent(in)                          :: RejectObsWithAnyVariableFailingQC
logical, intent(in)                          :: RejectObsWithAllVariablesFailingQC
integer(kind=integer64), intent(out)         :: ReportFlags(:)

! Local declarations:
type(oops_variables)                         :: ObsVariables
character(max_varname_length)                :: VarName
integer                                      :: NumObsVariables
integer                                      :: iVar, iOpsObs, iJediObsInRecord, iJediObs
integer                                      :: NumJediObsInRecord
integer(c_int)                               :: VarFlags(JediToOpsLayoutMapping % NumJediObs)
logical                                      :: AllJediObsRejected

! Body:

ObsVariables = opsinputs_obsdatavector_int_varnames(Flags)
NumObsVariables = ObsVariables % nvars()

! Initialise ReportFlags
if (RejectObsWithAnyVariableFailingQC) then
  ReportFlags = 0  
else if (RejectObsWithAllVariablesFailingQC) then
  ReportFlags = ibset(ReportFlags, FinalRejectReport)
end if

! Iterate over variables
do iVar = 1, NumObsVariables
  VarName = ObsVariables % variable(iVar)
  call opsinputs_obsdatavector_int_get(Flags, VarName, VarFlags)
  ! Iterate over (OPS) observations
  do iOpsObs = 1, JediToOpsLayoutMapping % NumOpsObs
    if (JediToOpsLayoutMapping % ConvertRecordsToMultilevelObs) then
      ! Check if all JEDI observations in this record are rejected
      AllJediObsRejected = .true.
      NumJediObsInRecord = JediToOpsLayoutMapping % RecordStarts(iOpsObs + 1) - &
                           JediToOpsLayoutMapping % RecordStarts(iOpsObs)
      do iJediObsInRecord = 1, NumJediObsInRecord
        iJediObs = JediToOpsLayoutMapping % LocationsOrderedByRecord( &
          JediToOpsLayoutMapping % RecordStarts(iOpsObs) + (iJediObsInRecord - 1))
        if (VarFlags(iJediObs) == 0) then
          AllJediObsRejected = .false.
          exit
        end if
      end do
    else
      AllJediObsRejected = (VarFlags(iOpsObs) > 0)
    end if

    if (RejectObsWithAnyVariableFailingQC) then
      ! Set the FinalRejectReport bit in ReportFlags for observations with a non-zero QC flag
      ! in at least one variable.
      if (AllJediObsRejected) then
        ReportFlags(iOpsObs) = ibset(ReportFlags(iOpsObs), FinalRejectReport)
      end if
    else if (RejectObsWithAllVariablesFailingQC) then
      ! Clear the FinalRejectReport bit in ReportFlags for observations with a zero QC flag
      ! in at least one variable.
      if (.not. AllJediObsRejected) then
        ReportFlags(iOpsObs) = ibclr(ReportFlags(iOpsObs), FinalRejectReport)
      end if
    end if
  end do
end do

end subroutine opsinputs_utils_fillreportflags

end module opsinputs_utils_mod
