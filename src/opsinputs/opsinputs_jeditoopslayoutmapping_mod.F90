! (C) Crown Copyright 2021, the Met Office. All rights reserved.
!
! Refer to COPYRIGHT.txt of this distribution for details.

!> The opsinputs_jeditoopslayoutmapping type (see below)

module opsinputs_jeditoopslayoutmapping_mod

use, intrinsic :: iso_c_binding, only: c_int32_t
use opsinputs_obsspace_mod, only: &
    opsinputs_obsspace_get_locs_ordered_by_record

use OpsMod_Kinds, only: integer64

implicit none
private

! ------------------------------------------------------------------------------

!> Holds the numbers of JEDI locations and OPS observations stored on the current PE. If each JEDI
!> record (rather than each JEDI location) needs to be converted to a single OPS observation, holds
!> also lists of locations belonging to each record.
type, public :: opsinputs_jeditoopslayoutmapping
  !> If true, each JEDI record will be converted to an OPS observation (with multiple levels).
  !> Otherwise each JEDI location will be converted to an OPS observation.
  logical              :: ConvertRecordsToMultilevelObs
  !> Number of locations in the ObsSpace
  integer              :: NumJediObs
  !> Number of observations in the OB_type data structure
  integer(integer64)   :: NumOpsObs

  ! The following fields are filled only if ConvertRecordsToMultilevelObs is .TRUE.

  !> Number of locations in the longest JEDI record. When each record is converted to an OPS
  !> observation, each JEDI location will be mapped to a separate OPS level.
  integer(c_int32_t)   :: MaxNumLevelsPerObs
  !> ObsSpace loc indices ordered by record, then any sorting variables
  integer(c_int32_t), allocatable :: LocationsOrderedByRecord(:)
  !> Index of the first element of LocationsOrderedByRecord from each record
  integer(c_int32_t), allocatable :: RecordStarts(:)
end type opsinputs_jeditoopslayoutmapping

! ------------------------------------------------------------------------------

public :: opsinputs_jeditoopslayoutmapping_create, &
          opsinputs_jeditoopslayoutmapping_clear_rejected_records

! ------------------------------------------------------------------------------
contains

! ------------------------------------------------------------------------------
!> Create an instance of opsinputs_jeditoopslayoutmapping
function opsinputs_jeditoopslayoutmapping_create(ObsSpace, ConvertRecordsToMultilevelObs) result(JediToOpsLayoutMapping)
use obsspace_mod, only:  &
    obsspace_get_nlocs, &
    obsspace_get_nrecs
use, intrinsic :: iso_c_binding, only: &
    c_ptr
implicit none

! Function arguments:
type(c_ptr), value, intent(in)  :: ObsSpace
logical, intent(in)             :: ConvertRecordsToMultilevelObs

! Local declarations:
type(opsinputs_jeditoopslayoutmapping)      :: JediToOpsLayoutMapping
integer                         :: nlocs, nrecs, i

! Body:

JediToOpsLayoutMapping % ConvertRecordsToMultilevelObs = ConvertRecordsToMultilevelObs

nlocs = obsspace_get_nlocs(ObsSpace)
JediToOpsLayoutMapping % NumJediObs = nlocs

if (JediToOpsLayoutMapping % ConvertRecordsToMultilevelObs) then
  nrecs = obsspace_get_nrecs(ObsSpace)
  allocate(JediToOpsLayoutMapping % LocationsOrderedByRecord(nlocs))
  allocate(JediToOpsLayoutMapping % RecordStarts(nrecs + 1))
  call opsinputs_obsspace_get_locs_ordered_by_record( &
    ObsSpace, JediToOpsLayoutMapping % LocationsOrderedByRecord, JediToOpsLayoutMapping % RecordStarts)

  JediToOpsLayoutMapping % NumOpsObs = nrecs

  JediToOpsLayoutMapping % MaxNumLevelsPerObs = 0
  do i = 1, nrecs
    JediToOpsLayoutMapping % MaxNumLevelsPerObs = max( &
      JediToOpsLayoutMapping % MaxNumLevelsPerObs, &
      JediToOpsLayoutMapping % RecordStarts(i + 1) - JediToOpsLayoutMapping % RecordStarts(i))
  end do
else
  JediToOpsLayoutMapping % NumOpsObs = nlocs
  JediToOpsLayoutMapping % MaxNumLevelsPerObs = 1
end if
end function opsinputs_jeditoopslayoutmapping_create

! ------------------------------------------------------------------------------
!> Remove all locations belonging to rejected records from JediToOpsLayoutMapping % LocationsOrderedByRecord and
!> adjust JediToOpsLayoutMapping % RecordStarts accordingly.
subroutine opsinputs_jeditoopslayoutmapping_clear_rejected_records(ReportFlags, JediToOpsLayoutMapping)
implicit none

! Subroutine arguments:
integer(integer64), intent(in)            :: ReportFlags(:)
type(opsinputs_jeditoopslayoutmapping), intent(inout) :: JediToOpsLayoutMapping

! Local declarations:
integer, allocatable :: NewLocationsOrderedByRecord(:)
integer              :: NewRecordStarts(JediToOpsLayoutMapping % NumOpsObs + 1)
integer              :: NewNumLocations, i, RecordLength

! Body:

NewNumLocations = 0
do i = 1, JediToOpsLayoutMapping % NumOpsObs
  if (ReportFlags(i) == 0) then
    NewNumLocations = NewNumLocations + &
      (JediToOpsLayoutMapping % RecordStarts(i + 1) - JediToOpsLayoutMapping % RecordStarts(i))
  end if
end do

allocate(NewLocationsOrderedByRecord(NewNumLocations))
NewRecordStarts(1) = 1
do i = 1, JediToOpsLayoutMapping % NumOpsObs
  if (ReportFlags(i) == 0) then
    ! Accepted record; copy over from JediToOpsLayoutMapping
    RecordLength = JediToOpsLayoutMapping % RecordStarts(i + 1) - JediToOpsLayoutMapping % RecordStarts(i)
    NewRecordStarts(i + 1) = NewRecordStarts(i) + RecordLength
    NewLocationsOrderedByRecord(NewRecordStarts(i) : &
                                NewRecordStarts(i + 1) - 1) = &
      JediToOpsLayoutMapping % LocationsOrderedByRecord(JediToOpsLayoutMapping % RecordStarts(i) : &
                                            JediToOpsLayoutMapping % RecordStarts(i + 1) - 1)
  else
    ! Rejected record; treat it as empty
    NewRecordStarts(i + 1) = NewRecordStarts(i)
  end if
end do

call move_alloc(NewLocationsOrderedByRecord, JediToOpsLayoutMapping % LocationsOrderedByRecord)
JediToOpsLayoutMapping % RecordStarts = NewRecordStarts

JediToOpsLayoutMapping % MaxNumLevelsPerObs = 0
do i = 1, JediToOpsLayoutMapping % NumOpsObs
  JediToOpsLayoutMapping % MaxNumLevelsPerObs = max( &
    JediToOpsLayoutMapping % MaxNumLevelsPerObs, &
    JediToOpsLayoutMapping % RecordStarts(i + 1) - JediToOpsLayoutMapping % RecordStarts(i))
end do

end subroutine opsinputs_jeditoopslayoutmapping_clear_rejected_records

end module opsinputs_jeditoopslayoutmapping_mod
