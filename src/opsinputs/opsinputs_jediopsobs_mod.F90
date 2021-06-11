! (C) Crown Copyright 2021, the Met Office. All rights reserved.
!
! Refer to COPYRIGHT.txt of this distribution for details.

!> The opsinputs_jediopsobs type (see below)

module opsinputs_jediopsobs_mod

use, intrinsic :: iso_c_binding, only: c_int32_t
use opsinputs_obsspace_mod, only: &
    opsinputs_obsspace_get_locs_ordered_by_record

use OpsMod_Kinds, only: integer64

implicit none
private

! ------------------------------------------------------------------------------

!> Holds the numbers of JEDI and OPS locations on the current PE and, if each JEDI record should be
!> converted to a single OPS observation, lists of locations belonging to each record.
type, public :: opsinputs_jediopsobs
  !> True to convert each JEDI record to an OPS ob
  logical              :: ConvertRecordsToMultilevelObs
  !> Number of locations in the ObsSpace
  integer              :: NumJediObs
  !> Number of OPS observations
  integer(integer64)   :: NumOpsObs

  ! The following fields are filled only if ConvertRecordsToMultilevelObs is .TRUE.

  !> Number of locations in the longest record
  integer(c_int32_t)   :: MaxNumLevelsPerObs
  !> ObsSpace loc indices ordered by record, then any sorting variables
  integer(c_int32_t), allocatable :: LocationsOrderedByRecord(:)
  !> Index of the first element of LocationsOrderedByRecord from each record
  integer(c_int32_t), allocatable :: RecordStarts(:)
end type opsinputs_jediopsobs

! ------------------------------------------------------------------------------

public :: opsinputs_jediopsobs_create, &
          opsinputs_jediopsobs_clear_rejected_records

! ------------------------------------------------------------------------------
contains

! ------------------------------------------------------------------------------
!> Create an opsinputs_jediopsobs object
function opsinputs_jediopsobs_create(ObsSpace, ConvertRecordsToMultilevelObs) result(JediOpsObs)
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
type(opsinputs_jediopsobs)      :: JediOpsObs
integer                         :: nlocs, nrecs, i

! Body:

JediOpsObs % ConvertRecordsToMultilevelObs = ConvertRecordsToMultilevelObs

nlocs = obsspace_get_nlocs(ObsSpace)
JediOpsObs % NumJediObs = nlocs

if (JediOpsObs % ConvertRecordsToMultilevelObs) then
  nrecs = obsspace_get_nrecs(ObsSpace)
  allocate(JediOpsObs % LocationsOrderedByRecord(nlocs))
  allocate(JediOpsObs % RecordStarts(nrecs + 1))
  call opsinputs_obsspace_get_locs_ordered_by_record( &
    ObsSpace, JediOpsObs % LocationsOrderedByRecord, JediOpsObs % RecordStarts)

  JediOpsObs % NumOpsObs = nrecs

  JediOpsObs % MaxNumLevelsPerObs = 0
  do i = 1, nrecs
    JediOpsObs % MaxNumLevelsPerObs = max( &
      JediOpsObs % MaxNumLevelsPerObs, &
      JediOpsObs % RecordStarts(i + 1) - JediOpsObs % RecordStarts(i))
  end do
else
  JediOpsObs % NumOpsObs = nlocs
  JediOpsObs % MaxNumLevelsPerObs = 1
end if
end function opsinputs_jediopsobs_create

! ------------------------------------------------------------------------------
!> Remove all locations belonging to rejected records from JediOpsObs % LocationsOrderedByRecord and
!> adjust JediOpsObs % RecordStarts accordingly.
subroutine opsinputs_jediopsobs_clear_rejected_records(ReportFlags, JediOpsObs)
implicit none

! Subroutine arguments:
integer(integer64), intent(in)            :: ReportFlags(:)
type(opsinputs_jediopsobs), intent(inout) :: JediOpsObs

! Local declarations:
integer, allocatable :: NewLocationsOrderedByRecord(:)
integer              :: NewRecordStarts(JediOpsObs % NumOpsObs + 1)
integer              :: NewNumLocations, i, RecordLength

! Body:

NewNumLocations = 0
do i = 1, JediOpsObs % NumOpsObs
  if (ReportFlags(i) == 0) then
    NewNumLocations = NewNumLocations + &
      (JediOpsObs % RecordStarts(i + 1) - JediOpsObs % RecordStarts(i))
  end if
end do

allocate(NewLocationsOrderedByRecord(NewNumLocations))
NewRecordStarts(1) = 1
do i = 1, JediOpsObs % NumOpsObs
  if (ReportFlags(i) == 0) then
    ! Accepted record; copy over from JediOpsObs
    RecordLength = JediOpsObs % RecordStarts(i + 1) - JediOpsObs % RecordStarts(i)
    NewRecordStarts(i + 1) = NewRecordStarts(i) + RecordLength
    NewLocationsOrderedByRecord(NewRecordStarts(i) : &
                                NewRecordStarts(i + 1) - 1) = &
      JediOpsObs % LocationsOrderedByRecord(JediOpsObs % RecordStarts(i) : &
                                            JediOpsObs % RecordStarts(i + 1) - 1)
  else
    ! Rejected record; treat it as empty
    NewRecordStarts(i + 1) = NewRecordStarts(i)
  end if
end do

call move_alloc(NewLocationsOrderedByRecord, JediOpsObs % LocationsOrderedByRecord)
JediOpsObs % RecordStarts = NewRecordStarts

JediOpsObs % MaxNumLevelsPerObs = 0
do i = 1, JediOpsObs % NumOpsObs
  JediOpsObs % MaxNumLevelsPerObs = max( &
    JediOpsObs % MaxNumLevelsPerObs, &
    JediOpsObs % RecordStarts(i + 1) - JediOpsObs % RecordStarts(i))
end do

end subroutine opsinputs_jediopsobs_clear_rejected_records

end module opsinputs_jediopsobs_mod
