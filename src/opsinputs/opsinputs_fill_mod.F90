! (C) Crown Copyright 2020, the Met Office. All rights reserved.
!
! Refer to COPYRIGHT.txt of this distribution for details.

!> Subroutines filling arrays belonging to OB_type or CX_type.

module opsinputs_fill_mod

use, intrinsic :: iso_c_binding, only: &
    c_double,                          &
    c_float,                           &
    c_int,                             &
    c_int32_t,                         &
    c_int64_t,                         &
    c_ptr
use kinds, only: kind_real
use missing_values_mod, only: missing_value
use obsspace_mod, only: &
    obsspace_has,         &
    obsspace_get_db
use oops_variables_mod, only: oops_variables
use ufo_geovals_mod, only: &
    ufo_geoval,            &
    ufo_geovals,           &
    ufo_geovals_get_var
use ufo_vars_mod, only: ufo_vars_getindex
use opsinputs_obsdatavector_mod, only:    &
    opsinputs_obsdatavector_int_has,      &
    opsinputs_obsdatavector_int_get,      &
    opsinputs_obsdatavector_float_has,    &
    opsinputs_obsdatavector_float_get
use opsinputs_obsspace_mod, only:                        &
    opsinputs_obsspace_get_db_string,                    &
    opsinputs_obsspace_get_db_datetime_offset_in_seconds
use opsinputs_jeditoopslayoutmapping_mod, only: &
    opsinputs_jeditoopslayoutmapping
use opsinputs_utils_mod, only: &
    max_varname_with_channel_length

use GenMod_Core, only: &
    gen_warn,          &
    gen_fail

use OpsMod_Constants, only: &
    PGEMDI,                 & ! missing value indicator for PGEs
    PPF                       ! PGE packing factor
use OpsMod_Kinds, only: &
    integer64,          &
    logical64,          &
    real64
use OpsMod_MiscTypes, only: &
    Coord_Type,             &
    Element_Type,           &
    ElementHeader_Type
use OpsMod_ObsInfo, only: &
    FinalRejectFlag,      &
    Ops_Alloc

implicit none
public :: opsinputs_fill_fillcoord2d, &
          opsinputs_fill_fillelementtypefromnormalvariable, &
          opsinputs_fill_fillelementtype2dfromnormalvariable, &
          opsinputs_fill_fillelementtypefromsimulatedvariable, &
          opsinputs_fill_fillelementtype2dfromsimulatedvariable, &
          opsinputs_fill_fillinteger, &
          opsinputs_fill_fillreal, &
          opsinputs_fill_fillreal2d, &
          opsinputs_fill_fillrealfromgeoval, &
          opsinputs_fill_fillreal2dfromgeoval, &
          opsinputs_fill_fillreal2dfromgeovalorhofx, &
          opsinputs_fill_fillreal2dfrom1dgeovalwithchans, &
          opsinputs_fill_fillreal2dfromhofx, &
          opsinputs_fill_fillstring, &
          opsinputs_fill_filltimeoffsets, &
          opsinputs_fill_filltimeoffsets2d, &
          opsinputs_fill_varnames_with_channels
private

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------

!> Populate a 1D array of Element_type objects and its header from a set of variables whose name
!> is included in the list passed to the 'simulate' option of the JEDI ObsSpace.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p El1 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] El1
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace containing the variables used to populate \p El1 and \p Hdr.
!> \param[in] Flags
!>   Pointer to a ioda::ObsDataVector<int> object containing QC flags.
!> \param[in] ObsErrors
!>   Pointer to a ioda::ObsDataVector<float> object containing observation errors.
!> \param[in] JediVarName
!>   Name of the JEDI variables (in the ObsValue, ObsError and GrossErrorProbability groups)
!>   used to populate \p El1 and \p Hdr.
!> \param[in] PackPGEs
!>   Optional; true by default. If set to false, PGEs won't be stored in packed form.
!>   The Ops_VarobPGEs subroutine expects PGEs to be stored in packed form for most varobs fields,
!>   but not all; the exceptions are mostly GNSSRO-related.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillelementtypefromsimulatedvariable( &
  Hdr, OpsVarName, NumObs, El1, ObsSpace, Flags, ObsErrors, JediVarName, PackPGEs)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(integer64), intent(in)                  :: NumObs
type(Element_type), pointer                     :: El1(:)
type(c_ptr), value, intent(in)                  :: ObsSpace
type(c_ptr), value, intent(in)                  :: Flags
type(c_ptr), value, intent(in)                  :: ObsErrors
character(len=*), intent(in)                    :: JediVarName
logical, optional, intent(in)                   :: PackPGEs

! Local declarations:
logical                                         :: DoPackPGEs
real(kind=c_double)                             :: ObsValue(NumObs)
integer(kind=c_int)                             :: Flag(NumObs)
real(kind=c_float)                              :: ObsError(NumObs)
real(kind=c_double)                             :: PGE(NumObs)
real(kind=c_double)                             :: MissingDouble
real(kind=c_float)                              :: MissingFloat
integer                                         :: i
character(len=*), parameter                     :: &
  RoutineName = "opsinputs_fill_fillelementtypefromsimulatedvariable"
character(len=256)                              :: ErrorMessage

! Body:

if (present(PackPGEs)) then
  DoPackPGEs = PackPGEs
else
  DoPackPGEs = .true.
end if

! The types of floating-point numbers used in this function are a bit confusing. OPS stores
! observation values as doubles, whereas JEDI stores them as floats. However, the Fortran interface
! to the IODA ObsSpace is only partially implemented: obsspace_get_db_real32 doesn't work, only
! obsspace_get_db_real64 does. So we need to retrieve observation values as doubles. Observation
! errors, though, are retrieved as floats.

MissingDouble = missing_value(0.0_c_double)
MissingFloat  = missing_value(0.0_c_float)

if (obsspace_has(ObsSpace, "ObsValue", JediVarName)) then
  ! Retrieve data from JEDI:
  ! - observation value
  call obsspace_get_db(ObsSpace, "ObsValue", JediVarName, ObsValue)
  ! - QC flag
  if (opsinputs_obsdatavector_int_has(Flags, JediVarName)) then
    call opsinputs_obsdatavector_int_get(Flags, JediVarName, Flag)
  else
    write (ErrorMessage, '(A,A)') "QC flags not found for variable ", JediVarName
    call gen_warn(RoutineName, ErrorMessage)
    Flag(:) = 0 ! assume all observations passed QC
  end if
  ! - observation error
  if (opsinputs_obsdatavector_float_has(ObsErrors, JediVarName)) then
    call opsinputs_obsdatavector_float_get(ObsErrors, JediVarName, ObsError)
  else
    write (ErrorMessage, '(A,A,A)') "Variable ", JediVarName, "@ObsError not found"
    call gen_warn(RoutineName, ErrorMessage)
    ObsError(:) = MissingFloat
  end if
  ! - gross error probability
  if (obsspace_has(ObsSpace, "GrossErrorProbability", JediVarName)) then
    call obsspace_get_db(ObsSpace, "GrossErrorProbability", JediVarName, PGE)
  else
    PGE(:) = MissingDouble
  end if

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, El1)
  do i = 1, NumObs
    if (ObsValue(i) /= MissingDouble) El1(i) % Value = ObsValue(i)
    if (ObsError(i) /= MissingFloat)  El1(i) % OBErr = ObsError(i)
    if (Flag(i) /= 0)                 El1(i) % Flags = ibset(0, FinalRejectFlag)
    call opsinputs_fill_setpgefinal(PGE(i), MissingDouble, DoPackPGEs, El1(i))
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine opsinputs_fill_fillelementtypefromsimulatedvariable

! ------------------------------------------------------------------------------

!> Populate a 2D array of Element_type objects and its header from a set of variables whose name
!> is included in the list passed to the 'simulate' option of the JEDI ObsSpace.
!>
!> This is the implementation used when each OPS observation should correspond to a single JEDI
!> location (possibly with multiple channels). There's another implementation (below) used when each
!> OPS observation should correspond to a single JEDI record.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p El2 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] El2
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace containing the variables used to populate \p El1 and \p Hdr.
!> \param[in] Channels
!>   Indices returned by ioda::ObsSpace::obsvariables().channels().
!> \param[in] Flags
!>   Pointer to a ioda::ObsDataVector<int> object containing QC flags.
!> \param[in] ObsErrors
!>   Pointer to a ioda::ObsDataVector<float> object containing observation errors.
!> \param[in] JediVarName
!>   Name of the JEDI variables (in the ObsValue, ObsError and GrossErrorProbability groups)
!>   used to populate El1 and Hdr. The variables can either have no channel suffix (in which case
!>   \p El2 will have only a single row) or have suffixes representing the indices specified in
!>   \p Channels.
!> \param[in] PackPGEs
!>   Optional; true by default. If set to false, PGEs won't be stored in packed form.
!>   The Ops_VarobPGEs subroutine expects PGEs to be stored in packed form for most varobs fields,
!>   but not all; the exceptions are mostly GNSSRO-related.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillelementtype2dfromsimulatedvariable_norecords( &
  Hdr, OpsVarName, NumObs, El2, ObsSpace, Channels, Flags, ObsErrors, JediVarName, PackPGEs)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(integer64), intent(in)                  :: NumObs
type(Element_type), pointer                     :: El2(:,:)
type(c_ptr), value, intent(in)                  :: ObsSpace
integer(c_int), intent(in)                      :: Channels(:)
type(c_ptr), value, intent(in)                  :: Flags
type(c_ptr), value, intent(in)                  :: ObsErrors
character(len=*), intent(in)                    :: JediVarName
logical, optional, intent(in)                   :: PackPGEs

! Local declarations:
logical                                         :: DoPackPGEs
real(kind=c_double)                             :: ObsValue(NumObs)
integer(kind=c_int)                             :: Flag(NumObs)
real(kind=c_float)                              :: ObsError(NumObs)
real(kind=c_double)                             :: PGE(NumObs)
real(kind=c_double)                             :: MissingDouble
real(kind=c_float)                              :: MissingFloat
character(len=max_varname_with_channel_length)  :: JediVarNamesWithChannels(max(size(Channels), 1))

integer                                         :: iChannel, iObs
character(len=*), parameter                     :: &
  RoutineName = "opsinputs_fill_fillelementtype2dfromsimulatedvariable_norecords"
character(len=256)                              :: ErrorMessage

! Body:

if (present(PackPGEs)) then
  DoPackPGEs = PackPGEs
else
  DoPackPGEs = .true.
end if

! The types of floating-point numbers used in this function are a bit confusing. OPS stores
! observation values as doubles, whereas JEDI stores them as floats. However, the Fortran interface
! to the IODA ObsSpace is only partially implemented: obsspace_get_db_real32 doesn't work, only
! obsspace_get_db_real64 does. So we need to retrieve observation values as doubles. Observation
! errors, though, are retrieved as floats.

MissingDouble = missing_value(0.0_c_double)
MissingFloat  = missing_value(0.0_c_float)

JediVarNamesWithChannels = opsinputs_fill_varnames_with_channels(JediVarName, Channels)

if (obsspace_has(ObsSpace, "ObsValue", JediVarNamesWithChannels(1))) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, El2, &
                 num_levels = int(size(JediVarNamesWithChannels), kind=integer64))

  do iChannel = 1, size(JediVarNamesWithChannels)
    ! Retrieve data from JEDI:
    ! - observation value
    call obsspace_get_db(ObsSpace, "ObsValue", JediVarNamesWithChannels(iChannel), ObsValue)
    ! - QC flag
    if (opsinputs_obsdatavector_int_has(Flags, JediVarNamesWithChannels(iChannel))) then
      call opsinputs_obsdatavector_int_get(Flags, JediVarNamesWithChannels(iChannel), Flag)
    else
      write (ErrorMessage, '(A,A,A)') &
        "Warning: variable ", JediVarNamesWithChannels(iChannel), "@ObsError not found"
      call gen_warn(RoutineName, ErrorMessage)
      Flag(:) = 0 ! assume all observations passed QC
    end if
    ! - observation error
    if (opsinputs_obsdatavector_float_has(ObsErrors, JediVarNamesWithChannels(iChannel))) then
      call opsinputs_obsdatavector_float_get(ObsErrors, JediVarNamesWithChannels(iChannel), ObsError)
    else
      write (ErrorMessage, '(A,A,A)') &
        "Warning: variable ", JediVarNamesWithChannels(iChannel), "@ObsError not found"
      call gen_warn(RoutineName, ErrorMessage)
      ObsError(:) = MissingFloat
    end if
    ! - gross error probability
    if (obsspace_has(ObsSpace, "GrossErrorProbability", JediVarNamesWithChannels(iChannel))) then
      call obsspace_get_db(ObsSpace, "GrossErrorProbability", JediVarNamesWithChannels(iChannel), &
                           PGE)
    else
      PGE(:) = MissingDouble
    end if

    ! Fill the OPS data structures
    do iObs = 1, NumObs
      if (ObsValue(iObs) /= MissingDouble) El2(iObs, iChannel) % Value = ObsValue(iObs)
      if (ObsError(iObs) /= MissingFloat)  El2(iObs, iChannel) % OBErr = ObsError(iObs)
      if (Flag(iObs) /= 0)                 El2(iObs, iChannel) % Flags = ibset(0, FinalRejectFlag)
      call opsinputs_fill_setpgefinal(PGE(iObs), MissingDouble, DoPackPGEs, El2(iObs, iChannel))
    end do
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine opsinputs_fill_fillelementtype2dfromsimulatedvariable_norecords

! ------------------------------------------------------------------------------

!> Populate a 2D array of Element_type objects and its header from a set of variables whose name
!> is included in the list passed to the 'simulate' option of the JEDI ObsSpace.
!>
!> This is the implementation used when each OPS observation should correspond to a single JEDI
!> record. There's another implementation (above) used when each OPS observation should correspond
!> to a single JEDI location (possibly with multiple channels).
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p El2 corresponds.
!> \param[in] JediToOpsLayoutMapping
!>   Data needed to map JEDI locations stored on the current PE to OPS observations.
!> \param[inout] El2
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace containing the variables used to populate \p El1 and \p Hdr.
!> \param[in] Flags
!>   Pointer to a ioda::ObsDataVector<int> object containing QC flags.
!> \param[in] ObsErrors
!>   Pointer to a ioda::ObsDataVector<float> object containing observation errors.
!> \param[in] JediVarName
!>   Name of the JEDI variables (in the ObsValue, ObsError and GrossErrorProbability groups)
!>   used to populate El1 and Hdr.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillelementtype2dfromsimulatedvariable_records( &
  Hdr, OpsVarName, JediToOpsLayoutMapping, El2, ObsSpace, Flags, ObsErrors, JediVarName, PackPGEs)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)            :: Hdr
character(len=*), intent(in)                       :: OpsVarName
type(opsinputs_jeditoopslayoutmapping), intent(in) :: JediToOpsLayoutMapping
type(Element_type), pointer                        :: El2(:,:)
type(c_ptr), value, intent(in)                     :: ObsSpace
type(c_ptr), value, intent(in)                     :: Flags
type(c_ptr), value, intent(in)                     :: ObsErrors
character(len=*), intent(in)                       :: JediVarName
logical, optional, intent(in)                      :: PackPGEs

! Local declarations:
logical                                            :: DoPackPGEs
real(kind=c_double)                                :: ObsValue(JediToOpsLayoutMapping % NumJediObs)
integer(kind=c_int)                                :: Flag(JediToOpsLayoutMapping % NumJediObs)
real(kind=c_float)                                 :: ObsError(JediToOpsLayoutMapping % NumJediObs)
real(kind=c_double)                                :: PGE(JediToOpsLayoutMapping % NumJediObs)
real(kind=c_double)                                :: MissingDouble
real(kind=c_float)                                 :: MissingFloat
integer                                            :: iObs, iLevel, iJediObs, numLevels
character(len=*), parameter                        :: &
  RoutineName = "opsinputs_fill_fillelementtype2dfromsimulatedvariable_records"
character(len=256)                                 :: ErrorMessage

! Body:

if (present(PackPGEs)) then
  DoPackPGEs = PackPGEs
else
  DoPackPGEs = .true.
end if

! The types of floating-point numbers used in this function are a bit confusing. OPS stores
! observation values as doubles, whereas JEDI stores them as floats. However, the Fortran interface
! to the IODA ObsSpace is only partially implemented: obsspace_get_db_real32 doesn't work, only
! obsspace_get_db_real64 does. So we need to retrieve observation values as doubles. Observation
! errors, though, are retrieved as floats.

MissingDouble = missing_value(0.0_c_double)
MissingFloat  = missing_value(0.0_c_float)

if (obsspace_has(ObsSpace, "ObsValue", JediVarName)) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, JediToOpsLayoutMapping % NumOpsObs, El2, &
                 num_levels = int(JediToOpsLayoutMapping % MaxNumLevelsPerObs, kind = integer64))

  ! Retrieve data from JEDI:
  ! - observation value
  call obsspace_get_db(ObsSpace, "ObsValue", JediVarName, ObsValue)
  ! - QC flag
  if (opsinputs_obsdatavector_int_has(Flags, JediVarName)) then
    call opsinputs_obsdatavector_int_get(Flags, JediVarName, Flag)
  else
    write (ErrorMessage, '(A,A,A)') &
      "Warning: variable ", JediVarName, "@ObsError not found"
    call gen_warn(RoutineName, ErrorMessage)
    Flag(:) = 0 ! assume all observations passed QC
  end if
  ! - observation error
  if (opsinputs_obsdatavector_float_has(ObsErrors, JediVarName)) then
    call opsinputs_obsdatavector_float_get(ObsErrors, JediVarName, ObsError)
  else
    write (ErrorMessage, '(A,A,A)') &
      "Warning: variable ", JediVarName, "@ObsError not found"
    call gen_warn(RoutineName, ErrorMessage)
    ObsError(:) = MissingFloat
  end if
  ! - gross error probability
  if (obsspace_has(ObsSpace, "GrossErrorProbability", JediVarName)) then
    call obsspace_get_db(ObsSpace, "GrossErrorProbability", JediVarName, &
                         PGE)
  else
    PGE(:) = MissingDouble
  end if

  ! Fill the OPS data structures
  do iObs = 1, JediToOpsLayoutMapping % NumOpsObs
    numLevels = JediToOpsLayoutMapping % RecordStarts(iObs + 1) - &
                JediToOpsLayoutMapping % RecordStarts(iObs)
    do iLevel = 1, numLevels
      iJediObs = JediToOpsLayoutMapping % LocationsOrderedByRecord( &
        JediToOpsLayoutMapping % RecordStarts(iObs) + (iLevel - 1))
      if (ObsValue(iJediObs) /= MissingDouble) then
        El2(iObs, iLevel) % Value = ObsValue(iJediObs)
      end if
      if (ObsError(iJediObs) /= MissingFloat) then
        El2(iObs, iLevel) % OBErr = ObsError(iJediObs)
      end if
      if (Flag(iJediObs) /= 0) then
        El2(iObs, iLevel) % Flags = ibset(0, FinalRejectFlag)
      end if
      call opsinputs_fill_setpgefinal(PGE(iJediObs), MissingDouble, DoPackPGEs, El2(iObs, iLevel))
    end do
    El2(iObs, numLevels + 1 : JediToOpsLayoutMapping % MaxNumLevelsPerObs) % Flags = &
      ibset(0, FinalRejectFlag)
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine opsinputs_fill_fillelementtype2dfromsimulatedvariable_records

! ------------------------------------------------------------------------------
!> Populate a 2D array of Element_type objects and its header from a set of variables whose name
!> is included in the list passed to the 'simulate' option of the JEDI ObsSpace.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p El2 corresponds.
!> \param[in] JediToOpsLayoutMapping
!>   Data needed to map JEDI locations stored on the current PE to OPS observations.
!> \param[inout] El2
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace containing the variables used to populate \p El1 and \p Hdr.
!> \param[in] Channels
!>   Indices returned by ioda::ObsSpace::obsvariables().channels().
!> \param[in] Flags
!>   Pointer to a ioda::ObsDataVector<int> object containing QC flags.
!> \param[in] ObsErrors
!>   Pointer to a ioda::ObsDataVector<float> object containing observation errors.
!> \param[in] JediVarName
!>   Name of the JEDI variables (in the ObsValue, ObsError and GrossErrorProbability groups) used
!>   to populate El1 and Hdr. If each JEDI location needs to be mapped to a separate OPS
!>   observation, the variables can either have no channel suffix (in which case \p El2 will have
!>   only a single row) or have suffixes representing the indices specified in \p Channels.
!> \param[in] PackPGEs
!>   Optional; true by default. If set to false, PGEs won't be stored in packed form.
!>   The Ops_VarobPGEs subroutine expects PGEs to be stored in packed form for most varobs fields,
!>   but not all; the exceptions are mostly GNSSRO-related.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillelementtype2dfromsimulatedvariable( &
  Hdr, OpsVarName, JediToOpsLayoutMapping, El2, ObsSpace, Channels, Flags, ObsErrors, JediVarName, &
  PackPGEs)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)            :: Hdr
character(len=*), intent(in)                       :: OpsVarName
type(opsinputs_jeditoopslayoutmapping), intent(in) :: JediToOpsLayoutMapping
type(Element_type), pointer                        :: El2(:,:)
type(c_ptr), value, intent(in)                     :: ObsSpace
integer(c_int), intent(in)                         :: Channels(:)
type(c_ptr), value, intent(in)                     :: Flags
type(c_ptr), value, intent(in)                     :: ObsErrors
character(len=*), intent(in)                       :: JediVarName
logical, optional, intent(in)                      :: PackPGEs

! Body:

if (JediToOpsLayoutMapping % ConvertRecordsToMultilevelObs) then
  call opsinputs_fill_fillelementtype2dfromsimulatedvariable_records( &
    Hdr, OpsVarName, JediToOpsLayoutMapping, El2, ObsSpace, Flags, ObsErrors, JediVarName, PackPGEs)
else
  call opsinputs_fill_fillelementtype2dfromsimulatedvariable_norecords( &
    Hdr, OpsVarName, JediToOpsLayoutMapping % NumOpsObs, El2, ObsSpace, Channels, Flags, &
    ObsErrors, JediVarName, PackPGEs)
end if

end subroutine opsinputs_fill_fillelementtype2dfromsimulatedvariable

! ------------------------------------------------------------------------------

!> Populate a 1D array of Element_type objects and its header from an arbitrary JEDI variable
!> (not included in the list passed to the 'simulate' option of the JEDI ObsSpace) containing
!> obsevation values and optionally another variable containing observation errors.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p El1 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] El1
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variables.
!> \param[in] JediValueVarName
!>   Name of the JEDI variable containing observation values.
!> \param[in] JediValueGroup
!>   Group of the JEDI variable containing observation values.
!> \param[in] JediErrorVarName
!>   (Optional) Name of the JEDI variable containing observation errors.
!> \param[in] JediErrorGroup
!>   (Optional) Group of the JEDI variable containing observation errors.
!> \param[in] PackPGEs
!>   Optional; true by default. If set to false, PGEs won't be stored in packed form.
!>   The Ops_VarobPGEs subroutine expects PGEs to be stored in packed form for most varobs fields,
!>   but not all; the exceptions are mostly GNSSRO-related.
!>   (At present, this routine sets all PGEs to missing values because it's not obvious whether
!>   they are needed for non-simulated variables. If necessary, it could be modified to retrieve
!>   them from the same group of JEDI variables, GrossErrorProbability, as
!>   opsinputs_fill_fillelementtypefromnormalvariable).
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillelementtypefromnormalvariable( &
  Hdr, OpsVarName, NumObs, El1, ObsSpace, &
  JediValueVarName, JediValueGroup, JediErrorVarName, JediErrorGroup, PackPGEs)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(integer64), intent(in)                  :: NumObs
type(Element_type), pointer                     :: El1(:)
type(c_ptr), value, intent(in)                  :: ObsSpace
character(len=*), intent(in)                    :: JediValueVarName
character(len=*), intent(in)                    :: JediValueGroup
character(len=*), optional, intent(in)          :: JediErrorVarName
character(len=*), optional, intent(in)          :: JediErrorGroup
logical, optional, intent(in)                   :: PackPGEs

! Local declarations:
logical                                         :: DoPackPGEs
real(kind=c_double)                             :: ObsValue(NumObs)
real(kind=c_float)                              :: ObsError(NumObs)
real(kind=c_double)                             :: MissingDouble
real(kind=c_float)                              :: MissingFloat
integer                                         :: i
character(len=*), parameter                     :: &
  RoutineName = "opsinputs_fill_fillelementtypefromnormalvariable"
character(len=256)                              :: ErrorMessage

! Body:

if (present(JediErrorVarName) .neqv. present(JediErrorGroup)) then
  write (ErrorMessage, '(A)') &
    "JediErrorVarName and JediErrorGroup must be either both absent or both present"
  call gen_warn(RoutineName, ErrorMessage)
end if

if (present(PackPGEs)) then
  DoPackPGEs = PackPGEs
else
  DoPackPGEs = .true.
end if

MissingDouble = missing_value(0.0_c_double)

if (obsspace_has(ObsSpace, JediValueGroup, JediValueVarName)) then
  ! Retrieve data from JEDI
  call obsspace_get_db(ObsSpace, JediValueGroup, JediValueVarName, ObsValue)
  if (present(JediErrorVarName) .and. present(JediErrorGroup)) then
    if (obsspace_has(ObsSpace, JediErrorGroup, JediErrorVarName)) then
      call obsspace_get_db(ObsSpace, JediErrorGroup, JediErrorVarName, ObsError)
    else
      write (ErrorMessage, '("Variable ",A,"@",A," not found")') JediErrorVarName, JediErrorGroup
      call gen_warn(RoutineName, ErrorMessage)
      ObsError(:) = MissingDouble
    end if
  else
    ObsError(:) = MissingDouble
  end if

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, El1)
  do i = 1, NumObs
    if (ObsValue(i) /= MissingDouble) El1(i) % Value = ObsValue(i)
    if (ObsError(i) /= MissingDouble)  El1(i) % OBErr = ObsError(i)
    ! We could also fill Flags and PGEFinal if these quantities were available in separate JEDI
    ! variables. At present, however, we don't even have a use case where there is a separate
    ! variable storing the observation error.

    ! Set El1(i) % PGEFinal to 'missing'
    call opsinputs_fill_setpgefinal(MissingDouble, MissingDouble, DoPackPGEs, El1(i))
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine opsinputs_fill_fillelementtypefromnormalvariable

! ------------------------------------------------------------------------------

!> Populate a 2D array of Element_type objects and its header from an arbitrary JEDI variable
!> (not included in the list passed to the 'simulate' option of the JEDI ObsSpace) containing
!> obsevation values and optionally another variable containing observation errors.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which El1 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] El2
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variables. The variables can
!>   have either no channel suffix (in which case \p El2 will have only a single row) or suffixes
!>   representing the indices specified in \p Channels.
!> \param[in] Channel indices returned by ioda::ObsSpace::obsvariables().channels().
!> \param[in] JediValueVarName
!>   Name of the JEDI variable containing observation values.
!> \param[in] JediValueGroup
!>   Group of the JEDI variable containing observation values.
!> \param[in] JediErrorVarName
!>   (Optional) Name of the JEDI variable containing observation errors.
!> \param[in] JediErrorGroup
!>   (Optional) Group of the JEDI variable containing observation errors.
!> \param[in] PackPGEs
!>   Optional; true by default. If set to false, PGEs won't be stored in packed form.
!>   The Ops_VarobPGEs subroutine expects PGEs to be stored in packed form for most varobs fields,
!>   but not all; the exceptions are mostly GNSSRO-related.
!>   (At present, this routine sets all PGEs to missing values because it's not obvious whether
!>   they are needed for non-simulated variables. If necessary, it could be modified to retrieve
!>   them from the same group of JEDI variables, GrossErrorProbability, as
!>   opsinputs_fill_fillelementtype2dfromnormalvariable).
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillelementtype2dfromnormalvariable( &
  Hdr, OpsVarName, NumObs, El2, ObsSpace, Channels, &
  JediValueVarName, JediValueGroup, JediErrorVarName, JediErrorGroup, PackPGEs)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(integer64), intent(in)                  :: NumObs
type(Element_type), pointer                     :: El2(:,:)
type(c_ptr), value, intent(in)                  :: ObsSpace
integer(c_int), intent(in)                      :: Channels(:)
character(len=*), intent(in)                    :: JediValueVarName
character(len=*), intent(in)                    :: JediValueGroup
character(len=*), optional, intent(in)          :: JediErrorVarName
character(len=*), optional, intent(in)          :: JediErrorGroup
logical, optional, intent(in)                   :: PackPGEs

! Local declarations:
logical                                         :: DoPackPGEs
real(kind=c_double)                             :: ObsValue(NumObs)
real(kind=c_float)                              :: ObsError(NumObs)
real(kind=c_double)                             :: MissingDouble
real(kind=c_float)                              :: MissingFloat
character(len=max_varname_with_channel_length)  :: &
  JediValueVarNamesWithChannels(max(size(Channels), 1)), &
  JediErrorVarNamesWithChannels(max(size(Channels), 1))
integer                                         :: iChannel, iObs
character(len=*), parameter                     :: &
  RoutineName = "opsinputs_fill_fillelementtype2dfromnormalvariable"
character(len=256)                              :: ErrorMessage

! Body:

if (present(JediErrorVarName) .neqv. present(JediErrorGroup)) then
  write (ErrorMessage, '(A)') &
    "JediErrorVarName and JediErrorGroup must be either both absent or both present"
  call gen_warn(RoutineName, ErrorMessage)
end if

if (present(PackPGEs)) then
  DoPackPGEs = PackPGEs
else
  DoPackPGEs = .true.
end if

MissingDouble = missing_value(0.0_c_double)


!check for lev or LEV in varname
!this is currently only required for  clw
if (index(JediValueVarName,"lev") > 0 .OR. index(JediValueVarName,"LEV") > 0) then

  !the actual names required are levels not channels
  !the main difference is lack of an "_" e.g. lev1, lev2 , etc
  JediValueVarNamesWithChannels = opsinputs_fill_varnames_with_levels( &
    JediValueVarName, Channels)
  if (present(JediErrorVarName)) then
    JediErrorVarNamesWithChannels = opsinputs_fill_varnames_with_levels( &
      JediErrorVarName, Channels)
  end if
  
else

  JediValueVarNamesWithChannels = opsinputs_fill_varnames_with_channels( &
    JediValueVarName, Channels)
  if (present(JediErrorVarName)) then
    JediErrorVarNamesWithChannels = opsinputs_fill_varnames_with_channels( &
      JediErrorVarName, Channels)
  end if

end if

if (obsspace_has(ObsSpace, JediValueGroup, JediValueVarNamesWithChannels(1))) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, El2, &
                 num_levels = int(size(JediValueVarNamesWithChannels), kind=integer64))

  do iChannel = 1, size(JediValueVarNamesWithChannels)
    ! Retrieve data from JEDI:
    ! - observation value
    call obsspace_get_db(ObsSpace, JediValueGroup, JediValueVarNamesWithChannels(iChannel), &
                         ObsValue)
    ! - observation error
    if (present(JediErrorVarName) .and. present(JediErrorGroup)) then
      if (obsspace_has(ObsSpace, JediErrorGroup, JediErrorVarNamesWithChannels(iChannel))) then
        call obsspace_get_db(ObsSpace, JediErrorGroup, JediErrorVarNamesWithChannels(iChannel), &
                             ObsError)
      else
        write (ErrorMessage, '("Variable ",A,"@",A," not found")') &
          JediErrorVarNamesWithChannels(iChannel), JediErrorGroup
        call gen_warn(RoutineName, ErrorMessage)
        ObsError(:) = MissingDouble
      end if
    else
      ObsError(:) = MissingDouble
    end if

    ! Fill the OPS data structures
    do iObs = 1, NumObs
      if (ObsValue(iObs) /= MissingDouble) El2(iObs, iChannel) % Value = ObsValue(iObs)
      if (ObsError(iObs) /= MissingDouble) El2(iObs, iChannel) % OBErr = ObsError(iObs)
      ! We could also fill Flags and PGEFinal if these quantities were available in separate JEDI
      ! variables. At present, however, we don't even have a use case where there is a separate
      ! variable storing the observation error.

      ! Set El1(i) % PGEFinal to 'missing'
      call opsinputs_fill_setpgefinal(MissingDouble, MissingDouble, DoPackPGEs, El2(iObs, iChannel))
    end do
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.

end subroutine opsinputs_fill_fillelementtype2dfromnormalvariable

! ------------------------------------------------------------------------------

!> Populate a 1D array of real numbers and its header from a JEDI variable.
!>
!> If each OPS observation needs to correspond to a single JEDI record, then the array is filled
!> with data taken from the first location in each record.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Real1 corresponds.
!> \param[in] JediToOpsLayoutMapping
!>   Data needed to map JEDI locations stored on the current PE to OPS observations.
!> \param[inout] Real1
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variable.
!> \param[in] JediVarName
!>   Name of the JEDI variable used to populate \p Real1.
!> \param[in] JediGroup
!>   Group of the JEDI variable used to populate \p Real1.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillreal( &
  Hdr, OpsVarName, JediToOpsLayoutMapping, Real1, ObsSpace, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)            :: Hdr
character(len=*), intent(in)                       :: OpsVarName
type(opsinputs_jeditoopslayoutmapping), intent(in) :: JediToOpsLayoutMapping
real(real64), pointer, intent(out)                 :: Real1(:)
type(c_ptr), value, intent(in)                     :: ObsSpace
character(len=*), intent(in)                       :: JediVarName
character(len=*), intent(in)                       :: JediVarGroup

! Local declarations:
real(kind=c_double)                                :: VarValue(JediToOpsLayoutMapping % NumJediObs)
real(kind=c_double)                                :: CurrentVarValue
real(kind=c_double)                                :: MissingDouble
integer                                            :: i

! Body:

MissingDouble = missing_value(0.0_c_double)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarName)) then
  ! Retrieve data from JEDI
  call obsspace_get_db(ObsSpace, JediVarGroup, JediVarName, VarValue)

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, JediToOpsLayoutMapping % NumOpsObs, Real1)
  do i = 1, JediToOpsLayoutMapping % NumOpsObs
    if (JediToOpsLayoutMapping % ConvertRecordsToMultilevelObs) then
      if (JediToOpsLayoutMapping % RecordStarts(i + 1) > JediToOpsLayoutMapping % RecordStarts(i)) then
        ! This record is non-empty. Use the first location from that record.
        CurrentVarValue = VarValue(JediToOpsLayoutMapping % LocationsOrderedByRecord( &
                                     JediToOpsLayoutMapping % RecordStarts(i)))
      else
        ! This record is empty
        CurrentVarValue = MissingDouble
      end if
    else
      CurrentVarValue = VarValue(i)
    end if

    if (CurrentVarValue /= MissingDouble) Real1(i) = CurrentVarValue
  end do
end if
end subroutine opsinputs_fill_fillreal

! ------------------------------------------------------------------------------

!> Populate a 2D array of real numbers and its header from a JEDI variable.
!>
!> This is the implementation used when each OPS observation should correspond to a single JEDI
!> location (possibly with multiple channels). There's another implementation (below) used when each
!> OPS observation should correspond to a single JEDI record.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Real1 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] Real2
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variable. The variable can
!>   have either no channel suffix (in which case \p Real2 will have only a single row) or suffixes
!>   representing the indices specified in \p Channels.
!> \param[in] Channels
!>   Indices returned by ioda::ObsSpace::obsvariables().channels().
!> \param[in] JediVarName
!>   Name of the JEDI variable used to populate \p Real2. This can represent either a single
!>   variable with no channel suffix (in which case \p Real2 will have only a single row) or a set
!>   of variables with suffixes corresponding to the indices specified in \p Channels.
!> \param[in] JediGroup
!>   Group of the JEDI variable used to populate \p Real2.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillreal2d_norecords( &
  Hdr, OpsVarName, NumObs, Real2, ObsSpace, Channels, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(integer64), intent(in)                  :: NumObs
real(real64), pointer, intent(out)              :: Real2(:,:)
type(c_ptr), value, intent(in)                  :: ObsSpace
integer(c_int), intent(in)                      :: Channels(:)
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup

! Local declarations:
real(kind=c_double)                             :: VarValue(NumObs)
real(kind=c_double)                             :: MissingDouble
character(len=max_varname_with_channel_length)  :: JediVarNamesWithChannels(max(size(Channels), 1))
integer                                         :: iChannel

! Body:

MissingDouble = missing_value(0.0_c_double)

JediVarNamesWithChannels = opsinputs_fill_varnames_with_channels(JediVarName, Channels)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarNamesWithChannels(1))) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, Real2, &
                 num_levels = int(size(JediVarNamesWithChannels), kind=integer64))
  do iChannel = 1, size(JediVarNamesWithChannels)
    ! Retrieve data from JEDI
    call obsspace_get_db(ObsSpace, JediVarGroup, JediVarNamesWithChannels(iChannel), VarValue)

    ! Fill the OPS data structures
    where (VarValue /= MissingDouble)
      Real2(:, iChannel) = VarValue
    end where
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine opsinputs_fill_fillreal2d_norecords

! ------------------------------------------------------------------------------

!> Populate a 2D array of real numbers and its header from a JEDI variable.
!>
!> This is the implementation used when each OPS observation should correspond to a single JEDI
!> record. There's another implementation (above) used when each OPS observation should correspond
!> to a single JEDI location (possibly with multiple channels).
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Real1 corresponds.
!> \param[in] JediToOpsLayoutMapping
!>   Data needed to map JEDI locations stored on the current PE to OPS observations.
!> \param[inout] Real2
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variable.
!> \param[in] JediVarName
!>   Name of the JEDI variable used to populate \p Real2.
!> \param[in] JediGroup
!>   Group of the JEDI variable used to populate \p Real2.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillreal2d_records( &
  Hdr, OpsVarName, JediToOpsLayoutMapping, Real2, ObsSpace, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)            :: Hdr
character(len=*), intent(in)                       :: OpsVarName
type(opsinputs_jeditoopslayoutmapping), intent(in) :: JediToOpsLayoutMapping
real(real64), pointer, intent(out)                 :: Real2(:,:)
type(c_ptr), value, intent(in)                     :: ObsSpace
character(len=*), intent(in)                       :: JediVarName
character(len=*), intent(in)                       :: JediVarGroup

! Local declarations:
real(kind=c_double)                                :: VarValue(JediToOpsLayoutMapping % NumJediObs)
real(kind=c_double)                                :: MissingDouble
integer                                            :: iObs, iLevel, iJediObs, numLevels

! Body:

MissingDouble = missing_value(0.0_c_double)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarName)) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, JediToOpsLayoutMapping % NumOpsObs, Real2, &
                 num_levels = int(JediToOpsLayoutMapping % MaxNumLevelsPerObs, kind = integer64))
  ! Retrieve data from JEDI
  call obsspace_get_db(ObsSpace, JediVarGroup, JediVarName, VarValue)

  ! Fill the OPS data structures
  do iObs = 1, JediToOpsLayoutMapping % NumOpsObs
    numLevels = JediToOpsLayoutMapping % RecordStarts(iObs + 1) - &
                JediToOpsLayoutMapping % RecordStarts(iObs)
    do iLevel = 1, numLevels
      iJediObs = JediToOpsLayoutMapping % LocationsOrderedByRecord( &
        JediToOpsLayoutMapping % RecordStarts(iObs) + (iLevel - 1))
      if (VarValue(iJediObs) /= MissingDouble) then
        Real2(iObs, iLevel) = VarValue(iJediObs)
      end if
    end do
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine opsinputs_fill_fillreal2d_records

! ------------------------------------------------------------------------------

!> Populate a 2D array of real numbers and its header from a JEDI variable.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Real1 corresponds.
!> \param[in] JediToOpsLayoutMapping
!>   Data needed to map JEDI locations stored on the current PE to OPS observations.
!> \param[inout] Real2
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variable. The variable can
!>   have either no channel suffix (in which case \p Real2 will have only a single row) or suffixes
!>   representing the indices specified in \p Channels.
!> \param[in] Channels
!>   Indices returned by ioda::ObsSpace::obsvariables().channels().
!> \param[in] JediVarName
!>   Name of the JEDI variable used to populate \p Real2. If each JEDI location needs to be mapped
!>   to a separate OPS observation, this can represent either a single variable with no channel
!>   suffix (in which case \p Real2 will have only a single row) or a set of variables with
!>   suffixes corresponding to the indices specified in \p Channels.
!> \param[in] JediGroup
!>   Group of the JEDI variable used to populate \p Real2.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillreal2d( &
  Hdr, OpsVarName, JediToOpsLayoutMapping, Real2, ObsSpace, Channels, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)            :: Hdr
character(len=*), intent(in)                       :: OpsVarName
type(opsinputs_jeditoopslayoutmapping), intent(in) :: JediToOpsLayoutMapping
real(real64), pointer, intent(out)                 :: Real2(:,:)
type(c_ptr), value, intent(in)                     :: ObsSpace
integer(c_int), intent(in)                         :: Channels(:)
character(len=*), intent(in)                       :: JediVarName
character(len=*), intent(in)                       :: JediVarGroup

! Body:

if (JediToOpsLayoutMapping % ConvertRecordsToMultilevelObs) then
  call opsinputs_fill_fillreal2d_records( &
    Hdr, OpsVarName, JediToOpsLayoutMapping, Real2, ObsSpace, JediVarName, JediVarGroup)
else
  call opsinputs_fill_fillreal2d_norecords( &
    Hdr, OpsVarName, JediToOpsLayoutMapping % NumOpsObs, Real2, ObsSpace, Channels, &
    JediVarName, JediVarGroup)
end if

end subroutine opsinputs_fill_fillreal2d

! ------------------------------------------------------------------------------

!> Populate a 1D array of real numbers and its header from a GeoVaL.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Real1 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] Real1
!>   Pointer to the array to be populated.
!> \param[in] GeoVals
!>   A container holding the specified GeoVaL.
!> \param[in] JediVarName
!>   Name of the GeoVaL used to populate \p Real1.
!>
!> \note If you're calling this function from opsinputs_fill_populateobservations, be sure
!> to update opsinputs_fill_addrequiredgeovars by adding \p JediVarName to the list of
!> GeoVaLs required by the VarObsWriter.
!>
!> \note This function returns early (without a warning) if the specified GeoVaL is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillrealfromgeoval( &
  Hdr, OpsVarName, NumObs, Real1, GeoVals, JediVarName)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(integer64), intent(in)                  :: NumObs
real(real64), pointer, intent(out)              :: Real1(:)
type(ufo_geovals), intent(in)                   :: GeoVals
character(len=*), intent(in)                    :: JediVarName

! Local declarations:
type(ufo_geoval), pointer                       :: GeoVal
real(kind_real)                                 :: MissingReal

character(len=*), parameter                     :: &
  RoutineName = "opsinputs_fill_fillrealfromgeoval"
character(len=256)                              :: ErrorMessage

! Body:

MissingReal = missing_value(0.0_c_float)

if (ufo_vars_getindex(GeoVals % variables, JediVarName) > 0) then
  ! Retrieve GeoVal
  call ufo_geovals_get_var(GeoVals, JediVarName, GeoVal)
  if (GeoVal % nval /= 1) then
    write (ErrorMessage, '("GeoVal ",A," contains more than one value per location. &
      &Only the first of these values will be written to the VarObs file")') JediVarName
    call gen_warn(RoutineName, ErrorMessage)
  end if

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, Real1)
  where (GeoVal % vals(1,:) /= MissingReal)
    Real1 = GeoVal % vals(1,:)
  end where
end if
end subroutine opsinputs_fill_fillrealfromgeoval


! ------------------------------------------------------------------------------

!> Populate a 2D array of real numbers and its header from a GeoVaL.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Real2 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] Real2
!>   Pointer to the array to be populated.
!> \param[in] GeoVals
!>   A container holding the specified GeoVaL.
!> \param[in] JediVarName
!>   Name of the GeoVal used to populate \p Real2.
!>
!> \note This function returns early (without a warning) if the specified GeoVaL is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillreal2dfromgeoval( &
  Hdr, OpsVarName, NumObs, Real2, GeoVals, JediVarName)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(integer64), intent(in)                  :: NumObs
real(real64), pointer, intent(out)              :: Real2(:,:)
character(len=*), intent(in)                    :: JediVarName
type(ufo_geovals), intent(in)                   :: GeoVals

! Local declarations:
type(ufo_geoval), pointer                       :: GeoVal
real(kind_real)                                 :: MissingReal

! Body:

MissingReal = missing_value(0.0_c_float)

if (ufo_vars_getindex(GeoVals % variables, JediVarName) > 0) then
  ! Retrieve GeoVal
  call ufo_geovals_get_var(GeoVals, JediVarName, GeoVal)

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, Real2, num_levels = int(GeoVal % nval, kind = 8))
  where (transpose(GeoVal % vals) /= MissingReal)
    Real2 = transpose(GeoVal % vals)
  end where
end if
end subroutine opsinputs_fill_fillreal2dfromgeoval

!> Populate a 2D array of real numbers and its header from either a GeoVaL or an HofX vector.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Real2 corresponds.
!> \param[inout] Real2
!>   Pointer to the array to be populated.
!> \param[in] GeoVals
!>   A container holding the specified GeoVaL.
!> \param[in] JediVarName
!>   Name of the input GeoVal.
!> \param[in] JediToOpsLayoutMapping
!>   Mapping between indices of observations in the JEDI and OPS data structures.
!> \param[in] hofx
!>   HofX data structure.
!> \param[in] varnames
!>   List of simulated variables.
subroutine opsinputs_fill_fillreal2dfromgeovalorhofx( &
  Hdr, OpsVarName, Real2, GeoVals, JediVarName, JediToOpsLayoutMapping, hofx, varnames)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)            :: Hdr
character(len=*), intent(in)                       :: OpsVarName
real(real64), pointer, intent(inout)               :: Real2(:,:)
type(ufo_geovals), intent(in)                      :: GeoVals
character(len=*), intent(in)                       :: JediVarName
type(opsinputs_jeditoopslayoutmapping), intent(in) :: JediToOpsLayoutMapping
real(c_double), intent(in)                         :: hofx(:, :)
type(oops_variables), intent(in)                   :: varnames

! Local declarations:
integer :: i
integer :: hofxIndex

! Body:

if (JediToOpsLayoutMapping % ConvertRecordsToMultiLevelObs) then
   ! Index of variable in hofx array.
   hofxIndex = 0
   do i = 1, size(hofx, 1)
      if (varnames % variable(i) == JediVarName) then
         hofxIndex = i
         exit
      end if
   end do
   if (hofxIndex > 0) then
      call opsinputs_fill_fillreal2dfromhofx( &
           Hdr, OpsVarName, Real2, &
           JediToOpsLayoutMapping, hofx(hofxIndex,:))
   else
      write(*, *) JediVarName, " does not appear in the list of simulated variables"
      call abort()
   end if
else
   call opsinputs_fill_fillreal2dfromgeoval( &
        Hdr, OpsVarName, JediToOpsLayoutMapping % NumOpsObs, Real2, &
        GeoVals, JediVarName)
end if

end subroutine opsinputs_fill_fillreal2dfromgeovalorhofx

! ------------------------------------------------------------------------------

!> Populate a 2D array of real numbers and its header from an HofX vector.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Real2 corresponds.
!> \param[inout] Real2
!>   Pointer to the array to be populated.
!> \param[in] JediToOpsLayoutMapping
!>   Mapping between indices of observations in the JEDI and OPS data structures.
!> \param[in] hofx
!>   The HofX vector to use.
subroutine opsinputs_fill_fillreal2dfromhofx( &
  Hdr, OpsVarName, Real2, JediToOpsLayoutMapping, hofx)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)            :: Hdr
character(len=*), intent(in)                       :: OpsVarName
real(real64), pointer, intent(out)                 :: Real2(:,:)
type(opsinputs_jeditoopslayoutmapping), intent(in) :: JediToOpsLayoutMapping
real(c_double), intent(in)                         :: hofx(:)

! Local declarations:
real(kind_real)                                 :: MissingReal
integer                                         :: iObs, iLevel, iJediObs, numLevels

! Body:

MissingReal = missing_value(0.0_c_double)

! Fill the OPS data structures
call Ops_Alloc(Hdr, OpsVarName, JediToOpsLayoutMapping % NumOpsObs, Real2, &
               num_levels = int(JediToOpsLayoutMapping % MaxNumLevelsPerObs, kind = 8))
do iObs = 1, JediToOpsLayoutMapping % NumOpsObs
   numLevels = JediToOpsLayoutMapping % RecordStarts(iObs + 1) - JediToOpsLayoutMapping % RecordStarts(iObs)
   do iLevel = 1, numLevels
      ! Location of current observation in the ObsSpace.
      iJediObs = JediToOpsLayoutMapping % LocationsOrderedByRecord( &
           JediToOpsLayoutMapping % RecordStarts(iObs) - 1 + iLevel)
      if (hofx(iJediObs) /= MissingReal) then
         Real2(iObs,iLevel) = hofx(iJediObs)
      end if
   end do
end do

end subroutine opsinputs_fill_fillreal2dfromhofx

! ------------------------------------------------------------------------------

!> Stach a series of 1D GeoVaLs, each corresponding to a separate channel, in the
!> columns of a 2D array of real numbers. Output array will be size (iobs, ichans).
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Real2 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] Real2
!>   Pointer to the array to be populated.
!> \param[in] GeoVals
!>   A container holding the specified GeoVaL entries.
!> \param[in] JediVarName
!>   Name of the GeoVal used to populate \p Real2.
!> \param[in] Channels
!>    Channel indices returned by ioda::ObsSpace::obsvariables().channels().
!>
!> \note This function returns early (without a warning) if the specified GeoVaL for the first
!> channel is not found.  We rely on warnings printed by the OPS code whenever data needed
!> to output a requested varfield are not found.  If the specified GeoVaL for the first channel
!> is found and any of the subsequent GeoVaL for a channel are not found an abort is thrown.
subroutine opsinputs_fill_fillreal2dfrom1dgeovalwithchans( &
  Hdr, OpsVarName, NumObs, Real2, GeoVals, JediVarName, Channels)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(integer64), intent(in)                  :: NumObs
real(real64), pointer, intent(out)              :: Real2(:,:)
type(ufo_geovals), intent(in)                   :: GeoVals
character(len=*), intent(in)                    :: JediVarName
integer(c_int), intent(in)                      :: Channels(:)

! Local declarations:
type(ufo_geoval), pointer                       :: GeoVal
real(kind_real)                                 :: MissingReal
integer                                         :: iChannel
character(len=max_varname_with_channel_length)  :: JediVarNamesWithChannels(max(size(Channels), 1))

! Body:
MissingReal = missing_value(0.0_c_float)
JediVarNamesWithChannels = opsinputs_fill_varnames_with_channels(JediVarName, Channels)

if (ufo_vars_getindex(GeoVals % variables, JediVarNamesWithChannels(1)) > 0) then
  do iChannel = 1, size(JediVarNamesWithChannels)
    ! Retrieve GeoVal
    call ufo_geovals_get_var(GeoVals, JediVarNamesWithChannels(iChannel), GeoVal)

    if (GeoVal % nval == 1) then
      if (.not. associated(Real2)) then
        ! Allocate OPS data structures
        call Ops_Alloc(Hdr, OpsVarName, NumObs, Real2, &
                       num_levels = int(size(JediVarNamesWithChannels), kind=integer64))
      end if

      ! Fill the OPS data structures
      where (GeoVal % vals(1,:) /= MissingReal)
        Real2(:, iChannel) = GeoVal % vals(1,:)
      end where
    end if
  end do
end if

end subroutine opsinputs_fill_fillreal2dfrom1dgeovalwithchans

! ------------------------------------------------------------------------------

!> Populate a 1D array of integers and its header from a JEDI variable.
!>
!> If each OPS observation needs to correspond to a single JEDI record, then the array is filled
!> with data taken from the first location in each record.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Int1 corresponds.
!> \param[in] JediToOpsLayoutMapping
!>   Data needed to map JEDI locations stored on the current PE to OPS observations.
!> \param[inout] Int1
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variable.
!> \param[in] JediVarName
!>   Name of the JEDI variable used to populate \p Int1.
!> \param[in] JediGroup
!>   Group of the JEDI variable used to populate \p Int1.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillinteger( &
  Hdr, OpsVarName, JediToOpsLayoutMapping, Int1, ObsSpace, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)            :: Hdr
character(len=*), intent(in)                       :: OpsVarName
type(opsinputs_jeditoopslayoutmapping), intent(in) :: JediToOpsLayoutMapping
integer(integer64), pointer                        :: Int1(:)
type(c_ptr), value, intent(in)                     :: ObsSpace
character(len=*), intent(in)                       :: JediVarName
character(len=*), intent(in)                       :: JediVarGroup

! Local declarations:
integer(kind=4)                                    :: VarValue(JediToOpsLayoutMapping % NumJediObs)
integer(kind=4)                                    :: CurrentVarValue
integer(kind=4)                                    :: MissingInt
integer                                            :: i

! Body:

MissingInt = missing_value(0_c_int32_t)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarName)) then
  ! Retrieve data from JEDI
  call obsspace_get_db(ObsSpace, JediVarGroup, JediVarName, VarValue)

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, JediToOpsLayoutMapping % NumOpsObs, Int1)
  do i = 1, JediToOpsLayoutMapping % NumOpsObs
    if (JediToOpsLayoutMapping % ConvertRecordsToMultilevelObs) then
      if (JediToOpsLayoutMapping % RecordStarts(i + 1) > JediToOpsLayoutMapping % RecordStarts(i)) then
        ! This record is non-empty. Use the first location from that record.
        CurrentVarValue = VarValue(JediToOpsLayoutMapping % LocationsOrderedByRecord( &
                                     JediToOpsLayoutMapping % RecordStarts(i)))
      else
        ! This record is empty
        CurrentVarValue = MissingInt
      end if
    else
      CurrentVarValue = VarValue(i)
    end if

    if (CurrentVarValue /= MissingInt) Int1(i) = CurrentVarValue
  end do
end if
end subroutine opsinputs_fill_fillinteger

! ------------------------------------------------------------------------------

!> Populate a 1D array of strings and its header from a JEDI variable.
!>
!> If each OPS observation needs to correspond to a single JEDI record, then the array is filled
!> with data taken from the first location in each record.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p String1 corresponds.
!> \param[in] JediToOpsLayoutMapping
!>   Data needed to map JEDI locations stored on the current PE to OPS observations.
!> \param[inout] String1
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variable.
!> \param[in] JediVarName
!>   Name of the JEDI variable used to populate \p String1.
!> \param[in] JediGroup
!>   Group of the JEDI variable used to populate \p String1.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillstring( &
  Hdr, OpsVarName, JediToOpsLayoutMapping, StringLen, String1, ObsSpace, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)            :: Hdr
character(len=*), intent(in)                       :: OpsVarName
type(opsinputs_jeditoopslayoutmapping), intent(in) :: JediToOpsLayoutMapping
integer(integer64), intent(in)                     :: StringLen
character(len=StringLen), pointer                  :: String1(:)
type(c_ptr), value, intent(in)                     :: ObsSpace
character(len=*), intent(in)                       :: JediVarName
character(len=*), intent(in)                       :: JediVarGroup

! Local declarations:
character(len=StringLen)                           :: VarValue(JediToOpsLayoutMapping % NumJediObs)
integer                                            :: i

! Body:

if (obsspace_has(ObsSpace, JediVarGroup, JediVarName)) then
  ! Retrieve data from JEDI
  call opsinputs_obsspace_get_db_string(ObsSpace, JediVarGroup, JediVarName, &
                                        int(StringLen, kind=4), VarValue)

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, JediToOpsLayoutMapping % NumOpsObs, String1)
  do i = 1, JediToOpsLayoutMapping % NumOpsObs
    if (JediToOpsLayoutMapping % ConvertRecordsToMultilevelObs) then
      if (JediToOpsLayoutMapping % RecordStarts(i + 1) > JediToOpsLayoutMapping % RecordStarts(i)) then
        ! This record is non-empty. Use the first location from that record.
        String1(i) = VarValue(JediToOpsLayoutMapping % LocationsOrderedByRecord( &
                                JediToOpsLayoutMapping % RecordStarts(i)))
      end if
    else
      String1(i) = VarValue(i)
    end if
  end do
end if
end subroutine opsinputs_fill_fillstring

! ------------------------------------------------------------------------------

!> Populate a 1D array of real numbers representing time offsets and its header from a JEDI
!> variable.
!>
!> If each OPS observation needs to correspond to a single JEDI record, then the array is filled
!> with data taken from the first location in each record.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Real1 corresponds.
!> \param[in] JediToOpsLayoutMapping
!>   Data needed to map JEDI locations stored on the current PE to OPS observations.
!> \param[inout] Real1
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variable.
!> \param[in] JediVarName
!>   Name of the JEDI variable used to populate \p Real1.
!> \param[in] JediGroup
!>   Group of the JEDI variable used to populate \p Real1.
!> \param[in] ReferenceTime
!>   Reference time. JEDI datetimes will be converted into offsets from this time.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_filltimeoffsets( &
  Hdr, OpsVarName, JediToOpsLayoutMapping, Real1, ObsSpace, JediVarName, JediVarGroup, ReferenceTime)
use datetime_mod, only: datetime
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)            :: Hdr
character(len=*), intent(in)                       :: OpsVarName
type(opsinputs_jeditoopslayoutmapping), intent(in) :: JediToOpsLayoutMapping
real(real64), pointer, intent(out)                 :: Real1(:)
type(c_ptr), value, intent(in)                     :: ObsSpace
character(len=*), intent(in)                       :: JediVarName
character(len=*), intent(in)                       :: JediVarGroup
type(datetime), intent(in)                         :: ReferenceTime

! Local declarations:
integer(c_int64_t)                                 :: VarValue(JediToOpsLayoutMapping % NumJediObs)
integer                                            :: i

! Body:

if (obsspace_has(ObsSpace, JediVarGroup, JediVarName)) then
  ! Retrieve data from JEDI
  call opsinputs_obsspace_get_db_datetime_offset_in_seconds( &
    ObsSpace, JediVarGroup, JediVarName, ReferenceTime, VarValue)

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, JediToOpsLayoutMapping % NumOpsObs, Real1)
  do i = 1, JediToOpsLayoutMapping % NumOpsObs
    if (JediToOpsLayoutMapping % ConvertRecordsToMultilevelObs) then
      if (JediToOpsLayoutMapping % RecordStarts(i + 1) > JediToOpsLayoutMapping % RecordStarts(i)) then
        ! This record is non-empty. Use the first location from that record.
        Real1(i) = VarValue(JediToOpsLayoutMapping % LocationsOrderedByRecord( &
                              JediToOpsLayoutMapping % RecordStarts(i)))
      end if
    else
      Real1(i) = VarValue(i)
    end if
  end do
end if
end subroutine opsinputs_fill_filltimeoffsets

! ------------------------------------------------------------------------------

!> Populate a 2D array of real numbers representing time offsets and its header from a JEDI variable.
!>
!> This is the implementation used when each OPS observation should correspond to a single JEDI
!> location (possibly with multiple channels). There's another implementation (below) used when each
!> OPS observation should correspond to a single JEDI record.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Real1 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] Real2
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variable. The variable can
!>   have either no channel suffix (in which case \p Real2 will have only a single row) or suffixes
!>   representing the indices specified in \p Channels.
!> \param[in] Channels
!>   Indices returned by ioda::ObsSpace::obsvariables().channels().
!> \param[in] JediVarName
!>   Name of the JEDI variable used to populate \p Real2. This can represent either a single
!>   variable with no channel suffix (in which case \p Real2 will have only a single row) or a set
!>   of variables with suffixes corresponding to the indices specified in \p Channels.
!> \param[in] JediGroup
!>   Group of the JEDI variable used to populate \p Real2.
!> \param[in] ReferenceTime
!>   Reference time. JEDI datetimes will be converted into offsets from this time.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_filltimeoffsets2d_norecords( &
  Hdr, OpsVarName, NumObs, Real2, ObsSpace, Channels, JediVarName, JediVarGroup, ReferenceTime)
use datetime_mod, only: datetime
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(integer64), intent(in)                  :: NumObs
real(real64), pointer, intent(out)              :: Real2(:,:)
type(c_ptr), value, intent(in)                  :: ObsSpace
integer(c_int), intent(in)                      :: Channels(:)
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup
type(datetime), intent(in)                      :: ReferenceTime

! Local declarations:
integer(c_int64_t)                              :: VarValue(NumObs)
real(kind=c_double)                             :: MissingDouble
character(len=max_varname_with_channel_length)  :: JediVarNamesWithChannels(max(size(Channels), 1))
integer                                         :: iChannel

! Body:

MissingDouble = missing_value(0.0_c_double)

JediVarNamesWithChannels = opsinputs_fill_varnames_with_channels(JediVarName, Channels)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarNamesWithChannels(1))) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, Real2, &
                 num_levels = int(size(JediVarNamesWithChannels), kind=integer64))
  do iChannel = 1, size(JediVarNamesWithChannels)
    ! Retrieve data from JEDI
    call opsinputs_obsspace_get_db_datetime_offset_in_seconds( &
      ObsSpace, JediVarGroup, JediVarNamesWithChannels(iChannel), ReferenceTime, VarValue)

    ! Fill the OPS data structures
    where (VarValue /= MissingDouble)
      Real2(:, iChannel) = VarValue
    end where
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine opsinputs_fill_filltimeoffsets2d_norecords

! ------------------------------------------------------------------------------

!> Populate a 2D array of real numbers representing time offsets and its header from a JEDI variable.
!>
!> This is the implementation used when each OPS observation should correspond to a single JEDI
!> record. There's another implementation (above) used when each OPS observation should correspond
!> to a single JEDI location (possibly with multiple channels).
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Real1 corresponds.
!> \param[in] JediToOpsLayoutMapping
!>   Data needed to map JEDI locations stored on the current PE to OPS observations.
!> \param[inout] Real2
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variable.
!> \param[in] JediVarName
!>   Name of the JEDI variable used to populate \p Real2.
!> \param[in] JediGroup
!>   Group of the JEDI variable used to populate \p Real2.
!> \param[in] ReferenceTime
!>   Reference time. JEDI datetimes will be converted into offsets from this time.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_filltimeoffsets2d_records( &
  Hdr, OpsVarName, JediToOpsLayoutMapping, Real2, ObsSpace, JediVarName, JediVarGroup, ReferenceTime)
use datetime_mod, only: datetime
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)            :: Hdr
character(len=*), intent(in)                       :: OpsVarName
type(opsinputs_jeditoopslayoutmapping), intent(in) :: JediToOpsLayoutMapping
real(real64), pointer, intent(out)                 :: Real2(:,:)
type(c_ptr), value, intent(in)                     :: ObsSpace
character(len=*), intent(in)                       :: JediVarName
character(len=*), intent(in)                       :: JediVarGroup
type(datetime), intent(in)                         :: ReferenceTime

! Local declarations:
integer(c_int64_t)                                 :: VarValue(JediToOpsLayoutMapping % NumJediObs)
real(kind=c_double)                                :: MissingDouble
integer                                            :: iObs, iLevel, iJediObs, numLevels

! Body:

MissingDouble = missing_value(0.0_c_double)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarName)) then
  ! Retrieve data from JEDI
  call opsinputs_obsspace_get_db_datetime_offset_in_seconds( &
    ObsSpace, JediVarGroup, JediVarName, ReferenceTime, VarValue)

  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, JediToOpsLayoutMapping % NumOpsObs, Real2, &
                 num_levels = int(JediToOpsLayoutMapping % MaxNumLevelsPerObs, kind = integer64))
  ! Fill the OPS data structures
  do iObs = 1, JediToOpsLayoutMapping % NumOpsObs
    numLevels = JediToOpsLayoutMapping % RecordStarts(iObs + 1) - &
                JediToOpsLayoutMapping % RecordStarts(iObs)
    do iLevel = 1, numLevels
      iJediObs = JediToOpsLayoutMapping % LocationsOrderedByRecord( &
        JediToOpsLayoutMapping % RecordStarts(iObs) + (iLevel - 1))
      if (VarValue(iJediObs) /= MissingDouble) then
        Real2(iObs, iLevel) = VarValue(iJediObs)
      end if
    end do
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine opsinputs_fill_filltimeoffsets2d_records

! ------------------------------------------------------------------------------

!> Populate a 2D array of real numbers representing time offsets and its header from a JEDI variable.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Real1 corresponds.
!> \param[in] JediToOpsLayoutMapping
!>   Data needed to map JEDI locations stored on the current PE to OPS observations.
!> \param[inout] Real2
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variable. The variable can
!>   have either no channel suffix (in which case \p Real2 will have only a single row) or suffixes
!>   representing the indices specified in \p Channels.
!> \param[in] Channels
!>   Indices returned by ioda::ObsSpace::obsvariables().channels().
!> \param[in] JediVarName
!>   Name of the JEDI variable used to populate \p Real2. If each JEDI location needs to be mapped
!>   to a separate OPS observation, this can represent either a single variable with no channel
!>   suffix (in which case \p Real2 will have only a single row) or a set of variables with
!>   suffixes corresponding to the indices specified in \p Channels.
!> \param[in] JediGroup
!>   Group of the JEDI variable used to populate \p Real2.
!> \param[in] ReferenceTime
!>   Reference time. JEDI datetimes will be converted into offsets from this time.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_filltimeoffsets2d( &
  Hdr, OpsVarName, JediToOpsLayoutMapping, Real2, ObsSpace, Channels, JediVarName, JediVarGroup, &
  ReferenceTime)
use datetime_mod, only: datetime
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)            :: Hdr
character(len=*), intent(in)                       :: OpsVarName
type(opsinputs_jeditoopslayoutmapping), intent(in) :: JediToOpsLayoutMapping
real(real64), pointer, intent(out)                 :: Real2(:,:)
type(c_ptr), value, intent(in)                     :: ObsSpace
integer(c_int), intent(in)                         :: Channels(:)
character(len=*), intent(in)                       :: JediVarName
character(len=*), intent(in)                       :: JediVarGroup
type(datetime), intent(in)                         :: ReferenceTime

! Body:

if (JediToOpsLayoutMapping % ConvertRecordsToMultilevelObs) then
  call opsinputs_fill_filltimeoffsets2d_records(Hdr, OpsVarName, &
                                                JediToOpsLayoutMapping, Real2, &
                                                ObsSpace, JediVarName, JediVarGroup, &
                                                ReferenceTime)
else
  call opsinputs_fill_filltimeoffsets2d_norecords(Hdr, OpsVarName, &
                                                  JediToOpsLayoutMapping % NumOpsObs, Real2, &
                                                  ObsSpace, Channels, JediVarName, JediVarGroup, &
                                                  ReferenceTime)
end if

end subroutine opsinputs_fill_filltimeoffsets2d

! ------------------------------------------------------------------------------

!> Populate a 2D array of Coord_type objects and its header from a JEDI variable.
!>
!> This is the implementation used when each OPS observation should correspond to a single JEDI
!> location (possibly with multiple channels). There's another implementation (below) used when each
!> OPS observation should correspond to a single JEDI record.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Coord2 corresponds.
!> \param[in] NumObs
!>   Number of observations held by this process.
!> \param[inout] Coord2
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variable. The variable can
!>   have either no channel suffix (in which case \p Coord2 will have only a single row) or suffixes
!>   representing the indices specified in \p Channels.
!> \param[in] Channels
!>   Indices returned by ioda::ObsSpace::obsvariables().channels().
!> \param[in] JediVarName
!>   Name of the JEDI variable used to populate \p Coord2. This can represent either a single
!>   variable with no channel suffix (in which case \p Coord2 will have only a single row) or a set
!>   of variables with suffixes corresponding to the indices specified in \p Channels.
!> \param[in] JediGroup
!>   Group of the JEDI variable used to populate \p Coord2.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillcoord2d_norecords( &
  Hdr, OpsVarName, NumObs, Coord2, ObsSpace, Channels, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
integer(integer64), intent(in)                  :: NumObs
type(coord_type), pointer                       :: Coord2(:,:)
type(c_ptr), value, intent(in)                  :: ObsSpace
integer(c_int), intent(in)                      :: Channels(:)
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup

! Local declarations:
real(kind=c_double)                             :: VarValue(NumObs)
real(kind=c_double)                             :: MissingDouble
character(len=max_varname_with_channel_length)  :: JediVarNamesWithChannels(max(size(Channels), 1))
integer                                         :: iChannel

! Body:

MissingDouble = missing_value(0.0_c_double)

JediVarNamesWithChannels = opsinputs_fill_varnames_with_channels(JediVarName, Channels)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarNamesWithChannels(1))) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, Coord2, &
                 num_levels = int(size(JediVarNamesWithChannels), kind=integer64))

  do iChannel = 1, size(JediVarNamesWithChannels)
    ! Retrieve data from JEDI
    call obsspace_get_db(ObsSpace, JediVarGroup, JediVarNamesWithChannels(iChannel), VarValue)

    ! Fill the OPS data structures
    where (VarValue /= MissingDouble)
      Coord2(:, iChannel) % Value = VarValue
    end where
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine opsinputs_fill_fillcoord2d_norecords

! ------------------------------------------------------------------------------

!> Populate a 2D array of Coord_type objects and its header from a JEDI variable.
!>
!> This is the implementation used when each OPS observation should correspond to a single JEDI
!> record. There's another implementation (above) used when each OPS observation should correspond
!> to a single JEDI location (possibly with multiple channels).
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Coord2 corresponds.
!> \param[in] JediToOpsLayoutMapping
!>   Data needed to map JEDI locations stored on the current PE to OPS observations.
!> \param[inout] Coord2
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variable.
!> \param[in] JediVarName
!>   Name of the JEDI variable used to populate \p Coord2.
!> \param[in] JediGroup
!>   Group of the JEDI variable used to populate \p Coord2.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillcoord2d_records( &
  Hdr, OpsVarName, JediToOpsLayoutMapping, Coord2, ObsSpace, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)            :: Hdr
character(len=*), intent(in)                       :: OpsVarName
type(opsinputs_jeditoopslayoutmapping), intent(in) :: JediToOpsLayoutMapping
type(coord_type), pointer                          :: Coord2(:,:)
type(c_ptr), value, intent(in)                     :: ObsSpace
character(len=*), intent(in)                       :: JediVarName
character(len=*), intent(in)                       :: JediVarGroup

! Local declarations:
real(kind=c_double)                                :: VarValue(JediToOpsLayoutMapping % NumJediObs)
real(kind=c_double)                                :: MissingDouble
integer                                            :: iObs, iLevel, iJediObs, numLevels

! Body:

MissingDouble = missing_value(0.0_c_double)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarName)) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, JediToOpsLayoutMapping % NumOpsObs, Coord2, &
                 num_levels = int(JediToOpsLayoutMapping % MaxNumLevelsPerObs, kind = integer64))

  ! Retrieve data from JEDI
  call obsspace_get_db(ObsSpace, JediVarGroup, JediVarName, VarValue)

  ! Fill the OPS data structures
  do iObs = 1, JediToOpsLayoutMapping % NumOpsObs
    numLevels = JediToOpsLayoutMapping % RecordStarts(iObs + 1) - &
                JediToOpsLayoutMapping % RecordStarts(iObs)
    do iLevel = 1, numLevels
      iJediObs = JediToOpsLayoutMapping % LocationsOrderedByRecord( &
        JediToOpsLayoutMapping % RecordStarts(iObs) + (iLevel - 1))
      if (VarValue(iJediObs) /= MissingDouble) then
        Coord2(iObs, iLevel) % Value = VarValue(iJediObs)
      end if
    end do
  end do
end if ! Data not present? OPS will produce a warning -- we don't need to duplicate it.
end subroutine opsinputs_fill_fillcoord2d_records

! ------------------------------------------------------------------------------

!> Populate a 2D array of Coord_type objects and its header from a JEDI variable.
!>
!> \param[inout] Hdr
!>   Header to be populated.
!> \param[in] OpsVarName
!>   Name of the OB_type field to which \p Coord2 corresponds.
!> \param[in] JediToOpsLayoutMapping
!>   Data needed to map JEDI locations stored on the current PE to OPS observations.
!> \param[inout] Coord2
!>   Pointer to the array to be populated.
!> \param[in] ObsSpace
!>   Pointer to ioda::ObsSpace object containing the specified JEDI variable.
!> \param[in] Channels
!>   Indices returned by ioda::ObsSpace::obsvariables().channels().
!> \param[in] JediVarName
!>   Name of the JEDI variable used to populate \p Coord2. If each JEDI location needs to be mapped
!>   to a separate OPS observation, this can represent either a single variable with no channel
!>   suffix (in which case \p Coord2 will have only a single row) or a set of variables with
!>   suffixes corresponding to the indices specified in \p Channels.
!> \param[in] JediGroup
!>   Group of the JEDI variable used to populate \p Coord2.
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillcoord2d( &
  Hdr, OpsVarName, JediToOpsLayoutMapping, Coord2, ObsSpace, Channels, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)            :: Hdr
character(len=*), intent(in)                       :: OpsVarName
type(opsinputs_jeditoopslayoutmapping), intent(in) :: JediToOpsLayoutMapping
type(coord_type), pointer                          :: Coord2(:,:)
type(c_ptr), value, intent(in)                     :: ObsSpace
integer(c_int), intent(in)                         :: Channels(:)
character(len=*), intent(in)                       :: JediVarName
character(len=*), intent(in)                       :: JediVarGroup

! Body:

if (JediToOpsLayoutMapping % ConvertRecordsToMultilevelObs) then
  call opsinputs_fill_fillcoord2d_records( &
    Hdr, OpsVarName, JediToOpsLayoutMapping, Coord2, ObsSpace, JediVarName, JediVarGroup)
else
  call opsinputs_fill_fillcoord2d_norecords( &
    Hdr, OpsVarName, JediToOpsLayoutMapping % NumOpsObs, Coord2, ObsSpace, Channels, &
    JediVarName, JediVarGroup)
end if

end subroutine opsinputs_fill_fillcoord2d

! ------------------------------------------------------------------------------

!> Return an array containing the names of JEDI variables storing individual levels of the
!> variable \p VarName. This routine constructs a character array containing
!> each level no. appended to the varname.
function opsinputs_fill_varnames_with_levels(VarName, Levels) result(VarNames)
implicit none
! Subroutine arguments:
character(len=*), intent(in)                   :: VarName
integer(c_int), intent(in)                  :: Levels(:)

! Local declarations:
character(len=max_varname_with_channel_length) :: VarNames(max(size(Levels), 1))
integer                                        :: i

if (size(Levels) == 0) then
  VarNames(1) = VarName
else
  do i = 1, size(Levels)
    write (VarNames(i),'(A,I0)') VarName, Levels(i)
  end do
end if
end function opsinputs_fill_varnames_with_levels


! ------------------------------------------------------------------------------

!> Return an array containing the names of JEDI variables storing individual channels of the
!> variable \p VarName. If the list of channels is empty, this means the variable in question is
!> 1D and hence the returned array contains just the single string \p VarName.
function opsinputs_fill_varnames_with_channels(VarName, Channels) result(VarNames)
implicit none
! Subroutine arguments:
character(len=*), intent(in)                   :: VarName
integer(c_int), intent(in)                     :: Channels(:)

! Local declarations:
character(len=max_varname_with_channel_length) :: VarNames(max(size(Channels), 1))
integer                                        :: i

if (size(Channels) == 0) then
  VarNames(1) = VarName
else
  do i = 1, size(Channels)
    write (VarNames(i),'(A,"_",I0)') VarName, Channels(i)
  end do
end if
end function opsinputs_fill_varnames_with_channels


! ------------------------------------------------------------------------------
!> Set the PGEFinal member of an Element_Type variable.
!>
!> \param[in] PGE
!>   PGE value to set.
!> \param[in] MissingDouble
!>   JEDI missing value indicator for doubles.
!> \param[in] PackPGEs
!>   If true, the PGE will be stored in packed form.
!> \param[inout] Element
!>   Variable whose PGEFinal member should be set.
subroutine opsinputs_fill_setpgefinal(PGE, MissingDouble, PackPGEs, Element)
implicit none

! Subroutine arguments:
real(kind=c_double), intent(in)   :: PGE
real(kind=c_double), intent(in)   :: MissingDouble
logical, intent(in)               :: PackPGEs
type(Element_type), intent(inout) :: Element

! Body:
if (PGE /= MissingDouble) then
  Element % PGEFinal = PGE
else
  Element % PGEFinal = PGEMDI
end if

! Pack the PGE
Element % PGEFinal = Element % PGEFinal * PPF

! Ops_VarobPGEs will chop off the fractional part. To reduce the error (and avoid having to take
! the truncation error into account when preparing known good outputs for tests), round the number
! first.
Element % PGEFinal = NINT(Element % PGEFinal)

if (.not. PackPGEs) then
  ! Unpack the PGE
  Element % PGEFinal = Element % PGEFinal / PPF
end if

end subroutine opsinputs_fill_setpgefinal

end module opsinputs_fill_mod
