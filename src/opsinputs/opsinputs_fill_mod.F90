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
use opsinputs_jediopsobs_mod, only: &
    opsinputs_jediopsobs
use opsinputs_utils_mod, only: &
    max_varname_with_channel_length

use GenMod_Core, only: &
    gen_warn,          &
    gen_fail

use OpsMod_Constants, only: PPF ! PGE packing factor
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
          opsinputs_fill_fillreal2dfrom1dgeovalwithchans, &
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
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillelementtypefromsimulatedvariable( &
  Hdr, OpsVarName, NumObs, El1, ObsSpace, Flags, ObsErrors, JediVarName)
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

! Local declarations:
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
    Flag = 0 ! assume all observations passed QC
  end if
  ! - observation error
  if (opsinputs_obsdatavector_float_has(ObsErrors, JediVarName)) then
    call opsinputs_obsdatavector_float_get(ObsErrors, JediVarName, ObsError)
  else
    write (ErrorMessage, '(A,A,A)') "Variable ", JediVarName, "@ObsError not found"
    call gen_warn(RoutineName, ErrorMessage)
    ObsError = MissingFloat
  end if
  ! - gross error probability
  if (obsspace_has(ObsSpace, "GrossErrorProbability", JediVarName)) then
    call obsspace_get_db(ObsSpace, "GrossErrorProbability", JediVarName, PGE)
  else
    PGE = MissingDouble
  end if

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, El1)
  do i = 1, NumObs
    if (ObsValue(i) /= MissingDouble) El1(i) % Value = ObsValue(i)
    if (ObsError(i) /= MissingFloat)  El1(i) % OBErr = ObsError(i)
    if (PGE(i) /= MissingDouble)      El1(i) % PGEFinal = PGE(i) * PPF
    if (Flag(i) /= 0)                 El1(i) % Flags = ibset(0, FinalRejectFlag)
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
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillelementtype2dfromsimulatedvariable_norecords( &
  Hdr, OpsVarName, NumObs, El2, ObsSpace, Channels, Flags, ObsErrors, JediVarName)
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

! Local declarations:
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
      Flag = 0 ! assume all observations passed QC
    end if
    ! - observation error
    if (opsinputs_obsdatavector_float_has(ObsErrors, JediVarNamesWithChannels(iChannel))) then
      call opsinputs_obsdatavector_float_get(ObsErrors, JediVarNamesWithChannels(iChannel), ObsError)
    else
      write (ErrorMessage, '(A,A,A)') &
        "Warning: variable ", JediVarNamesWithChannels(iChannel), "@ObsError not found"
      call gen_warn(RoutineName, ErrorMessage)
      ObsError = MissingFloat
    end if
    ! - gross error probability
    if (obsspace_has(ObsSpace, "GrossErrorProbability", JediVarNamesWithChannels(iChannel))) then
      call obsspace_get_db(ObsSpace, "GrossErrorProbability", JediVarNamesWithChannels(iChannel), &
                           PGE)
    else
      PGE = MissingDouble
    end if

    ! Fill the OPS data structures
    do iObs = 1, NumObs
      if (ObsValue(iObs) /= MissingDouble) El2(iObs, iChannel) % Value = ObsValue(iObs)
      if (ObsError(iObs) /= MissingFloat)  El2(iObs, iChannel) % OBErr = ObsError(iObs)
      if (PGE(iObs) /= MissingDouble)      El2(iObs, iChannel) % PGEFinal = PGE(iObs) * PPF
      if (Flag(iObs) /= 0)                 El2(iObs, iChannel) % Flags = ibset(0, FinalRejectFlag)
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
!> \param[in] JediOpsObs
!>   Numbers of JEDI/OPS observations and their partitioning into records.
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
  Hdr, OpsVarName, JediOpsObs, El2, ObsSpace, Flags, ObsErrors, JediVarName)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
type(opsinputs_jediopsobs), intent(in)          :: JediOpsObs
type(Element_type), pointer                     :: El2(:,:)
type(c_ptr), value, intent(in)                  :: ObsSpace
type(c_ptr), value, intent(in)                  :: Flags
type(c_ptr), value, intent(in)                  :: ObsErrors
character(len=*), intent(in)                    :: JediVarName

! Local declarations:
real(kind=c_double)                             :: ObsValue(JediOpsObs % NumJediObs)
integer(kind=c_int)                             :: Flag(JediOpsObs % NumJediObs)
real(kind=c_float)                              :: ObsError(JediOpsObs % NumJediObs)
real(kind=c_double)                             :: PGE(JediOpsObs % NumJediObs)
real(kind=c_double)                             :: MissingDouble
real(kind=c_float)                              :: MissingFloat
integer                                         :: iObs, iLevel, iJediObs, numLevels
character(len=*), parameter                     :: &
  RoutineName = "opsinputs_fill_fillelementtype2dfromsimulatedvariable_records"
character(len=256)                              :: ErrorMessage

! Body:

! The types of floating-point numbers used in this function are a bit confusing. OPS stores
! observation values as doubles, whereas JEDI stores them as floats. However, the Fortran interface
! to the IODA ObsSpace is only partially implemented: obsspace_get_db_real32 doesn't work, only
! obsspace_get_db_real64 does. So we need to retrieve observation values as doubles. Observation
! errors, though, are retrieved as floats.

MissingDouble = missing_value(0.0_c_double)
MissingFloat  = missing_value(0.0_c_float)

if (obsspace_has(ObsSpace, "ObsValue", JediVarName)) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, JediOpsObs % NumOpsObs, El2, &
                 num_levels = int(JediOpsObs % MaxNumLevelsPerObs, kind = integer64))

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
    Flag = 0 ! assume all observations passed QC
  end if
  ! - observation error
  if (opsinputs_obsdatavector_float_has(ObsErrors, JediVarName)) then
    call opsinputs_obsdatavector_float_get(ObsErrors, JediVarName, ObsError)
  else
    write (ErrorMessage, '(A,A,A)') &
      "Warning: variable ", JediVarName, "@ObsError not found"
    call gen_warn(RoutineName, ErrorMessage)
    ObsError = MissingFloat
  end if
  ! - gross error probability
  if (obsspace_has(ObsSpace, "GrossErrorProbability", JediVarName)) then
    call obsspace_get_db(ObsSpace, "GrossErrorProbability", JediVarName, &
                         PGE)
  else
    PGE = MissingDouble
  end if

  ! Fill the OPS data structures
  do iObs = 1, JediOpsObs % NumOpsObs
    numLevels = JediOpsObs % RecordStarts(iObs + 1) - JediOpsObs % RecordStarts(iObs)
    do iLevel = 1, numLevels
      iJediObs = JediOpsObs % LocationsOrderedByRecord( &
        JediOpsObs % RecordStarts(iObs) + (iLevel - 1))
      if (ObsValue(iJediObs) /= MissingDouble) then
        El2(iObs, iLevel) % Value = ObsValue(iJediObs)
      end if
      if (ObsError(iJediObs) /= MissingFloat) then
        El2(iObs, iLevel) % OBErr = ObsError(iJediObs)
      end if
      if (PGE(iJediObs) /= MissingDouble) then
        El2(iObs, iLevel) % PGEFinal = PGE(iJediObs) * PPF
      end if
      if (Flag(iJediObs) /= 0) then
        El2(iObs, iLevel) % Flags = ibset(0, FinalRejectFlag)
      end if
    end do
    El2(iObs, numLevels + 1 : JediOpsObs % MaxNumLevelsPerObs) % Flags = ibset(0, FinalRejectFlag)
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
!> \param[in] JediOpsObs
!>   Numbers of JEDI/OPS observations and (optionally) their partitioning into records.
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
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillelementtype2dfromsimulatedvariable( &
  Hdr, OpsVarName, JediOpsObs, El2, ObsSpace, Channels, Flags, ObsErrors, JediVarName)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
type(opsinputs_jediopsobs), intent(in)          :: JediOpsObs
type(Element_type), pointer                     :: El2(:,:)
type(c_ptr), value, intent(in)                  :: ObsSpace
integer(c_int), intent(in)                      :: Channels(:)
type(c_ptr), value, intent(in)                  :: Flags
type(c_ptr), value, intent(in)                  :: ObsErrors
character(len=*), intent(in)                    :: JediVarName

! Body:

if (JediOpsObs % ConvertRecordsToMultilevelObs) then
  call opsinputs_fill_fillelementtype2dfromsimulatedvariable_records( &
    Hdr, OpsVarName, JediOpsObs, El2, ObsSpace, Flags, ObsErrors, JediVarName)
else
  call opsinputs_fill_fillelementtype2dfromsimulatedvariable_norecords( &
    Hdr, OpsVarName, JediOpsObs % NumOpsObs, El2, ObsSpace, Channels, Flags, ObsErrors, JediVarName)
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
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillelementtypefromnormalvariable( &
  Hdr, OpsVarName, NumObs, El1, ObsSpace, &
  JediValueVarName, JediValueGroup, JediErrorVarName, JediErrorGroup)
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

! Local declarations:
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
      ObsError = MissingDouble
    end if
  else
    ObsError = MissingDouble
  end if

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, NumObs, El1)
  do i = 1, NumObs
    if (ObsValue(i) /= MissingDouble) El1(i) % Value = ObsValue(i)
    if (ObsError(i) /= MissingDouble)  El1(i) % OBErr = ObsError(i)
    ! We could also fill Flags and PGEFinal if these quantities were available in separate JEDI
    ! variables. At present, however, we don't even have a use case where there is a separate
    ! variable storing the observation error.
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
!>
!> \note This function returns early (without a warning) if the specified JEDI variable is not found.
!> We rely on warnings printed by the OPS code whenever data needed to output a requested varfield
!> are not found.
subroutine opsinputs_fill_fillelementtype2dfromnormalvariable( &
  Hdr, OpsVarName, NumObs, El2, ObsSpace, Channels, &
  JediValueVarName, JediValueGroup, JediErrorVarName, JediErrorGroup)
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

! Local declarations:
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

MissingDouble = missing_value(0.0_c_double)

JediValueVarNamesWithChannels = opsinputs_fill_varnames_with_channels( &
  JediValueVarName, Channels)
if (present(JediErrorVarName)) then
  JediErrorVarNamesWithChannels = opsinputs_fill_varnames_with_channels( &
    JediErrorVarName, Channels)
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
        ObsError = MissingDouble
      end if
    else
      ObsError = MissingDouble
    end if

    ! Fill the OPS data structures
    do iObs = 1, NumObs
      if (ObsValue(iObs) /= MissingDouble) El2(iObs, iChannel) % Value = ObsValue(iObs)
      if (ObsError(iObs) /= MissingDouble) El2(iObs, iChannel) % OBErr = ObsError(iObs)
      ! We could also fill Flags and PGEFinal if these quantities were available in separate JEDI
      ! variables. At present, however, we don't even have a use case where there is a separate
      ! variable storing the observation error.
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
!> \param[in] JediOpsObs
!>   Numbers of JEDI/OPS observations and optionally their partitioning into records.
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
  Hdr, OpsVarName, JediOpsObs, Real1, ObsSpace, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
type(opsinputs_jediopsobs), intent(in)          :: JediOpsObs
real(real64), pointer                           :: Real1(:)
type(c_ptr), value, intent(in)                  :: ObsSpace
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup

! Local declarations:
real(kind=c_double)                             :: VarValue(JediOpsObs % NumJediObs)
real(kind=c_double)                             :: CurrentVarValue
real(kind=c_double)                             :: MissingDouble
integer                                         :: i

! Body:

MissingDouble = missing_value(0.0_c_double)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarName)) then
  ! Retrieve data from JEDI
  call obsspace_get_db(ObsSpace, JediVarGroup, JediVarName, VarValue)

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, JediOpsObs % NumOpsObs, Real1)
  do i = 1, JediOpsObs % NumOpsObs
    if (JediOpsObs % ConvertRecordsToMultilevelObs) then
      if (JediOpsObs % RecordStarts(i + 1) > JediOpsObs % RecordStarts(i)) then
        ! This record is non-empty. Use the first location from that record.
        CurrentVarValue = VarValue(JediOpsObs % LocationsOrderedByRecord(JediOpsObs % RecordStarts(i)))
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
real(real64), pointer                           :: Real2(:,:)
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
!> \param[in] JediOpsObs
!>   Numbers of JEDI/OPS observations and (optionally) their partitioning into records.
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
  Hdr, OpsVarName, JediOpsObs, Real2, ObsSpace, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
type(opsinputs_jediopsobs), intent(in)          :: JediOpsObs
real(real64), pointer                           :: Real2(:,:)
type(c_ptr), value, intent(in)                  :: ObsSpace
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup

! Local declarations:
real(kind=c_double)                             :: VarValue(JediOpsObs % NumJediObs)
real(kind=c_double)                             :: MissingDouble
integer                                         :: iObs, iLevel, iJediObs, numLevels

! Body:

MissingDouble = missing_value(0.0_c_double)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarName)) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, JediOpsObs % NumOpsObs, Real2, &
                 num_levels = int(JediOpsObs % MaxNumLevelsPerObs, kind = integer64))
  ! Retrieve data from JEDI
  call obsspace_get_db(ObsSpace, JediVarGroup, JediVarName, VarValue)

  ! Fill the OPS data structures
  do iObs = 1, JediOpsObs % NumOpsObs
    numLevels = JediOpsObs % RecordStarts(iObs + 1) - JediOpsObs % RecordStarts(iObs)
    do iLevel = 1, numLevels
      iJediObs = JediOpsObs % LocationsOrderedByRecord( &
        JediOpsObs % RecordStarts(iObs) + (iLevel - 1))
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
!> \param[in] JediOpsObs
!>   Numbers of JEDI/OPS observations and (optionally) their partitioning into records.
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
  Hdr, OpsVarName, JediOpsObs, Real2, ObsSpace, Channels, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
type(opsinputs_jediopsobs), intent(in)          :: JediOpsObs
real(real64), pointer                           :: Real2(:,:)
type(c_ptr), value, intent(in)                  :: ObsSpace
integer(c_int), intent(in)                      :: Channels(:)
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup

! Body:

if (JediOpsObs % ConvertRecordsToMultilevelObs) then
  call opsinputs_fill_fillreal2d_records( &
    Hdr, OpsVarName, JediOpsObs, Real2, ObsSpace, JediVarName, JediVarGroup)
else
  call opsinputs_fill_fillreal2d_norecords( &
    Hdr, OpsVarName, JediOpsObs % NumOpsObs, Real2, ObsSpace, Channels, JediVarName, JediVarGroup)
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
real(real64), pointer                           :: Real1(:)
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
real(real64), pointer                           :: Real2(:,:)
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
real(real64), pointer                           :: Real2(:,:)
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
!> \param[in] JediOpsObs
!>   Numbers of JEDI/OPS observations and optionally their partitioning into records.
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
  Hdr, OpsVarName, JediOpsObs, Int1, ObsSpace, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
type(opsinputs_jediopsobs), intent(in)          :: JediOpsObs
integer(integer64), pointer                     :: Int1(:)
type(c_ptr), value, intent(in)                  :: ObsSpace
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup

! Local declarations:
integer(kind=4)                                 :: VarValue(JediOpsObs % NumJediObs)
integer(kind=4)                                 :: CurrentVarValue
integer(kind=4)                                 :: MissingInt
integer                                         :: i

! Body:

MissingInt = missing_value(0_c_int32_t)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarName)) then
  ! Retrieve data from JEDI
  call obsspace_get_db(ObsSpace, JediVarGroup, JediVarName, VarValue)

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, JediOpsObs % NumOpsObs, Int1)
  do i = 1, JediOpsObs % NumOpsObs
    if (JediOpsObs % ConvertRecordsToMultilevelObs) then
      if (JediOpsObs % RecordStarts(i + 1) > JediOpsObs % RecordStarts(i)) then
        ! This record is non-empty. Use the first location from that record.
        CurrentVarValue = VarValue(JediOpsObs % LocationsOrderedByRecord(JediOpsObs % RecordStarts(i)))
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
!> \param[in] JediOpsObs
!>   Numbers of JEDI/OPS observations and optionally their partitioning into records.
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
  Hdr, OpsVarName, JediOpsObs, StringLen, String1, ObsSpace, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
type(opsinputs_jediopsobs), intent(in)          :: JediOpsObs
integer(integer64), intent(in)                  :: StringLen
character(len=StringLen), pointer               :: String1(:)
type(c_ptr), value, intent(in)                  :: ObsSpace
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup

! Local declarations:
character(len=StringLen)                        :: VarValue(JediOpsObs % NumJediObs)
integer                                         :: i

! Body:

if (obsspace_has(ObsSpace, JediVarGroup, JediVarName)) then
  ! Retrieve data from JEDI
  call opsinputs_obsspace_get_db_string(ObsSpace, JediVarGroup, JediVarName, &
                                        int(StringLen, kind=4), VarValue)

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, JediOpsObs % NumOpsObs, String1)
  do i = 1, JediOpsObs % NumOpsObs
    if (JediOpsObs % ConvertRecordsToMultilevelObs) then
      if (JediOpsObs % RecordStarts(i + 1) > JediOpsObs % RecordStarts(i)) then
        ! This record is non-empty. Use the first location from that record.
        String1(i) = VarValue(JediOpsObs % LocationsOrderedByRecord(JediOpsObs % RecordStarts(i)))
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
!> \param[in] JediOpsObs
!>   Numbers of JEDI/OPS observations and optionally their partitioning into records.
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
  Hdr, OpsVarName, JediOpsObs, Real1, ObsSpace, JediVarName, JediVarGroup, ReferenceTime)
use datetime_mod, only: datetime
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
type(opsinputs_jediopsobs), intent(in)          :: JediOpsObs
real(real64), pointer                           :: Real1(:)
type(c_ptr), value, intent(in)                  :: ObsSpace
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup
type(datetime), intent(in)                      :: ReferenceTime

! Local declarations:
integer(c_int64_t)                              :: VarValue(JediOpsObs % NumJediObs)
integer                                         :: i

! Body:

if (obsspace_has(ObsSpace, JediVarGroup, JediVarName)) then
  ! Retrieve data from JEDI
  call opsinputs_obsspace_get_db_datetime_offset_in_seconds( &
    ObsSpace, JediVarGroup, JediVarName, ReferenceTime, VarValue)

  ! Fill the OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, JediOpsObs % NumOpsObs, Real1)
  do i = 1, JediOpsObs % NumOpsObs
    if (JediOpsObs % ConvertRecordsToMultilevelObs) then
      if (JediOpsObs % RecordStarts(i + 1) > JediOpsObs % RecordStarts(i)) then
        ! This record is non-empty. Use the first location from that record.
        Real1(i) = VarValue(JediOpsObs % LocationsOrderedByRecord(JediOpsObs % RecordStarts(i)))
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
real(real64), pointer                           :: Real2(:,:)
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
!> \param[in] JediOpsObs
!>   Numbers of JEDI/OPS observations and (optionally) their partitioning into records.
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
  Hdr, OpsVarName, JediOpsObs, Real2, ObsSpace, JediVarName, JediVarGroup, ReferenceTime)
use datetime_mod, only: datetime
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
type(opsinputs_jediopsobs), intent(in)          :: JediOpsObs
real(real64), pointer                           :: Real2(:,:)
type(c_ptr), value, intent(in)                  :: ObsSpace
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup
type(datetime), intent(in)                      :: ReferenceTime

! Local declarations:
integer(c_int64_t)                              :: VarValue(JediOpsObs % NumJediObs)
real(kind=c_double)                             :: MissingDouble
integer                                         :: iObs, iLevel, iJediObs, numLevels

! Body:

MissingDouble = missing_value(0.0_c_double)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarName)) then
  ! Retrieve data from JEDI
  call opsinputs_obsspace_get_db_datetime_offset_in_seconds( &
    ObsSpace, JediVarGroup, JediVarName, ReferenceTime, VarValue)

  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, JediOpsObs % NumOpsObs, Real2, &
                 num_levels = int(JediOpsObs % MaxNumLevelsPerObs, kind = integer64))
  ! Fill the OPS data structures
  do iObs = 1, JediOpsObs % NumOpsObs
    numLevels = JediOpsObs % RecordStarts(iObs + 1) - JediOpsObs % RecordStarts(iObs)
    do iLevel = 1, numLevels
      iJediObs = JediOpsObs % LocationsOrderedByRecord( &
        JediOpsObs % RecordStarts(iObs) + (iLevel - 1))
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
!> \param[in] JediOpsObs
!>   Numbers of JEDI/OPS observations and (optionally) their partitioning into records.
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
  Hdr, OpsVarName, JediOpsObs, Real2, ObsSpace, Channels, JediVarName, JediVarGroup, ReferenceTime)
use datetime_mod, only: datetime
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
type(opsinputs_jediopsobs), intent(in)          :: JediOpsObs
real(real64), pointer                           :: Real2(:,:)
type(c_ptr), value, intent(in)                  :: ObsSpace
integer(c_int), intent(in)                      :: Channels(:)
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup
type(datetime), intent(in)                      :: ReferenceTime

! Body:

if (JediOpsObs % ConvertRecordsToMultilevelObs) then
  call opsinputs_fill_filltimeoffsets2d_records(Hdr, OpsVarName, JediOpsObs, Real2, &
                                                ObsSpace, JediVarName, JediVarGroup, &
                                                ReferenceTime)
else
  call opsinputs_fill_filltimeoffsets2d_norecords(Hdr, OpsVarName, JediOpsObs % NumOpsObs, Real2, &
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
!> \param[in] JediOpsObs
!>   Numbers of JEDI/OPS observations and (optionally) their partitioning into records.
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
  Hdr, OpsVarName, JediOpsObs, Coord2, ObsSpace, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
type(opsinputs_jediopsobs), intent(in)          :: JediOpsObs
type(coord_type), pointer                       :: Coord2(:,:)
type(c_ptr), value, intent(in)                  :: ObsSpace
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup

! Local declarations:
real(kind=c_double)                             :: VarValue(JediOpsObs % NumJediObs)
real(kind=c_double)                             :: MissingDouble
integer                                         :: iObs, iLevel, iJediObs, numLevels

! Body:

MissingDouble = missing_value(0.0_c_double)

if (obsspace_has(ObsSpace, JediVarGroup, JediVarName)) then
  ! Allocate OPS data structures
  call Ops_Alloc(Hdr, OpsVarName, JediOpsObs % NumOpsObs, Coord2, &
                 num_levels = int(JediOpsObs % MaxNumLevelsPerObs, kind = integer64))

  ! Retrieve data from JEDI
  call obsspace_get_db(ObsSpace, JediVarGroup, JediVarName, VarValue)

  ! Fill the OPS data structures
  do iObs = 1, JediOpsObs % NumOpsObs
    numLevels = JediOpsObs % RecordStarts(iObs + 1) - JediOpsObs % RecordStarts(iObs)
    do iLevel = 1, numLevels
      iJediObs = JediOpsObs % LocationsOrderedByRecord( &
        JediOpsObs % RecordStarts(iObs) + (iLevel - 1))
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
!> \param[in] JediOpsObs
!>   Numbers of JEDI/OPS observations and (optionally) their partitioning into records.
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
  Hdr, OpsVarName, JediOpsObs, Coord2, ObsSpace, Channels, JediVarName, JediVarGroup)
implicit none

! Subroutine arguments:
type(ElementHeader_Type), intent(inout)         :: Hdr
character(len=*), intent(in)                    :: OpsVarName
type(opsinputs_jediopsobs), intent(in)          :: JediOpsObs
type(coord_type), pointer                       :: Coord2(:,:)
type(c_ptr), value, intent(in)                  :: ObsSpace
integer(c_int), intent(in)                      :: Channels(:)
character(len=*), intent(in)                    :: JediVarName
character(len=*), intent(in)                    :: JediVarGroup

! Body:

if (JediOpsObs % ConvertRecordsToMultilevelObs) then
  call opsinputs_fill_fillcoord2d_records( &
    Hdr, OpsVarName, JediOpsObs, Coord2, ObsSpace, JediVarName, JediVarGroup)
else
  call opsinputs_fill_fillcoord2d_norecords( &
    Hdr, OpsVarName, JediOpsObs % NumOpsObs, Coord2, ObsSpace, Channels, JediVarName, JediVarGroup)
end if

end subroutine opsinputs_fill_fillcoord2d


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

end module opsinputs_fill_mod
