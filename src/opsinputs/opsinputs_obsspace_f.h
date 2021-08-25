/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details.
 */

#ifndef OPSINPUTS_OPSINPUTS_OBSSPACE_F_H_
#define OPSINPUTS_OPSINPUTS_OBSSPACE_F_H_

#include <cstddef>
#include <cstdint>

/// \file Extensions to the Fortran-callable interface to ioda::ObsSpace.

namespace ioda {
  class ObsSpace;
}  // namespace ioda

namespace util {
  class DateTime;
}  // namespace util

namespace opsinputs {

extern "C" {

  void opsinputs_obsspace_get_db_datetime_offset_in_seconds_f(
      const ioda::ObsSpace &obsspace, const char *group, const char *vname,
      const util::DateTime &reference, const size_t &length, int64_t *offsets);

  void opsinputs_obsspace_get_db_string_f(
      const ioda::ObsSpace &obsspace, const char *group, const char *vname,
      const size_t &string_length, const size_t &num_strings, char *characters);

  /// Order location indices first by record index and then the sorting variable defined for the
  /// ObsSpace
  ///
  /// \param[in] obsspace
  ///   The ObsSpace.
  /// \param[in] num_locs_ordered
  ///   Length of the `locs_ordered` array. Should be equal to `obsspace.nlocs()`.
  /// \param[inout] locs_ordered
  ///   An array that will be filled with ordered (1-based) location indices.
  /// \param[in] num_record_starts
  ///   Length of the `record_starts` array. Should be equal to `obsspace.nrecs() + 1`.
  /// \param[inout] record_starts
  ///   An array that will be filled with (1-based) indices of the elements of `locs_ordered`
  ///   storing the first location of each record. The last element will be set to
  ///   `num_locs_ordered + 1`.
  void opsinputs_obsspace_get_locs_ordered_by_record_f(
      const ioda::ObsSpace &obsspace,
      const int32_t &num_locs_ordered,
      int32_t *locs_ordered,
      const int32_t &num_record_starts,
      int32_t *record_starts);

}

}  // namespace opsinputs

#endif  // OPSINPUTS_OPSINPUTS_OBSSPACE_F_H_
