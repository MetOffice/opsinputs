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

}

}  // namespace opsinputs

#endif  // OPSINPUTS_OPSINPUTS_OBSSPACE_F_H_
