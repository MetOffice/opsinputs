/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details.
 */

#include "opsinputs/opsinputs_obsspace_f.h"

#include <algorithm>
#include <cstring>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"

#include "ioda/ObsSpace.h"

#include "oops/util/DateTime.h"
#include "oops/util/Duration.h"

namespace opsinputs {

void opsinputs_obsspace_get_db_datetime_offset_in_seconds_f(
    const ioda::ObsSpace &obsspace, const char *group, const char *vname,
    const util::DateTime &reference, const std::size_t &length, int64_t *offsets) {
  if (std::string(group) == "VarMetaData")
    ASSERT(length >= obsspace.nvars());
  else
    ASSERT(length >= obsspace.nlocs());

  std::vector<util::DateTime> datetimes(length, util::DateTime("0000-01-01T00:00:00Z"));
  obsspace.get_db(std::string(group), std::string(vname), datetimes);

  for (std::size_t i = 0; i < length; i++) {
    offsets[i] = (datetimes[i] - reference).toSeconds();
  }
}

void opsinputs_obsspace_get_db_string_f(
    const ioda::ObsSpace &obsspace, const char *group, const char *vname,
    const size_t &string_length, const size_t &num_strings, char *characters) {
  if (std::string(group) == "VarMetaData")
    ASSERT(num_strings >= obsspace.nvars());
  else
    ASSERT(num_strings >= obsspace.nlocs());

  const size_t output_string_length = string_length;

  std::vector<std::string> strings(num_strings);
  obsspace.get_db(std::string(group), std::string(vname), strings);

  for (size_t string_index = 0; string_index < num_strings; ++string_index) {
    const std::string &input_string = strings[string_index];
    const size_t input_string_length = input_string.size();
    size_t char_index = 0;
    char *outputString = characters + string_index *output_string_length;
    for (; char_index < input_string_length &&char_index < output_string_length; ++char_index)
      outputString[char_index] = input_string[char_index];
    for (; char_index < output_string_length; ++char_index)
      outputString[char_index] = ' ';
  }
}

void opsinputs_obsspace_get_locs_ordered_by_record_f(
    const ioda::ObsSpace &obsspace,
    const int32_t &/*num_locs_ordered*/,
    int32_t *locs_ordered,
    const int32_t &/*num_record_starts*/,
    int32_t *record_starts) {
  size_t iloc = 0;
  size_t irec = 0;
  for (ioda::ObsSpace::RecIdxIter it = obsspace.recidx_begin(), end = obsspace.recidx_end();
       it != end; ++it) {
    const std::vector<size_t> & locsInRec = it->second;
    record_starts[irec++] = iloc + 1;  // +1 produces a 1-based index
    for (size_t loc : locsInRec) {
      locs_ordered[iloc++] = loc + 1;  // same
    }
  }
  record_starts[irec++] = iloc + 1;
}


}  // namespace opsinputs
