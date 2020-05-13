/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "cxvarobs/cxvarobs_obsspace_f.h"

#include <algorithm>
#include <cstring>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"

#include "ioda/ObsSpace.h"

#include "oops/util/DateTime.h"
#include "oops/util/Duration.h"

namespace cxvarobs {

void cxvarobs_obsspace_get_db_datetime_offset_in_seconds_f(
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
  }
}

}  // namespace cxvarobs
