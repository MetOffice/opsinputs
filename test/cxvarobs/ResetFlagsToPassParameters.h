/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef TEST_CXVAROBS_RESETFLAGSTOPASSPARAMETERS_H_
#define TEST_CXVAROBS_RESETFLAGSTOPASSPARAMETERS_H_

#include <vector>

#include "oops/util/parameters/Parameter.h"
#include "oops/util/parameters/Parameters.h"

namespace cxvarobs {

/// \brief ResetFlagsToPass filter's options.
class ResetFlagsToPassParameters : public oops::Parameters {
 public:
  /// \brief QC flags to replace with "pass".
  oops::Parameter<std::vector<int>> flagsToReset{"flags_to_reset", {}, this};
};

}  // namespace cxvarobs

#endif  // TEST_CXVAROBS_RESETFLAGSTOPASSPARAMETERS_H_
