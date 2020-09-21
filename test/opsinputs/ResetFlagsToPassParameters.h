/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details.
 */

#ifndef TEST_OPSINPUTS_RESETFLAGSTOPASSPARAMETERS_H_
#define TEST_OPSINPUTS_RESETFLAGSTOPASSPARAMETERS_H_

#include <vector>

#include "oops/util/parameters/Parameter.h"
#include "oops/util/parameters/Parameters.h"

namespace opsinputs {

/// \brief ResetFlagsToPass filter's options.
class ResetFlagsToPassParameters : public oops::Parameters {
  OOPS_CONCRETE_PARAMETERS(ResetFlagsToPassParameters, Parameters)

 public:
  /// \brief List of QC flags (elements of ufo::QCflags) to be replaced with "pass".
  oops::Parameter<std::vector<int>> flagsToReset{"flags_to_reset", {}, this};
};

}  // namespace opsinputs

#endif  // TEST_OPSINPUTS_RESETFLAGSTOPASSPARAMETERS_H_
