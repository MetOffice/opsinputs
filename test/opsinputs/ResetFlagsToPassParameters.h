/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 */

#ifndef TEST_OPSINPUTS_RESETFLAGSTOPASSPARAMETERS_H_
#define TEST_OPSINPUTS_RESETFLAGSTOPASSPARAMETERS_H_

#include <vector>

#include "oops/util/parameters/Parameter.h"
#include "ufo/ObsFilterParametersBase.h"

namespace opsinputs {

/// \brief ResetFlagsToPass filter's options.
class ResetFlagsToPassParameters : public ufo::ObsFilterParametersBase {
  OOPS_CONCRETE_PARAMETERS(ResetFlagsToPassParameters, ObsFilterParametersBase)

 public:
  /// \brief List of QC flags (elements of ufo::QCflags) to be replaced with "pass".
  oops::Parameter<std::vector<int>> flagsToReset{"flags_to_reset", {}, this};
};

}  // namespace opsinputs

#endif  // TEST_OPSINPUTS_RESETFLAGSTOPASSPARAMETERS_H_
