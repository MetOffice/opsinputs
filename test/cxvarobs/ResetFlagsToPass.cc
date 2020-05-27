/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "../cxvarobs/ResetFlagsToPass.h"

#include <algorithm>
#include <cmath>
#include <functional>
#include <string>
#include <utility>
#include <vector>

#include "eckit/config/Configuration.h"
#include "ioda/ObsDataVector.h"
#include "ioda/ObsSpace.h"
#include "oops/base/Variables.h"
#include "oops/util/DateTime.h"
#include "oops/util/Duration.h"
#include "oops/util/Logger.h"
#include "ufo/utils/RecursiveSplitter.h"

namespace cxvarobs {
namespace test {

ResetFlagsToPass::ResetFlagsToPass(ioda::ObsSpace & obsdb, const eckit::Configuration & config,
                                   boost::shared_ptr<ioda::ObsDataVector<int> > flags,
                                   boost::shared_ptr<ioda::ObsDataVector<float> > obserr)
  : ufo::FilterBase(obsdb, config, flags, obserr)
{
  oops::Log::debug() << "ResetFlagsToPass: config = " << config_ << std::endl;
}

void ResetFlagsToPass::applyFilter(const std::vector<bool> & apply,
                                   const ufo::Variables & filtervars,
                                   std::vector<std::vector<bool>> &/*flagged*/) const {
  for (size_t jv = 0; jv < filtervars.nvars(); ++jv) {
    const size_t iv = flags_->varnames().find(filtervars_.variable(jv).variable());
    for (size_t jobs = 0; jobs < flags_->nlocs(); ++jobs)
      if (apply[jobs])
        (*flags_)[iv][jobs] = ufo::QCflags::pass;
  }
}

void ResetFlagsToPass::print(std::ostream & os) const {
  os << "ResetFlagsToPass: config = " << config_ << std::endl;
}

}  // namespace test
}  // namespace cxvarobs
