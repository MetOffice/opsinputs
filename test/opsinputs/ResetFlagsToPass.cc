/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details.
 */

#include "../../test/opsinputs/ResetFlagsToPass.h"

#include "eckit/config/Configuration.h"

#include "ioda/ObsDataVector.h"
#include "ioda/ObsSpace.h"
#include "oops/base/Variables.h"
#include "oops/util/IntSetParser.h"  // for contains()
#include "oops/util/Logger.h"
#include "ufo/filters/QCflags.h"

namespace opsinputs {
namespace test {

ResetFlagsToPass::ResetFlagsToPass(ioda::ObsSpace & obsdb, const Parameters_ & params,
                                   std::shared_ptr<ioda::ObsDataVector<int> > flags,
                                   std::shared_ptr<ioda::ObsDataVector<float> > /*obsErrors*/)
  : obsdb_(obsdb), geovars_(), flags_(std::move(flags)), parameters_(params)
{
  oops::Log::trace() << "ResetFlagsToPass constructor starting" << std::endl;

  flagsToReset_.insert(parameters_.flagsToReset.value().begin(),
                       parameters_.flagsToReset.value().end());
}

ResetFlagsToPass::~ResetFlagsToPass() {
  oops::Log::trace() << "ResetFlagsToPass destructor starting" << std::endl;
}

void ResetFlagsToPass::postFilter(const ioda::ObsVector &, const ufo::ObsDiagnostics &) {
  oops::Log::trace() << "ResetFlagsToPass postFilter" << std::endl;
  for (size_t v = 0; v < flags_->nvars(); ++v) {
    ioda::ObsDataRow<int> &varflags = (*flags_)[v];
    for (size_t i = 0; i < flags_->nlocs(); ++i)
      if (oops::contains(flagsToReset_, varflags[i]))
        varflags[i] = ufo::QCflags::pass;
  }
}

void ResetFlagsToPass::print(std::ostream & os) const {
  os << "ResetFlagsToPass: config = " << parameters_ << std::endl;
}

}  // namespace test
}  // namespace opsinputs

