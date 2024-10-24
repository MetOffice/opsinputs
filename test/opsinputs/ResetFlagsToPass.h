/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 */

#ifndef TEST_OPSINPUTS_RESETFLAGSTOPASS_H_
#define TEST_OPSINPUTS_RESETFLAGSTOPASS_H_

#include <memory>
#include <ostream>
#include <set>
#include <string>

#include "../opsinputs/ResetFlagsToPassParameters.h"
#include "ioda/ObsDataVector.h"
#include "oops/base/ObsVariables.h"
#include "oops/base/Variables.h"
#include "oops/util/ObjectCounter.h"
#include "ufo/ObsFilterBase.h"
#include "ufo/ObsTraits.h"

namespace eckit {
  class Configuration;
}

namespace ioda {
  class ObsSpace;
  class ObsVector;
}

namespace ufo {
  class GeoVaLs;
  class ObsDiagnostics;
}

namespace opsinputs {

namespace test {

/// \brief Resets observation QC flags to 'pass'.
///
/// See ResetFlagsToPassParameters for the available options.
class ResetFlagsToPass : public ufo::ObsFilterBase,
                         private util::ObjectCounter<ResetFlagsToPass> {
 public:
  static const std::string classname() {return "opsinputs::test::ResetFlagsToPass";}

  /// The type of parameters accepted by the constructor of this filter.
  /// This typedef is used by the FilterFactory.
  typedef ResetFlagsToPassParameters Parameters_;

  ResetFlagsToPass(ioda::ObsSpace &, const Parameters_ &,
                   std::shared_ptr<ioda::ObsDataVector<int> > flags,
                   std::shared_ptr<ioda::ObsDataVector<float> > obsErrors);
  ~ResetFlagsToPass();

  void preProcess() override {}
  void priorFilter(const ufo::GeoVaLs &) override {}
  void postFilter(const ufo::GeoVaLs &,
                  const ioda::ObsVector &hofx,
                  const ioda::ObsVector &bias,
                  const ufo::ObsDiagnostics &) override;
  void checkFilterData(const ufo::FilterStage filterStage) override {}

  oops::Variables requiredVars() const override {return geovars_;}
  oops::ObsVariables requiredHdiagnostics() const override {return extradiagvars_;}

 private:
  void print(std::ostream &) const override;

  ioda::ObsSpace & obsdb_;
  oops::Variables geovars_;
  oops::ObsVariables extradiagvars_;
  std::shared_ptr<ioda::ObsDataVector<int>> flags_;

  ResetFlagsToPassParameters parameters_;

  std::set<int> flagsToReset_;
};

}  // namespace test
}  // namespace opsinputs

#endif  // TEST_OPSINPUTS_RESETFLAGSTOPASS_H_
