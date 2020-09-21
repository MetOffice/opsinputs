/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details.
 */

#ifndef TEST_OPSINPUTS_RESETFLAGSTOPASS_H_
#define TEST_OPSINPUTS_RESETFLAGSTOPASS_H_

#include <memory>
#include <ostream>
#include <set>
#include <string>

#include "../opsinputs/ResetFlagsToPassParameters.h"
#include "ioda/ObsDataVector.h"
#include "oops/base/Variables.h"
#include "oops/util/ObjectCounter.h"
#include "oops/util/Printable.h"

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
class ResetFlagsToPass : public util::Printable, private util::ObjectCounter<ResetFlagsToPass> {
 public:
  static const std::string classname() {return "opsinputs::test::ResetFlagsToPass";}

  ResetFlagsToPass(ioda::ObsSpace &, const eckit::Configuration &,
                   std::shared_ptr<ioda::ObsDataVector<int> > flags,
                   std::shared_ptr<ioda::ObsDataVector<float> > obsErrors);
  ~ResetFlagsToPass();

  void preProcess() const {}
  void priorFilter(const ufo::GeoVaLs &) const {}
  void postFilter(const ioda::ObsVector &, const ufo::ObsDiagnostics &) const;

  const oops::Variables & requiredVars() const {return geovars_;}
  const oops::Variables & requiredHdiagnostics() const {return extradiagvars_;}

 private:
  void print(std::ostream &) const;

  ioda::ObsSpace & obsdb_;
  oops::Variables geovars_;
  oops::Variables extradiagvars_;
  std::shared_ptr<ioda::ObsDataVector<int>> flags_;

  std::set<int> flagsToReset_;
};

}  // namespace test
}  // namespace opsinputs

#endif  // TEST_OPSINPUTS_RESETFLAGSTOPASS_H_
