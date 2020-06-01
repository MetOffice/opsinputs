/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef TEST_CXVAROBS_RESETFLAGSTOPASS_H_
#define TEST_CXVAROBS_RESETFLAGSTOPASS_H_

#include <ostream>
#include <set>
#include <string>

#include <boost/shared_ptr.hpp>

#include "../cxvarobs/ResetFlagsToPassParameters.h"
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

namespace cxvarobs {

namespace test {

/// \brief Resets observation QC flags to 'pass'.
///
/// See ResetFlagsToPassParameters for the available options.
class ResetFlagsToPass : public util::Printable, private util::ObjectCounter<ResetFlagsToPass> {
 public:
  static const std::string classname() {return "cxvarobs::test::ResetFlagsToPass";}

  ResetFlagsToPass(ioda::ObsSpace &, const eckit::Configuration &,
                   boost::shared_ptr<ioda::ObsDataVector<int> > flags,
                   boost::shared_ptr<ioda::ObsDataVector<float> > obsErrors);
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
  boost::shared_ptr<ioda::ObsDataVector<int>> flags_;

  std::set<int> flagsToReset_;
};

}  // namespace test
}  // namespace cxvarobs

#endif  // TEST_CXVAROBS_RESETFLAGSTOPASS_H_
