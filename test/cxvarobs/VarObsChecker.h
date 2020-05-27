/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef TEST_CXVAROBS_VAROBSCHECKER_H_
#define TEST_CXVAROBS_VAROBSCHECKER_H_

#include <ostream>
#include <string>

#include "boost/shared_ptr.hpp"

#include "ioda/ObsDataVector.h"
#include "oops/base/Variables.h"
#include "oops/util/ObjectCounter.h"
#include "oops/util/Printable.h"
#include "../cxvarobs/VarObsCheckerParameters.h"

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

class LocalEnvironment;

namespace test {

class VarObsChecker : public util::Printable, private util::ObjectCounter<VarObsChecker> {
 public:
  static const std::string classname() {return "ufo::VarObsChecker";}

  VarObsChecker(ioda::ObsSpace &, const eckit::Configuration &,
                boost::shared_ptr<ioda::ObsDataVector<int> > flags,
                boost::shared_ptr<ioda::ObsDataVector<float> > obsErrors);
  ~VarObsChecker();

  void preProcess() const {}
  void priorFilter(const ufo::GeoVaLs &) const;
  void postFilter(const ioda::ObsVector &, const ufo::ObsDiagnostics &) const;

  const oops::Variables & requiredVars() const {return geovars_;}
  const oops::Variables & requiredHdiagnostics() const {return extradiagvars_;}

 private:
  class PrintVarObsOutput;
  class MainTable;

  void print(std::ostream &) const;

  void setupEnvironment(cxvarobs::LocalEnvironment &localEnvironment) const;

  PrintVarObsOutput parsePrintVarObsOutput(const std::string &fileName) const;

  void checkHeader(const std::map<std::string, std::string> &headerFields) const;

  void checkMainTable(const MainTable &mainTable) const;

  ioda::ObsSpace & obsdb_;
  oops::Variables geovars_;
  oops::Variables extradiagvars_;
  boost::shared_ptr<ioda::ObsDataVector<int>> flags_;
  boost::shared_ptr<ioda::ObsDataVector<float>> obsErrors_;

  VarObsCheckerParameters parameters_;
};

}  // namespace test
}  // namespace cxvarobs

#endif  // TEST_CXVAROBS_VAROBSCHECKER_H_
