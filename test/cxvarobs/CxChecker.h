/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef TEST_CXVAROBS_CXCHECKER_H_
#define TEST_CXVAROBS_CXCHECKER_H_

#include <map>
#include <ostream>
#include <string>
#include <vector>

#include <boost/shared_ptr.hpp>

#include "../cxvarobs/CxCheckerParameters.h"
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

class LocalEnvironment;

namespace test {

/// \brief Compares contents of Cx files against reference values specified in the
/// configuration.
///
/// The OpsProg_PrintCxFile.exe OPS utility is used to print the contents of Cx files in textual
/// form. Reference values of individual fields/arrays are then compared against values extracted
/// from that output.
///
/// See CxCheckerParameters for a list of available options.
class CxChecker : public util::Printable, private util::ObjectCounter<CxChecker> {
 public:
  static const std::string classname() {return "cxvarobs::test::CxChecker";}

  CxChecker(ioda::ObsSpace &, const eckit::Configuration &,
                boost::shared_ptr<ioda::ObsDataVector<int> > flags,
                boost::shared_ptr<ioda::ObsDataVector<float> > obsErrors);
  ~CxChecker();

  void preProcess() const {}
  void priorFilter(const ufo::GeoVaLs &) const {}
  void postFilter(const ioda::ObsVector &, const ufo::ObsDiagnostics &) const;

  const oops::Variables & requiredVars() const {return geovars_;}
  const oops::Variables & requiredHdiagnostics() const {return extradiagvars_;}

 private:
  class PrintCxFileOutput;
  class MainTable;

  void print(std::ostream &) const;

  void setupEnvironment(cxvarobs::LocalEnvironment &localEnvironment) const;

  PrintCxFileOutput parsePrintCxFileOutput(const std::string &printCxFileOutput) const;

  void checkHeader(const std::map<std::string, std::string> &headerFields) const;
  void checkLevelDependentConstants(const std::vector<std::string> &etaThetaLevels,
                                    const std::vector<std::string> &etaRhoLevels) const;
  void checkLookup(
      const std::vector<std::map<std::string, std::string>> &lookupFields) const;
  void checkMainTable(
      const std::vector<std::vector<std::vector<std::string>>> &mainTable) const;

  ioda::ObsSpace & obsdb_;
  oops::Variables geovars_;
  oops::Variables extradiagvars_;
  boost::shared_ptr<ioda::ObsDataVector<int>> flags_;
  boost::shared_ptr<ioda::ObsDataVector<float>> obsErrors_;

  CxCheckerParameters parameters_;
};

}  // namespace test
}  // namespace cxvarobs

#endif  // TEST_CXVAROBS_CXCHECKER_H_
