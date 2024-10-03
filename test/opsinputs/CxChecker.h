/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 */

#ifndef TEST_OPSINPUTS_CXCHECKER_H_
#define TEST_OPSINPUTS_CXCHECKER_H_

#include <map>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

#include "../opsinputs/CxCheckerParameters.h"
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
class CxChecker : public ufo::ObsFilterBase,
                  private util::ObjectCounter<CxChecker> {
 public:
  static const std::string classname() {return "opsinputs::test::CxChecker";}

  /// The type of parameters accepted by the constructor of this filter.
  /// This typedef is used by the FilterFactory.
  typedef CxCheckerParameters Parameters_;

  CxChecker(ioda::ObsSpace &, const Parameters_ &,
            std::shared_ptr<ioda::ObsDataVector<int> > flags,
            std::shared_ptr<ioda::ObsDataVector<float> > obsErrors);
  ~CxChecker();

  void preProcess() override {}
  void priorFilter(const ufo::GeoVaLs &) override {}
  void postFilter(const ufo::GeoVaLs &,
                  const ioda::ObsVector &hofx,
                  const ioda::ObsVector &bias,
                  const ufo::ObsDiagnostics &) override;
  void checkFilterData(const oops::FilterStage filterStage) override {}

  oops::Variables requiredVars() const override {return geovars_;}
  oops::ObsVariables requiredHdiagnostics() const override {return extradiagvars_;}

 private:
  struct PrintCxFileOutput;
  class MainTable;

  void print(std::ostream &) const override;

  void setupEnvironment(opsinputs::LocalEnvironment &localEnvironment) const;

  PrintCxFileOutput parsePrintCxFileOutput(const std::string &printCxFileOutput) const;

  void checkHeader(const std::map<std::string, std::string> &headerFields) const;
  void checkLevelDependentConstants(const std::vector<std::string> &etaThetaLevels,
                                    const std::vector<std::string> &etaRhoLevels) const;
  void checkVariables(const std::vector<std::string> &surfaceVariables,
                      const std::vector<std::string> &upperAirVariables) const;
  void checkLookup(
      const std::vector<std::map<std::string, std::string>> &lookupFields) const;
  void checkMainTable(
      const std::vector<std::vector<std::vector<std::string>>> &mainTable) const;

  ioda::ObsSpace & obsdb_;
  oops::Variables geovars_;
  oops::ObsVariables extradiagvars_;
  std::shared_ptr<ioda::ObsDataVector<int>> flags_;
  std::shared_ptr<ioda::ObsDataVector<float>> obsErrors_;

  CxCheckerParameters parameters_;
};

}  // namespace test
}  // namespace opsinputs

#endif  // TEST_OPSINPUTS_CXCHECKER_H_
