/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 */

#ifndef TEST_OPSINPUTS_VAROBSCHECKER_H_
#define TEST_OPSINPUTS_VAROBSCHECKER_H_

#include <map>
#include <memory>
#include <ostream>
#include <string>

#include "../opsinputs/VarObsCheckerParameters.h"
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

/// \brief Compares contents of VarObs files against reference values specified in the
/// configuration.
///
/// The OpsProg_PrintVarobs.exe OPS utility is used to print the contents of VarObs files in textual
/// form. Reference values of individual fields/arrays are then compared against values extracted
/// from that output.
///
/// See VarObsCheckerParameters for a list of available options.
class VarObsChecker : public ufo::ObsFilterBase,
                      private util::ObjectCounter<VarObsChecker> {
 public:
  static const std::string classname() {return "opsinputs::test::VarObsChecker";}

  /// The type of parameters accepted by the constructor of this filter.
  /// This typedef is used by the FilterFactory.
  typedef VarObsCheckerParameters Parameters_;

  VarObsChecker(ioda::ObsSpace &, const Parameters_ &,
                std::shared_ptr<ioda::ObsDataVector<int> > flags,
                std::shared_ptr<ioda::ObsDataVector<float> > obsErrors);
  ~VarObsChecker();

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
  struct PrintVarObsOutput;
  class MainTable;

  void print(std::ostream &) const override;

  void setupEnvironment(opsinputs::LocalEnvironment &localEnvironment) const;

  PrintVarObsOutput parsePrintVarObsOutput(const std::string &printVarObsOutput) const;

  void checkHeader(const std::map<std::string, std::string> &headerFields) const;

  void checkMainTable(const MainTable &mainTable) const;

  ioda::ObsSpace & obsdb_;
  oops::Variables geovars_;
  oops::ObsVariables extradiagvars_;
  std::shared_ptr<ioda::ObsDataVector<int>> flags_;
  std::shared_ptr<ioda::ObsDataVector<float>> obsErrors_;

  VarObsCheckerParameters parameters_;
};

}  // namespace test
}  // namespace opsinputs

#endif  // TEST_OPSINPUTS_VAROBSCHECKER_H_
