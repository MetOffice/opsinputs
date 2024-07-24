/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 */

#ifndef OPSINPUTS_CXWRITER_H_
#define OPSINPUTS_CXWRITER_H_

#include <memory>
#include <ostream>
#include <string>

#include "ioda/ObsDataVector.h"
#include "oops/base/ObsVariables.h"
#include "oops/base/Variables.h"
#include "oops/interface/ObsFilterBase.h"
#include "oops/util/ObjectCounter.h"
#include "opsinputs/CxWriter.interface.h"
#include "opsinputs/CxWriterParameters.h"
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

/// \brief Outputs geovals to Cx files.
///
/// Most of the implementation is in Fortran (opsinputs_cxwriter_mod.F90).
///
/// \see CxWriterParameters for the list of accepted configuration parameters.
class CxWriter : public oops::interface::ObsFilterBase<ufo::ObsTraits>,
                 private util::ObjectCounter<CxWriter> {
 public:
  static const std::string classname() {return "opsinputs::CxWriter";}

  /// The type of parameters accepted by the constructor of this filter.
  /// This typedef is used by the FilterFactory.
  typedef CxWriterParameters Parameters_;

  CxWriter(ioda::ObsSpace &, const Parameters_ &,
           std::shared_ptr<ioda::ObsDataVector<int> > flags,
           std::shared_ptr<ioda::ObsDataVector<float> > obsErrors);
  ~CxWriter();

  void preProcess() override {}
  void priorFilter(const ufo::GeoVaLs &) override;
  void postFilter(const ufo::GeoVaLs &,
                  const ioda::ObsVector &hofx,
                  const ioda::ObsVector &bias,
                  const ufo::ObsDiagnostics & diags) override;
  void checkFilterData(const oops::FilterStage filterStage) override {}

  oops::Variables requiredVars() const override {return geovars_;}
  oops::ObsVariables requiredHdiagnostics() const override {return extradiagvars_;}

 private:
  void print(std::ostream &) const override;

  void setupEnvironment(LocalEnvironment &localEnvironment) const;

  void createOutputDirectory();

  F90check key_;

  ioda::ObsSpace & obsdb_;
  oops::Variables geovars_;
  oops::ObsVariables extradiagvars_;
  std::shared_ptr<ioda::ObsDataVector<int>> flags_;
  std::shared_ptr<ioda::ObsDataVector<float>> obsErrors_;

  CxWriterParameters parameters_;
};

}  // namespace opsinputs

#endif  // OPSINPUTS_CXWRITER_H_
