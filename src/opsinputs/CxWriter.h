/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details. 
 */

#ifndef OPSINPUTS_CXWRITER_H_
#define OPSINPUTS_CXWRITER_H_

#include <memory>
#include <ostream>
#include <string>

#include "ioda/ObsDataVector.h"
#include "oops/base/Variables.h"
#include "oops/util/ObjectCounter.h"
#include "oops/util/Printable.h"
#include "opsinputs/CxWriter.interface.h"
#include "opsinputs/CxWriterParameters.h"

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
class CxWriter : public util::Printable, private util::ObjectCounter<CxWriter> {
 public:
  static const std::string classname() {return "opsinputs::CxWriter";}

  CxWriter(ioda::ObsSpace &, const eckit::Configuration &,
           std::shared_ptr<ioda::ObsDataVector<int> > flags,
           std::shared_ptr<ioda::ObsDataVector<float> > obsErrors);
  ~CxWriter();

  void preProcess() const {}
  void priorFilter(const ufo::GeoVaLs &) const;
  void postFilter(const ioda::ObsVector &, const ufo::ObsDiagnostics & diags) const;

  const oops::Variables & requiredVars() const {return geovars_;}
  const oops::Variables & requiredHdiagnostics() const {return extradiagvars_;}

 private:
  void print(std::ostream &) const;

  void setupEnvironment(LocalEnvironment &localEnvironment) const;

  void createOutputDirectory();

  F90check key_;

  ioda::ObsSpace & obsdb_;
  oops::Variables geovars_;
  oops::Variables extradiagvars_;
  std::shared_ptr<ioda::ObsDataVector<int>> flags_;
  std::shared_ptr<ioda::ObsDataVector<float>> obsErrors_;

  CxWriterParameters parameters_;
};

}  // namespace opsinputs

#endif  // OPSINPUTS_CXWRITER_H_
