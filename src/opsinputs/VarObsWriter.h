/*
 * (C) Copyright 2020 Met Office UK
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#ifndef OPSINPUTS_VAROBSWRITER_H_
#define OPSINPUTS_VAROBSWRITER_H_

#include <memory>
#include <ostream>
#include <string>

#include "ioda/ObsDataVector.h"
#include "oops/base/Variables.h"
#include "oops/util/ObjectCounter.h"
#include "oops/util/Printable.h"
#include "opsinputs/VarObsWriter.interface.h"
#include "opsinputs/VarObsWriterParameters.h"

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

/// \brief Outputs observations to VarObs files.
///
/// Most of the implementation is in Fortran (opsinputs_varobswriter_mod.F90).
///
/// \note This filter must only be used with ObsSpaces using the \c MPI_COMM_WORLD communicator,
/// otherwise a deadlock will occur while writing the VarObs file. This is due to a limitation of
/// the \c Ops_WriteVarobs function, which could be removed by replacing \c mpl_comm_world in the
/// call to \c Ops_Mpl_Gatherv by \c mpi_group (for consistency with all other MPI calls in \c
/// Ops_WriteVarobs).
///
/// \see VarObsWriterParameters for the list of accepted configuration parameters.
class VarObsWriter : public util::Printable, private util::ObjectCounter<VarObsWriter> {
 public:
  static const std::string classname() {return "opsinputs::VarObsWriter";}

  VarObsWriter(ioda::ObsSpace &, const eckit::Configuration &,
               std::shared_ptr<ioda::ObsDataVector<int> > flags,
               std::shared_ptr<ioda::ObsDataVector<float> > obsErrors);
  ~VarObsWriter();

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

  VarObsWriterParameters parameters_;
};

}  // namespace opsinputs

#endif  // OPSINPUTS_VAROBSWRITER_H_
