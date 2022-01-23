/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details. 
 */

#ifndef OPSINPUTS_VAROBSWRITER_H_
#define OPSINPUTS_VAROBSWRITER_H_

#include <memory>
#include <ostream>
#include <string>

#include "ioda/ObsDataVector.h"
#include "oops/base/Variables.h"
#include "oops/interface/ObsFilterBase.h"
#include "oops/util/ObjectCounter.h"
#include "oops/util/Printable.h"
#include "opsinputs/VarObsWriter.interface.h"
#include "opsinputs/VarObsWriterParameters.h"
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
class VarObsWriter : public oops::interface::ObsFilterBase<ufo::ObsTraits>,
                     private util::ObjectCounter<VarObsWriter> {
 public:
  static const std::string classname() {return "opsinputs::VarObsWriter";}

  /// The type of parameters accepted by the constructor of this filter.
  /// This typedef is used by the FilterFactory.
  typedef VarObsWriterParameters Parameters_;

  VarObsWriter(ioda::ObsSpace &, const Parameters_ &,
               std::shared_ptr<ioda::ObsDataVector<int> > flags,
               std::shared_ptr<ioda::ObsDataVector<float> > obsErrors);
  ~VarObsWriter();

  void preProcess() override {}
  void priorFilter(const ufo::GeoVaLs &) override;
  void postFilter(const ufo::GeoVaLs &,
                  const ioda::ObsVector &hofx,
                  const ioda::ObsVector &bias,
                  const ufo::ObsDiagnostics &diags) override;
  void checkFilterData(const oops::FilterStage filterStage) override {}

  oops::Variables requiredVars() const override {return geovars_;}
  oops::Variables requiredHdiagnostics() const override {return extradiagvars_;}

 private:
  void print(std::ostream &) const override;

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
