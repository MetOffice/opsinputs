/*
 * (C) Copyright 2020 Met Office UK
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#include "opsinputs/VarObsWriter.h"

#include <utility>
#include <vector>

#include "eckit/config/Configuration.h"
#include "eckit/mpi/Comm.h"
#include "eckit/mpi/Parallel.h"

#include "ioda/ObsDataVector.h"
#include "ioda/ObsSpace.h"
#include "ioda/ObsVector.h"
#include "oops/base/Variables.h"
#include "oops/interface/ObsFilter.h"
#include "oops/parallel/mpi/mpi.h"
#include "oops/util/Logger.h"
#include "opsinputs/LocalEnvironment.h"
#include "opsinputs/VarObsWriterParameters.h"
#include "ufo/GeoVaLs.h"

namespace opsinputs {

VarObsWriter::VarObsWriter(ioda::ObsSpace & obsdb, const eckit::Configuration & config,
                           std::shared_ptr<ioda::ObsDataVector<int> > flags,
                           std::shared_ptr<ioda::ObsDataVector<float> > obsErrors)
  : obsdb_(obsdb), geovars_(), flags_(std::move(flags)), obsErrors_(std::move(obsErrors))
{
  oops::Log::trace() << "VarObsWriter constructor starting" << std::endl;

  parameters_.deserialize(config);

  LocalEnvironment localEnvironment;
  setupEnvironment(localEnvironment);
  createOutputDirectory();

  eckit::LocalConfiguration conf(config);
  // Validity time is set to the midpoint of the assimilation window
  const util::DateTime validityTime =
      obsdb.windowStart() + (obsdb.windowEnd() - obsdb.windowStart()) / 2;
  conf.set("validity_time", validityTime.toString());
  conf.set("obs_group", obsdb.obsname());

  MPI_Comm mpiComm = MPI_COMM_WORLD;
  if (auto parallelComm = dynamic_cast<const eckit::mpi::Parallel*>(&obsdb.comm())) {
    mpiComm = parallelComm->MPIComm();
  }

  if (!opsinputs_varobswriter_create_f90(key_, &conf, mpiComm, geovars_))
    throw std::runtime_error("VarObsWriter construction failed. "
                             "See earlier messages for more details");
  oops::Log::debug() << "VarObsWriter constructor key = " << key_ << std::endl;
}

VarObsWriter::~VarObsWriter() {
  oops::Log::trace() << "VarObsWriter destructor key = " << key_ << std::endl;

  LocalEnvironment localEnvironment;
  setupEnvironment(localEnvironment);

  opsinputs_varobswriter_delete_f90(key_);
}

void VarObsWriter::priorFilter(const ufo::GeoVaLs & gv) const {
  oops::Log::trace() << "VarObsWriter priorFilter" << std::endl;

  LocalEnvironment localEnvironment;
  setupEnvironment(localEnvironment);

  opsinputs_varobswriter_prior_f90(key_, obsdb_, gv.toFortran());
}

void VarObsWriter::postFilter(const ioda::ObsVector & hofxb,
                              const ufo::ObsDiagnostics &) const {
  oops::Log::trace() << "VarObsWriter postFilter" << std::endl;

  LocalEnvironment localEnvironment;
  setupEnvironment(localEnvironment);

  // We need to pass the list of channels in a separate parameter because the Fortran interface to
  // oops::Variables doesn't give access to it. I (wsmigaj) suspect channel handling will change
  // in the refactored version of ioda, so it doesn't seem worth patching oops::Variables now.
  const std::vector<int> &channels = obsdb_.obsvariables().channels();
  opsinputs_varobswriter_post_f90(key_, obsdb_, channels.size(), channels.data(),
                                 *flags_, *obsErrors_,
                                 hofxb.nvars(), hofxb.nlocs(), hofxb.toFortran());
}

void VarObsWriter::print(std::ostream & os) const {
  os << "VarObsWriter::print not yet implemented " << key_;
}

void VarObsWriter::setupEnvironment(LocalEnvironment &localEnvironment) const {
  if (parameters_.namelistDirectory.value() != boost::none)
    localEnvironment.set("OPS_VAROBSCONTROL_NL_DIR", *parameters_.namelistDirectory.value());
  if (parameters_.outputDirectory.value() != boost::none)
    localEnvironment.set("OPS_VAROB_OUTPUT_DIR", *parameters_.outputDirectory.value());

  // Hack taken from OpsScr_Setup; prevents ODB code (which provides yomhook and parkind1)
  // from calling mpi_buffer_attach. gc_init() already does this.
  localEnvironment.set("MPL_METHOD", "JP_BLOCKING_STANDARD");
}

void VarObsWriter::createOutputDirectory() {
  char *outputDirectory = getenv("OPS_VAROB_OUTPUT_DIR");
  ASSERT_MSG(outputDirectory != nullptr, "The output directory has not been set");
  eckit::PathName outputPath(outputDirectory);
  ASSERT_MSG(!outputPath.exists() || outputPath.isDir(),
             "The output path '" + std::string(outputDirectory) + "' is not a directory");
  outputPath.mkdir();
}

}  // namespace opsinputs
