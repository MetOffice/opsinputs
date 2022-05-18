/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details.
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
#include "oops/mpi/mpi.h"
#include "oops/util/Logger.h"
#include "opsinputs/LocalEnvironment.h"
#include "opsinputs/VarObsWriterParameters.h"
#include "ufo/GeoVaLs.h"
#include "ufo/ObsDiagnostics.h"

namespace opsinputs {

VarObsWriter::VarObsWriter(ioda::ObsSpace & obsdb, const Parameters_ & params,
                           std::shared_ptr<ioda::ObsDataVector<int> > flags,
                           std::shared_ptr<ioda::ObsDataVector<float> > obsErrors)
  : obsdb_(obsdb), geovars_(), extradiagvars_(), flags_(std::move(flags)),
    obsErrors_(std::move(obsErrors)), parameters_(params)
{
  oops::Log::trace() << "VarObsWriter constructor starting" << std::endl;

  LocalEnvironment localEnvironment;
  setupEnvironment(localEnvironment);
  createOutputDirectory();

  eckit::LocalConfiguration conf(parameters_.toConfiguration());
  // Validity time is set to the midpoint of the assimilation window
  // (this is rounded to the nearest hour)
  const util::DateTime exactValidityTime =
      obsdb.windowStart() + (obsdb.windowEnd() - obsdb.windowStart()) / 2;
  int year, month, day, hour, minute, second;
  exactValidityTime.toYYYYMMDDhhmmss(year, month, day, hour, minute, second);
  int nearestHour = std::round(hour + minute/60.0f + second/(60.0f*60.0f));
  util::DateTime validityTime;
  if (nearestHour == 24) {
    validityTime = util::DateTime(year, month, day, 0, 0, 0);
    validityTime += util::Duration("P1D");
  } else {
    validityTime = util::DateTime(year, month, day, nearestHour, 0, 0);
  }
  conf.set("validity_time", validityTime.toString());
  conf.set("obs_group", obsdb.obsname());

  const MPI_Fint fortranMpiCommunicator = obsdb.comm().communicator();
  // The call above returns a valid MPI communicator only if we're running with MPI.
  const bool fortranMpiCommunicatorIsValid =
      dynamic_cast<const eckit::mpi::Parallel*>(&obsdb.comm()) != nullptr;

  // We need to pass the list of channels in a separate parameter because the Fortran interface to
  // oops::Variables doesn't give access to it. I (wsmigaj) suspect channel handling will change
  // in the refactored version of ioda, so it doesn't seem worth patching oops::Variables now.
  const std::vector<int> scatwindchannels{1, 2, 3, 4};
  const std::vector<int> &channels = obsdb.obsname() == "Scatwind" ?
                                     scatwindchannels : obsdb_.obsvariables().channels();
  const int fallbackChannels = 0;
  // Avoid passing a null pointer to Fortran.
  const int *channelsData = channels.empty() ? &fallbackChannels : channels.data();
  if (!opsinputs_varobswriter_create_f90(key_, &conf,
                                         fortranMpiCommunicatorIsValid,
                                         fortranMpiCommunicator,
                                         channels.size(), channelsData,
                                         geovars_, extradiagvars_))
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

void VarObsWriter::priorFilter(const ufo::GeoVaLs & gv) {
  oops::Log::trace() << "VarObsWriter priorFilter" << std::endl;

  LocalEnvironment localEnvironment;
  setupEnvironment(localEnvironment);

  opsinputs_varobswriter_prior_f90(key_, obsdb_, gv.toFortran());
}

void VarObsWriter::postFilter(const ufo::GeoVaLs & gv,
                              const ioda::ObsVector &hofx,
                              const ioda::ObsVector &/*bias*/,
                              const ufo::ObsDiagnostics & obsdiags) {
  oops::Log::trace() << "VarObsWriter postFilter" << std::endl;

  LocalEnvironment localEnvironment;
  setupEnvironment(localEnvironment);

  opsinputs_varobswriter_post_f90(key_, obsdb_, *flags_, *obsErrors_,
                                  hofx.nvars(), hofx.nlocs(), hofx.toFortran(),
                                  obsdiags.toFortran());
}

void VarObsWriter::print(std::ostream & os) const {
  os << "VarObsWriter: config = " << parameters_ << std::endl;
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
