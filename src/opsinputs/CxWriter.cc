/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details.
 */

#include "opsinputs/CxWriter.h"

#include <utility>
#include <vector>

#include "eckit/config/Configuration.h"

#include "ioda/ObsDataVector.h"
#include "ioda/ObsSpace.h"
#include "ioda/ObsVector.h"
#include "oops/base/Variables.h"
#include "oops/util/Logger.h"
#include "opsinputs/CxWriterParameters.h"
#include "opsinputs/LocalEnvironment.h"
#include "ufo/GeoVaLs.h"

namespace opsinputs {

CxWriter::CxWriter(ioda::ObsSpace & obsdb, const Parameters_ & params,
                   std::shared_ptr<ioda::ObsDataVector<int> > flags,
                   std::shared_ptr<ioda::ObsDataVector<float> > obsErrors)
  : obsdb_(obsdb), geovars_(), flags_(std::move(flags)), obsErrors_(std::move(obsErrors)),
    parameters_(params)
{
  oops::Log::trace() << "CxWriter constructor starting" << std::endl;

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
  int nearestHour = std::round(hour + minute/60.0f + second/60.0f/60.0f);
  const util::DateTime validityTime(year, month, day, nearestHour, 0, 0);
  conf.set("validity_time", validityTime.toString());
  conf.set("obs_group", obsdb.obsname());

  if (!opsinputs_cxwriter_create_f90(key_, &conf, geovars_))
    throw std::runtime_error("CxWriter construction failed. "
                             "See earlier messages for more details");
  oops::Log::debug() << "CxWriter constructor key = " << key_ << std::endl;
}

CxWriter::~CxWriter() {
  oops::Log::trace() << "CxWriter destructor key = " << key_ << std::endl;

  LocalEnvironment localEnvironment;
  setupEnvironment(localEnvironment);

  opsinputs_cxwriter_delete_f90(key_);
}

void CxWriter::priorFilter(const ufo::GeoVaLs & gv) {
  oops::Log::trace() << "CxWriter priorFilter" << std::endl;

  LocalEnvironment localEnvironment;
  setupEnvironment(localEnvironment);

  opsinputs_cxwriter_prior_f90(key_, obsdb_, gv.toFortran());
}

void CxWriter::postFilter(const ioda::ObsVector &hofx,
                          const ioda::ObsVector &/*bias*/,
                          const ufo::ObsDiagnostics &) {
  oops::Log::trace() << "CxWriter postFilter" << std::endl;

  LocalEnvironment localEnvironment;
  setupEnvironment(localEnvironment);

  opsinputs_cxwriter_post_f90(key_, obsdb_, *flags_,
                              hofx.nvars(), hofx.nlocs(), hofx.varnames(), &hofx.toFortran());
}

void CxWriter::print(std::ostream & os) const {
  os << "CxWriter: config = " << parameters_ << std::endl;
}

void CxWriter::setupEnvironment(LocalEnvironment &localEnvironment) const {
  if (parameters_.namelistDirectory.value() != boost::none)
    localEnvironment.set("OPS_CX_CONTROL_NL_DIR", *parameters_.namelistDirectory.value());
  if (parameters_.outputDirectory.value() != boost::none)
    localEnvironment.set("OPS_CX_DIR_LIST", *parameters_.outputDirectory.value());

  // Hack taken from OpsScr_Setup; prevents ODB code (which provides yomhook and parkind1)
  // from calling mpi_buffer_attach. gc_init() already does this.
  localEnvironment.set("MPL_METHOD", "JP_BLOCKING_STANDARD");
}

void CxWriter::createOutputDirectory() {
  char *outputDirectory = getenv("OPS_CX_DIR_LIST");
  ASSERT_MSG(outputDirectory != nullptr, "The output directory has not been set");
  eckit::PathName outputPath(outputDirectory);
  ASSERT_MSG(!outputPath.exists() || outputPath.isDir(),
             "The output path '" + std::string(outputDirectory) + "' is not a directory");
  outputPath.mkdir();
}

}  // namespace opsinputs
