/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "opsinputs/CxWriter.h"

#include <utility>
#include <vector>

#include "eckit/config/Configuration.h"

#include "ioda/ObsDataVector.h"
#include "ioda/ObsSpace.h"
#include "ioda/ObsVector.h"
#include "oops/base/Variables.h"
#include "oops/interface/ObsFilter.h"
#include "oops/util/Logger.h"
#include "opsinputs/CxWriterParameters.h"
#include "opsinputs/LocalEnvironment.h"
#include "ufo/GeoVaLs.h"

namespace opsinputs {

CxWriter::CxWriter(ioda::ObsSpace & obsdb, const eckit::Configuration & config,
                   boost::shared_ptr<ioda::ObsDataVector<int> > flags,
                   boost::shared_ptr<ioda::ObsDataVector<float> > obsErrors)
  : obsdb_(obsdb), geovars_(), flags_(std::move(flags)), obsErrors_(std::move(obsErrors))
{
  oops::Log::trace() << "CxWriter constructor starting" << std::endl;

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

void CxWriter::priorFilter(const ufo::GeoVaLs & gv) const {
  oops::Log::trace() << "CxWriter priorFilter" << std::endl;

  LocalEnvironment localEnvironment;
  setupEnvironment(localEnvironment);

  opsinputs_cxwriter_prior_f90(key_, obsdb_, gv.toFortran());
}

void CxWriter::postFilter(const ioda::ObsVector &, const ufo::ObsDiagnostics &) const {
  oops::Log::trace() << "CxWriter postFilter" << std::endl;

  LocalEnvironment localEnvironment;
  setupEnvironment(localEnvironment);

  opsinputs_cxwriter_post_f90(key_, obsdb_, *flags_);
}

void CxWriter::print(std::ostream & os) const {
  // To implement this, it would be best to add a print method or equivalent to Parameters.
  // Something to think of in future.
  os << "CxWriter::print not yet implemented " << key_;
}

void CxWriter::setupEnvironment(LocalEnvironment &localEnvironment) const {
  if (parameters_.namelistDirectory.value() != boost::none)
    localEnvironment.set("OPS_CX_CONTROL_NL_DIR", *parameters_.namelistDirectory.value());
  if (parameters_.outputDirectory.value() != boost::none)
    localEnvironment.set("OPS_CX_DIR_LIST", *parameters_.outputDirectory.value());
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
