/*
 * (C) Copyright 2020 Met Office UK
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#include <utility>
#include <vector>

#include "cxvarobs/VarObsWriter.h"

#include "cxvarobs/LocalEnvironment.h"
#include "cxvarobs/VarObsWriterParameters.h"

#include "eckit/config/Configuration.h"

#include "ioda/ObsDataVector.h"
#include "ioda/ObsSpace.h"
#include "ioda/ObsVector.h"
#include "oops/base/Variables.h"
#include "oops/interface/ObsFilter.h"
#include "oops/util/Logger.h"
#include "ufo/GeoVaLs.h"

namespace cxvarobs {

VarObsWriter::VarObsWriter(ioda::ObsSpace & obsdb, const eckit::Configuration & config,
                           boost::shared_ptr<ioda::ObsDataVector<int> > flags,
                           boost::shared_ptr<ioda::ObsDataVector<float> > obsErrors)
  : obsdb_(obsdb), geovars_(), flags_(std::move(flags)), obsErrors_(std::move(obsErrors))
{
  oops::Log::trace() << "VarObsWriter constructor starting" << std::endl;

  parameters_.deserialize(config);

  LocalEnvironment localEnvironment;
  setupEnvironment(localEnvironment);
  createOutputDirectory();

  eckit::LocalConfiguration conf(config);
  // TODO(wsmigaj): is this the correct definition of the validity time?
  conf.set("validity_time", obsdb.windowEnd().toString());
  conf.set("obs_group", obsdb.obsname());

  if (!cxvarobs_varobswriter_create_f90(key_, &conf, geovars_))
    throw std::runtime_error("VarObsWriter construction failed. "
                             "See earlier messages for more details");
  oops::Log::debug() << "VarObsWriter constructor key = " << key_ << std::endl;
}

VarObsWriter::~VarObsWriter() {
  oops::Log::trace() << "VarObsWriter destructor key = " << key_ << std::endl;

  LocalEnvironment localEnvironment;
  setupEnvironment(localEnvironment);

  cxvarobs_varobswriter_delete_f90(key_);
}

void VarObsWriter::priorFilter(const ufo::GeoVaLs & gv) const {
  oops::Log::trace() << "VarObsWriter priorFilter" << std::endl;

  LocalEnvironment localEnvironment;
  setupEnvironment(localEnvironment);

  cxvarobs_varobswriter_prior_f90(key_, obsdb_, gv.toFortran());
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
  cxvarobs_varobswriter_post_f90(key_, obsdb_, channels.size(), channels.data(),
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
}

void VarObsWriter::createOutputDirectory() {
  char *outputDirectory = getenv("OPS_VAROB_OUTPUT_DIR");
  ASSERT_MSG(outputDirectory != nullptr, "The output directory has not been set");
  eckit::PathName outputPath(outputDirectory);
  ASSERT_MSG(!outputPath.exists() || outputPath.isDir(),
             "The output path '" + std::string(outputDirectory) + "'is not a directory");
  if (!outputPath.exists()) {
    outputPath.mkdir();
  }
}

}  // namespace cxvarobs