/*
 * (C) Copyright 2020 Met Office UK
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#include <fstream>
#include <regex>

#include <boost/algorithm/string/split.hpp>
#include <Eigen/Core>

#include "../test/cxvarobs/VarObsChecker.h"

#include "cxvarobs/LocalEnvironment.h"
#include "cxvarobs/VarObsWriterParameters.h"

#include "eckit/config/Configuration.h"
#include "eckit/filesystem/TempFile.h"
#include "eckit/testing/Test.h"

#include "ioda/ObsDataVector.h"
#include "ioda/ObsSpace.h"
#include "oops/base/Variables.h"
#include "oops/interface/ObsFilter.h"
#include "oops/util/Logger.h"

#include "ufo/filters/Variables.h"

namespace cxvarobs {

namespace {

// This could be made OS-dependent.
const char PATH_SEPARATOR = '/';

struct FloatVariable {
  std::vector<int> channels;
  Eigen::MatrixXf values;
  Eigen::MatrixXi qcFlags;
  boost::optional<Eigen::MatrixXf> errors;
  boost::optional<Eigen::MatrixXf> grossErrorProbabilities;
};

struct IntVariable {
  std::vector<int> channels;
  Eigen::MatrixXi values;
};

std::string getEnvVariableOrDefault(const char *variableName, const char *defaultValue) {
  const char *value = std::getenv(variableName);
  return value ? value : defaultValue;
}

std::string getEnvVariableOrThrow(const char *variableName) {
  const char *value = std::getenv(variableName);
  if (!value)
    throw std::runtime_error("Environment variable '" + std::string(variableName) + "' not set");
  return value;
}

bool startsWith(const std::string &string, const char *prefix) {
  return string.rfind(prefix, 0 /*look only at the beginning of string*/);
}

}  // namespace

VarObsChecker::VarObsChecker(ioda::ObsSpace & obsdb, const eckit::Configuration & config,
                             boost::shared_ptr<ioda::ObsDataVector<int> > flags,
                             boost::shared_ptr<ioda::ObsDataVector<float> > obsErrors)
  : obsdb_(obsdb), geovars_(), flags_(std::move(flags)), obsErrors_(std::move(obsErrors))
{
  oops::Log::trace() << "VarObsChecker constructor starting" << std::endl;
  parameters_.deserialize(config);
}

VarObsChecker::~VarObsChecker() {
  oops::Log::trace() << "VarObsChecker destructor starting" << std::endl;
}

void VarObsChecker::priorFilter(const ufo::GeoVaLs &) const {
  oops::Log::trace() << "VarObsChecker priorFilter" << std::endl;
}

void VarObsChecker::postFilter(const ioda::ObsVector &, const ufo::ObsDiagnostics &) const {
  oops::Log::trace() << "VarObsChecker postFilter" << std::endl;

  LocalEnvironment localEnvironment;
  setupEnvironment(localEnvironment);

  const eckit::PathName varObsFileName(getEnvVariableOrThrow("OPS_VAROB_OUTPUT_DIR") +
            PATH_SEPARATOR + obsdb_.obsname() + ".varobs");
  if (!varObsFileName.exists())
    throw std::runtime_error("File '" + varObsFileName + "' not found");

  eckit::TempFile tempFile;
  std::string cmd = "PrintVarobs \"" + varObsFileName + "\" --all --outfile \"" + tempFile + "\"";
  const int exitCode = std::system(cmd.c_str());

  if (exitCode != 0)
    throw std::runtime_error("PrintVarobs failed with exit code " + std::to_string(exitCode));
  std::ifstream tempFileStream(tempFile.asString());
  std::string line;

  enum FileSection {
    BeforeColumnDependentConstants,
    BeforeLevelDependentConstants,
    BeforeMainTable,
    InMainTable
  };

  FileSection section = BeforeColumnDependentConstants;

  std::regex columnDepConstantsRegEx(R"((.*?) +\(([0-9]+)\) +([0-9]+)");

  std::map<std::string, std::string> headerFields;
  std::map<int, int> numLevelsByVarfield;
  std::vector<std::string> mainTableHeaders;
  std::vector<std::string> tokens;
  std::vector<std::vector<std::string>> mainTableColumns;
  while (std::getline(tempFileStream, line)) {
    switch (section) {
    case BeforeColumnDependentConstants:
      if (line == "Column Dependent Constants:") {
        section = BeforeLevelDependentConstants;
      } else {
        const char* separator = " = ";
        auto separatorPos = line.find(separator);
        if (separatorPos != std::string::npos) {
          headerFields[line.substr(0, separatorPos)] =
              line.substr(separatorPos + sizeof(separator) - 1);
        }
      }
      break;
    case BeforeLevelDependentConstants:
      if (line == "Level Dependent Constants:") {
        section = BeforeMainTable;
      } else {
        std::smatch match;
        if (std::regex_match(line, match, columnDepConstantsRegEx))
          numLevelsByVarfield[std::stoi(match[1].str())] = std::stoi(match[2].str());
      }
      break;
    case BeforeMainTable:
      if (startsWith(line, "batch ")) {
        section = InMainTable;
        boost::algorithm::split(mainTableHeaders, line, [](char ch) { return ch == ' ';},
                                boost::token_compress_on);
        // Unfortunately some headers contain spaces; they start with "ob ", and we need to
        // merge each "ob" prefix with the following word.
        for (size_t i = 0; i + 1 < mainTableHeaders.size(); ++i) {
          if (mainTableHeaders[i] == "ob") {
            mainTableHeaders[i] += " " + mainTableHeaders[i + 1];
            mainTableHeaders.erase(mainTableHeaders.begin() + i + 1);
          }
        }
        mainTableColumns.resize(mainTableHeaders.size());
      }
      break;
    case InMainTable:
      boost::algorithm::split(tokens, line, [](char ch) { return ch == ' ';},
                              boost::token_compress_on);
      if (tokens.size() != mainTableColumns.size())
        throw std::runtime_error("Unexpected number of space-separated tokens in line '"
                                 + line + "'");
      for (size_t i = 0; i < tokens.size(); ++i)
        mainTableColumns[i].push_back(tokens[i]);
      break;
    }
  }

  checkUMHeader(headerFields);

  for (const std::string &varfieldName : parameters_.expectedVarfields.value()) {
    checkVarfield(varfieldName, numLevelsByVarfield, mainTableHeaders, mainTableColumns);
  }


}

void VarObsChecker::checkUMHeader(const std::map<std::string, std::string> &headerFields) const {
  EXPECT_EQUAL(std::stoi(headerFields.at("Vertical coordinate type")),
               FH_VertCoord_from_string(parameters_.FH_VertCoord.value()));
  EXPECT_EQUAL(std::stoi(headerFields.at("Horizontal grid type")),
               FH_HorizGrid_from_string(parameters_.FH_HorizGrid.value()));
  EXPECT_EQUAL(std::stoi(headerFields.at("Grid staggering type")),
               FH_GridStagger_from_string(parameters_.FH_GridStagger.value()));

  EXPECT_EQUAL(std::stoi(headerFields.at("Number EW points")),
               parameters_.IC_XLen.value());
//  oops::Parameter<int> IC_XLen{"IC_XLen", 0, this};
//  oops::Parameter<int> IC_YLen{"IC_YLen", 0, this};
//  oops::Parameter<int> IC_PLevels{"IC_PLevels", 0, this};
//  oops::Parameter<int> IC_WetLevels{"IC_WetLevels", 0, this};
//  oops::Parameter<std::string> IC_TorTheta{"IC_TorTheta", "IC_TorTheta_T", this};
//  oops::Parameter<bool> IC_ShipWind{"IC_ShipWind", false, this};
//  oops::Parameter<std::string> IC_GroundGPSOperator{"IC_GroundGPSOperator", "", this};
//  oops::Parameter<bool> IC_GPSRO_Operator_pseudo{"IC_GPSRO_Operator_pseudo", false, this};
//  oops::Parameter<bool> IC_GPSRO_Operator_press{"IC_GPSRO_Operator_press", false, this};

  // Or just print the C++ number with 2 digits after the decimal point...
  const realHeaderFieldsPrecision = 0.01;
  EXPECT_EQUAL(oops::is_close_absolute(std::stod(headerFields.at("Longitude spacing")),
               parameters_.RC_LongSpacing.value(), realHeaderFieldsPrecision));
//  oops::Parameter<double> RC_LongSpacing{"RC_LongSpacing", 0.0, this};
//  oops::Parameter<double> RC_LatSpacing{"RC_LatSpacing", 0.0, this};
//  oops::Parameter<double> RC_FirstLat{"RC_FirstLat", 0.0, this};
//  oops::Parameter<double> RC_FirstLong{"RC_FirstLong", 0.0, this};
//  oops::Parameter<double> RC_PoleLat{"RC_PoleLat", 0.0, this};
//  oops::Parameter<double> RC_PoleLong{"RC_PoleLong", 0.0, this};
}

void VarObsChecker::checkVarfield(
    const std::string &varfieldName,
    const std::map<int, int> &numLevelsByVarfield,
    const std::vector<std::string> &mainTableHeaders,
    const std::vector<std::vector<std::string>> &mainTableColumns) const {
  if (parameters_.expectedObValues.value())
    EXPECT_EQUAL(mainTable["ob value"], parameters_.expectedObValues.value());
  if (parameters_.expectedObErrors.value())
    EXPECT_EQUAL(mainTable["ob value"], parameters_.expectedObErrors.value());
  if (parameters_.expectedPges.value())
    EXPECT_EQUAL(mainTable["pge"], parameters_.expectedPges.value());
  if (parameters_.expectedDate.value())
    EXPECT_EQUAL(mainTable["pge"], parameters_.expectedPges.value());

  int varfield;
  switch (varfield) {
    case VarfieldAirTemperature:
    {
      // Clearly we can't hardcode these values here...
      EXPECT_EQUAL(mainTable["ob value"], parameters_.expectedObValues);
    }
  }
}

FloatVariable VarObsChecker::getVariableFromObsSpace(const std::string &name,
                                                     const std::string &group) const {
  const int minChannel = 0, maxChannel = 10000;

  FloatVariable var;

  std::vector<float> values(obsdb_.nlocs());
  if (obsdb_.has(group, name)) {
    var.values.set_size(obsdb_.nlocs());
    obsdb_.get_db(group, name, values);
    obsErrors_->has(name);
    obsdb_.
  }
}

void VarObsChecker::print(std::ostream & os) const {
  os << "VarObsChecker::print not yet implemented";
}

void VarObsChecker::setupEnvironment(LocalEnvironment &localEnvironment) const {
  if (parameters_.namelist_directory.value() != boost::none)
    localEnvironment.set("OPS_VAROBSCONTROL_NL_DIR", *parameters_.namelist_directory.value());
  if (parameters_.output_directory.value() != boost::none)
    localEnvironment.set("OPS_VAROB_OUTPUT_DIR", *parameters_.output_directory.value());
}

}  // namespace cxvarobs
