/*
 * (C) Copyright 2020 Met Office UK
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#include <cstdio>
#include <fstream>

#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/trim.hpp>

#include "../../test/cxvarobs/VarObsChecker.h"

#include "cxvarobs/LocalEnvironment.h"
#include "cxvarobs/VarObsWriterParameters.h"

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"

#include "ioda/ObsDataVector.h"
#include "ioda/ObsSpace.h"
#include "oops/base/Variables.h"
#include "oops/interface/ObsFilter.h"
#include "oops/parallel/mpi/mpi.h"
#include "oops/util/Logger.h"

#include "ufo/filters/Variables.h"

namespace cxvarobs {
namespace test {

namespace {

// This could be made OS-dependent.
const char PATH_SEPARATOR = '/';

std::string getEnvVariableOrThrow(const char *variableName) {
  const char *value = std::getenv(variableName);
  if (!value)
    throw std::runtime_error("Environment variable '" + std::string(variableName) + "' not set");
  return value;
}

bool startsWith(const std::string &string, const char *prefix) {
  return string.rfind(prefix, 0 /*look only at the beginning of string*/);
}

/// Manages a temporary file (deleted when this object is destroyed).
class TempFile {
 public:
  explicit TempFile(const eckit::PathName &fileName) : fileName_(fileName) {}
  explicit TempFile(const char *fileName) : fileName_(fileName) {}

  TempFile(const TempFile &) = delete;
  TempFile(TempFile &&other) { std::swap(fileName_, other.fileName_); }

  TempFile & operator=(const TempFile &) = delete;
  TempFile & operator=(TempFile &&other) {
    if (this != &other) {
      deleteFileAndResetFileName();
      std::swap(fileName_, other.fileName_);
    }
    return *this;
  }

  ~TempFile() {
    deleteFileAndResetFileName();
  }

  std::string name() const { return fileName_.asString(); }

 private:
  void deleteFileAndResetFileName() {
    if (fileName_.exists()) {
      fileName_.unlink();
    }
    fileName_ = eckit::PathName();
  }

 private:
  eckit::PathName fileName_;
};

}  // namespace

/// Encapsulates the main table of per-observation data printed by PrintVarobs.
class VarObsChecker::MainTable {
 public:
  MainTable(std::string headerLine, std::vector<std::string> dataLines);

  /// \brief Return values from the column with the specified header (trimmed on both sides).
  std::vector<std::string> operator[](const std::string &columnHeader) const;

 private:
  std::string headerLine_;
  std::vector<std::string> dataLines_;
};

VarObsChecker::MainTable::MainTable(std::string headerLine, std::vector<std::string> dataLines) :
  headerLine_(std::move(headerLine)), dataLines_(std::move(dataLines))
{}

std::vector<std::string> VarObsChecker::MainTable::operator[](
    const std::string &columnHeader) const {
  auto position = headerLine_.find(columnHeader);
  if (position == std::string::npos)
    throw std::runtime_error("Column '" + columnHeader + "' not found in VarObs data table");
  auto columnEnd = position + columnHeader.size();

  // Find the left end of the column, i.e. the beginning of the sequence of spaces preceding
  // the column header
  auto columnBegin = position;
  while (columnBegin != 0 && headerLine_[columnBegin - 1] == ' ')
    --columnBegin;

  std::vector<std::string> entries;
  entries.reserve(dataLines_.size());
  for (const std::string &line : dataLines_) {
    std::string entry = line.substr(columnBegin, columnEnd - columnBegin);
    boost::algorithm::trim(entry);
    entries.push_back(std::move(entry));
  }

  return entries;
}

struct VarObsChecker::PrintVarObsOutput {
  std::map<std::string, std::string> headerFields;
  MainTable mainTable;
};


VarObsChecker::VarObsChecker(ioda::ObsSpace & obsdb, const eckit::Configuration & config,
                             boost::shared_ptr<ioda::ObsDataVector<int> > flags,
                             boost::shared_ptr<ioda::ObsDataVector<float> > obsErrors)
  : obsdb_(obsdb), geovars_(), flags_(std::move(flags)), obsErrors_(std::move(obsErrors))
{
  oops::Log::trace() << "VarObsChecker constructor starting" << std::endl;
  parameters_.deserialize(config);
  ASSERT_MSG(!parameters_.expectedHeaderFields.value().empty() ||
             !parameters_.expectedMainTableColumns.value().empty(),
             "No VarObs file components to check have been specified");
}

VarObsChecker::~VarObsChecker() {
  oops::Log::trace() << "VarObsChecker destructor starting" << std::endl;
}

void VarObsChecker::postFilter(const ioda::ObsVector &, const ufo::ObsDiagnostics &) const {
  oops::Log::trace() << "VarObsChecker postFilter" << std::endl;

  // Print the contents of the varobs file to a temporary file...

  cxvarobs::LocalEnvironment localEnvironment;
  setupEnvironment(localEnvironment);

  const eckit::PathName varObsFileName(getEnvVariableOrThrow("OPS_VAROB_OUTPUT_DIR") +
            PATH_SEPARATOR + obsdb_.obsname() + ".varobs");
  if (!varObsFileName.exists())
    throw std::runtime_error("File '" + varObsFileName + "' not found");

  char tempFileName[L_tmpnam];
  std::tmpnam(tempFileName);

  // TODO(wsmigaj): need to share the same file name across all MPI processes.
  TempFile tempFile(tempFileName);

  const char exeName[] = "OpsProg_PrintVarobs.exe";
  std::string exePath;
  if (char *dir = getenv("CXVAROBS_OPS_BIN_DIR")) {
    exePath = dir;
    exePath += PATH_SEPARATOR;
    exePath += exeName;
  } else {
    exePath = exeName;
  }

  // TODO(wsmigaj): perhaps read the name of the MPI runner from an environment variable
  const std::string cmd = "mpiexec -n 1 " + exePath + " \"" + varObsFileName +
      "\" --all --outfile=\"" + tempFile.name() + "\"";
  if (oops::mpi::comm().rank() == 0) {
    oops::Log::info() << "Running " << cmd << "\n";
    const int exitCode = std::system(cmd.c_str());
    if (exitCode != 0)
      throw std::runtime_error("PrintVarobs failed with exit code " + std::to_string(exitCode));
  }

  // ... parse them and check them

  PrintVarObsOutput output = parsePrintVarObsOutput(tempFile.name());
  checkHeader(output.headerFields);
  checkMainTable(output.mainTable);
}

void VarObsChecker::setupEnvironment(cxvarobs::LocalEnvironment &localEnvironment) const {
  if (parameters_.namelistDirectory.value() != boost::none)
    localEnvironment.set("OPS_VAROBSCONTROL_NL_DIR", *parameters_.namelistDirectory.value());
  if (parameters_.outputDirectory.value() != boost::none)
    localEnvironment.set("OPS_VAROB_OUTPUT_DIR", *parameters_.outputDirectory.value());
}

VarObsChecker::PrintVarObsOutput VarObsChecker::parsePrintVarObsOutput(
    const std::string &fileName) const {
  std::ifstream is(fileName);
  std::string line;

  enum FileSection {
    BeforeColumnDependentConstants,
    BeforeLevelDependentConstants,
    BeforeMainTable,
    InMainTable
  };

  FileSection section = BeforeColumnDependentConstants;

  std::map<std::string, std::string> headerFields;

  std::string mainTableHeader;
  std::vector<std::string> mainTableContents;
  while (std::getline(is, line)) {
    switch (section) {
    case BeforeColumnDependentConstants:
      if (line == "Column Dependent Constants:") {
        section = BeforeLevelDependentConstants;
      } else {
        const char separator[] = " = ";
        auto separatorPos = line.find(separator);
        if (separatorPos != std::string::npos) {
          std::string name = line.substr(0, separatorPos);
          std::string value = line.substr(separatorPos + sizeof(separator) - 1);
          boost::algorithm::trim(name);
          boost::algorithm::trim(value);
          headerFields[name] = value;
        }
      }
      break;
    case BeforeLevelDependentConstants:
      if (line == "Level Dependent Constants:") {
        section = BeforeMainTable;
      }
      break;
    case BeforeMainTable:
      if (boost::algorithm::starts_with(line, "  batch ")) {
        mainTableHeader = line;
        section = InMainTable;
      }
      break;
    case InMainTable:
      mainTableContents.push_back(line);
      break;
    }
  }

  return PrintVarObsOutput{std::move(headerFields),
                           MainTable(std::move(mainTableHeader), std::move(mainTableContents))};
}

void VarObsChecker::checkHeader(const std::map<std::string, std::string> &headerFields) const {
  for (const std::pair<const std::string, std::string> &expectedNameAndValue :
       parameters_.expectedHeaderFields.value()) {
    const std::string &value = headerFields.at(expectedNameAndValue.first);
    if (value != expectedNameAndValue.second) {
      std::stringstream str;
      str << "Header field '" << expectedNameAndValue.first << "':\n  received: " << value
          << "\n  expected: " << expectedNameAndValue.second;
      throw std::runtime_error(str.str());
    }
  }
}

void VarObsChecker::checkMainTable(const MainTable &mainTable) const {
  for (const std::pair<const std::string, std::vector<std::string>> &expectedNameAndValues :
       parameters_.expectedMainTableColumns.value()) {
    const std::vector<std::string> &values = mainTable[expectedNameAndValues.first];
    if (values != expectedNameAndValues.second) {
      std::stringstream str;
      str << "Column '" << expectedNameAndValues.first << "':\n  received: " << values
          << "\n  expected: " << expectedNameAndValues.second;
      throw std::runtime_error(str.str());
    }
  }
}

void VarObsChecker::print(std::ostream & os) const {
  os << "VarObsChecker::print not yet implemented";
}

}  // namespace test
}  // namespace cxvarobs
