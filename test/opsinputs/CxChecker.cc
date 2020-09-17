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

#include "../../test/opsinputs/CheckerUtils.h"
#include "../../test/opsinputs/CxChecker.h"

#include "opsinputs/LocalEnvironment.h"
#include "opsinputs/MPIExceptionSynchronizer.h"

#include "eckit/config/Configuration.h"
#include "eckit/exception/Exceptions.h"

#include "ioda/ObsDataVector.h"
#include "ioda/ObsSpace.h"
#include "oops/base/Variables.h"
#include "oops/interface/ObsFilter.h"
#include "oops/mpi/mpi.h"
#include "oops/util/Logger.h"

#include "ufo/filters/Variables.h"

namespace opsinputs {
namespace test {

struct CxChecker::PrintCxFileOutput {
  /// Fixed header, integer constants and real constants. Maps field name to its value.
  std::map<std::string, std::string> headerFields;
  std::vector<std::string> etaThetaLevels;
  std::vector<std::string> etaRhoLevels;
  /// Indices of surface variables present in the file, corresponding to constants defined in
  /// OpsMod_CXIndexes.
  std::vector<std::string> surfaceVariables;
  /// Indices of upper-air variables present in the file, corresponding to constants defined in
  /// OpsMod_CXIndexes.
  std::vector<std::string> upperAirVariables;
  /// Element i stores elements of the lookup table for batch (i + 1), as a map of field names
  /// to values.
  std::vector<std::map<std::string, std::string>> lookupFields;
  /// Cx fields using the following indexing: [batch][column][level].
  ///
  /// Note: all fields in a single column are stacked together (the levels of the first field
  /// are followed by those of the second field and so on).
  std::vector<std::vector<std::vector<std::string>>> mainTable;
};


CxChecker::CxChecker(ioda::ObsSpace & obsdb, const eckit::Configuration & config,
                             boost::shared_ptr<ioda::ObsDataVector<int> > flags,
                             boost::shared_ptr<ioda::ObsDataVector<float> > obsErrors)
  : obsdb_(obsdb), geovars_(), flags_(std::move(flags)), obsErrors_(std::move(obsErrors))
{
  oops::Log::trace() << "CxChecker constructor starting" << std::endl;
  parameters_.deserialize(config);
  ASSERT_MSG(!parameters_.expectedHeaderFields.value().empty() ||
             parameters_.expectedEtaThetaLevels.value() != boost::none ||
             parameters_.expectedEtaRhoLevels.value() != boost::none ||
             parameters_.expectedSurfaceVariables.value() != boost::none ||
             parameters_.expectedUpperAirVariables.value() != boost::none ||
             !parameters_.expectedLookupFields.value().empty() ||
             parameters_.expectedMainTableColumns.value() != boost::none,
             "No Cx file components to check have been specified");
}

CxChecker::~CxChecker() {
  oops::Log::trace() << "CxChecker destructor starting" << std::endl;
}

void CxChecker::postFilter(const ioda::ObsVector &, const ufo::ObsDiagnostics &) const {
  oops::Log::trace() << "CxChecker postFilter" << std::endl;

  opsinputs::LocalEnvironment localEnvironment;
  setupEnvironment(localEnvironment);

  const char exeName[] = "OpsProg_PrintCXFile.exe";
  const std::string varObsFileName(getEnvVariableOrThrow("OPS_CX_DIR_LIST") +
            PATH_SEPARATOR + obsdb_.obsname() + ".cx");
  std::string printCxFileOutput = runOpsPrintUtil(exeName, varObsFileName);

  PrintCxFileOutput output = parsePrintCxFileOutput(printCxFileOutput);

  checkHeader(output.headerFields);
  checkVariables(output.surfaceVariables, output.upperAirVariables);
  checkLevelDependentConstants(output.etaThetaLevels, output.etaRhoLevels);
  checkLookup(output.lookupFields);
  checkMainTable(output.mainTable);
}

void CxChecker::setupEnvironment(opsinputs::LocalEnvironment &localEnvironment) const {
  if (parameters_.outputDirectory.value() != boost::none)
    localEnvironment.set("OPS_CX_DIR_LIST", *parameters_.outputDirectory.value());
}

CxChecker::PrintCxFileOutput CxChecker::parsePrintCxFileOutput(
    const std::string &printCxFileOutput) const {
  std::istringstream is(printCxFileOutput);
  std::string line;

  enum FileSection {
    BeforeLevelDependentConstants,
    InLevelDependentConstants,
    InEtaThetaLevels,
    InEtaRhoLevels,
    InColumnDependentConstants,
    InSurfaceVariablesPresent,
    InUpperAirVariablesPresent,
    InLookup,
    InMainTable
  };

  FileSection section = BeforeLevelDependentConstants;

  int batchIndex = 0;
  int columnIndex = 0;
  PrintCxFileOutput output;
  while (std::getline(is, line)) {
    switch (section) {
    case BeforeLevelDependentConstants:
      if (line == "Level Dependent Constants:") {
        section = InLevelDependentConstants;
      } else if (auto nameAndValue = splitAtEqualsSignAndTrim(line)) {
        output.headerFields[nameAndValue->first] = nameAndValue->second;
      }
      break;
    case InLevelDependentConstants:
      if (startsWith(line, "Eta-Theta levels"))
        section = InEtaThetaLevels;
      else if (startsWith(line, "Eta-Rho levels"))
        section = InEtaRhoLevels;
      else if (line == "Column Dependent Constants:")
        section = InColumnDependentConstants;
      break;
    case InEtaThetaLevels:
      output.etaThetaLevels = splitIntoFixedLengthChunksAndTrim(line, 6);
      section = InLevelDependentConstants;
      break;
    case InEtaRhoLevels:
      output.etaRhoLevels = splitIntoFixedLengthChunksAndTrim(line, 6);
      section = InLevelDependentConstants;
      break;
    case InColumnDependentConstants:
      if (line == "Surface variables present:")
        section = InSurfaceVariablesPresent;
      break;
    case InSurfaceVariablesPresent:
      if (line == "Upper-air variables present:")
        section = InUpperAirVariablesPresent;
      else if (startsWith(line, " Variable"))
        output.surfaceVariables.push_back(boost::algorithm::trim_copy(line.substr(10, 5)));
      break;
    case InUpperAirVariablesPresent:
      if (line == "Lookup:")
        section = InLookup;
      else if (startsWith(line, " Variable"))
        output.upperAirVariables.push_back(boost::algorithm::trim_copy(line.substr(10, 5)));
      break;
    case InLookup:
      if (startsWith(line, "Batch number ")) {
        section = InMainTable;
        batchIndex = std::stoi(line.substr(13)) - 1;
        output.mainTable.resize(batchIndex + 1);
      } else if (startsWith(line, "For batch number ")) {
        batchIndex = std::stoi(line.substr(17)) - 1;
        output.lookupFields.resize(batchIndex + 1);
      } else if (auto nameAndValue = splitAtEqualsSignAndTrim(line)) {
        output.lookupFields[batchIndex][nameAndValue->first] = nameAndValue->second;
      }
      break;
    case InMainTable:
      if (startsWith(line, "Batch number ")) {
        batchIndex = std::stoi(line.substr(13)) - 1;
        output.mainTable.resize(batchIndex + 1);
      } else if (startsWith(line, "Column number ")) {
        columnIndex = std::stoi(line.substr(14)) - 1;
        output.mainTable[batchIndex].resize(columnIndex + 1);
      } else if (line.empty() || line == "doesn't exist") {
        // nothing to do
      } else {
        std::vector<std::string> &destination = output.mainTable[batchIndex][columnIndex];
        std::vector<std::string> source = splitIntoFixedLengthChunksAndTrim(line, 10);
        // Long columns may be split into multiple lines, so append to any existing contents
        destination.insert(destination.end(), source.begin(), source.end());
      }
      break;
    }
  }

  return output;
}

void CxChecker::checkHeader(const std::map<std::string, std::string> &headerFields) const {
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

void CxChecker::checkVariables(const std::vector<std::string> &surfaceVariables,
                               const std::vector<std::string> &upperAirVariables) const {
  if (parameters_.expectedSurfaceVariables.value() != boost::none) {
    const std::vector<std::string> &expectedValue = *parameters_.expectedSurfaceVariables.value();
    if (surfaceVariables != expectedValue) {
      std::stringstream str;
      str << "Surface variables:\n  received: " << surfaceVariables
          << "\n  expected: " << expectedValue;
      throw std::runtime_error(str.str());
    }
  }
  if (parameters_.expectedUpperAirVariables.value() != boost::none) {
    const std::vector<std::string> &expectedValue = *parameters_.expectedUpperAirVariables.value();
    if (upperAirVariables != expectedValue) {
      std::stringstream str;
      str << "Upper-air variables:\n  received: " << upperAirVariables
          << "\n  expected: " << expectedValue;
      throw std::runtime_error(str.str());
    }
  }
}

void CxChecker::checkLevelDependentConstants(const std::vector<std::string> &etaThetaLevels,
                                             const std::vector<std::string> &etaRhoLevels) const {
  if (parameters_.expectedEtaThetaLevels.value() != boost::none) {
    const std::vector<std::string> &expectedValue = *parameters_.expectedEtaThetaLevels.value();
    if (etaThetaLevels != expectedValue) {
      std::stringstream str;
      str << "Eta-Theta levels:\n  received: " << etaThetaLevels
          << "\n  expected: " << expectedValue;
      throw std::runtime_error(str.str());
    }
  }
  if (parameters_.expectedEtaRhoLevels.value() != boost::none) {
    const std::vector<std::string> &expectedValue = *parameters_.expectedEtaRhoLevels.value();
    if (etaRhoLevels != expectedValue) {
      std::stringstream str;
      str << "Eta-Theta levels:\n  received: " << etaRhoLevels
          << "\n  expected: " << expectedValue;
      throw std::runtime_error(str.str());
    }
  }
}

void CxChecker::checkLookup(
    const std::vector<std::map<std::string, std::string>> &lookupFields) const {
  // Currently we only support checking lookup fields expected to be the same in all batches
  for (size_t batchIndex = 0; batchIndex < lookupFields.size(); ++batchIndex)
    for (const std::pair<const std::string, std::string> &expectedNameAndValue :
         parameters_.expectedLookupFields.value()) {
      const std::string &value = lookupFields[batchIndex].at(expectedNameAndValue.first);
      if (value != expectedNameAndValue.second) {
        std::stringstream str;
        str << "Lookup field (batch " << batchIndex << ") '"
            << expectedNameAndValue.first << "':\n  received: " << value
            << "\n  expected: " << expectedNameAndValue.second;
        throw std::runtime_error(str.str());
      }
    }
}

void CxChecker::checkMainTable(
    const std::vector<std::vector<std::vector<std::string>>> &mainTable) const {
  if (parameters_.expectedMainTableColumns.value() == boost::none)
    return;

  const std::vector<std::vector<std::vector<std::string>>> &expectedValues =
      *parameters_.expectedMainTableColumns.value();

  if (mainTable.size() < expectedValues.size()) {
    std::stringstream str;
    str << "Expected " << expectedValues.size()
        << " batches, but only " << mainTable.size() << " found ";
    throw std::runtime_error(str.str());
  }

  for (size_t batch = 0; batch < expectedValues.size(); ++batch) {
    if (mainTable[batch].size() < expectedValues[batch].size()) {
      std::stringstream str;
      str << "Batch " << batch << ": expected " << expectedValues[batch].size()
          << " columns, but only " << mainTable[batch].size() << " found ";
      throw std::runtime_error(str.str());
    }

    for (size_t column = 0; column < expectedValues[batch].size(); ++column) {
      if (mainTable[batch][column] != expectedValues[batch][column]) {
        std::stringstream str;
        str << "Batch " << batch << ", column " << column
            << ":\n  received: " << mainTable[batch][column]
            << "\n  expected: " << expectedValues[batch][column];
        throw std::runtime_error(str.str());
      }
    }

    for (size_t column = expectedValues[batch].size(); column < mainTable[batch].size(); ++column) {
      if (!mainTable[batch][column].empty()) {
        std::stringstream str;
        str << "Batch " << batch << ": unexpected non-empty column " << column
            << ":\n  " << mainTable[batch][column];
        throw std::runtime_error(str.str());
      }
    }
  }

  // Cx files seem to contain an extra empty batch at the end.
  // This loop verifies it is indeed empty.
  for (size_t batch = expectedValues.size(); batch < mainTable.size(); ++batch) {
    for (size_t column = 0; column < mainTable[batch].size(); ++column) {
      if (!mainTable[batch][column].empty()) {
        std::stringstream str;
        str << "Batch " << batch << ": unexpected non-empty column " << column
            << ":\n  " << mainTable[batch][column];
        throw std::runtime_error(str.str());
      }
    }
  }
}

void CxChecker::print(std::ostream & os) const {
  os << "CxChecker::print not yet implemented";
}

}  // namespace test
}  // namespace opsinputs
