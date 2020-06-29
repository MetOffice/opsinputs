/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <cstdlib>
#include <fstream>
#include <sstream>

#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/trim.hpp>

#include "../../test/opsinputs/CheckerUtils.h"
#include "../../test/opsinputs/TempFile.h"

#include "opsinputs/MPIExceptionSynchronizer.h"

#include "oops/parallel/mpi/mpi.h"
#include "oops/util/Logger.h"

namespace opsinputs {
namespace test {

namespace {

std::string readTextFile(const std::string &fileName) {
  std::ostringstream sstream;
  std::ifstream fstream(fileName);
  sstream << fstream.rdbuf();
  return sstream.str();
}

}  // namespace

std::string getEnvVariableOrThrow(const char *variableName) {
  const char *value = std::getenv(variableName);
  if (!value)
    throw std::runtime_error("Environment variable '" + std::string(variableName) + "' not set");
  return value;
}

bool startsWith(const std::string &string, const char *prefix) {
  return boost::algorithm::starts_with(string, prefix);
}

boost::optional<std::pair<std::string, std::string>> splitAtEqualsSignAndTrim(
    const std::string &line) {
  const char separator[] = " = ";
  std::string::size_type separatorPos = line.find(separator);
  if (separatorPos != std::string::npos) {
    std::string name = line.substr(0, separatorPos);
    std::string value = line.substr(separatorPos + sizeof(separator) - 1);
    boost::algorithm::trim(name);
    boost::algorithm::trim(value);
    return std::make_pair(std::move(name), std::move(value));
  }
  return boost::none;
}

std::vector<std::string> splitIntoFixedLengthChunksAndTrim(const std::string &line,
                                                           size_t chunkSize) {
  const size_t numChunks = line.size() / chunkSize;
  std::vector<std::string> chunks(numChunks);
  for (size_t i = 0; i < numChunks; ++i) {
    chunks[i] = line.substr(i * chunkSize, chunkSize);
    boost::algorithm::trim(chunks[i]);
  }
  return chunks;
}

std::string runOpsPrintUtil(const char *printUtilName, const std::string &inputFilePath) {
  MPIExceptionSynchronizer exceptionSynchronizer;

  const size_t rootProcessRank = 0;

  // Construct a name of a temporary file

  char tempFileName[L_tmpnam];
  std::unique_ptr<TempFile> tempFile;
  if (oops::mpi::comm().rank() == rootProcessRank) {
    std::tmpnam(tempFileName);
    tempFile.reset(new TempFile(tempFileName));
  }
  exceptionSynchronizer.throwIfAnyProcessHasThrown();
  oops::mpi::comm().broadcast(tempFileName, L_tmpnam, rootProcessRank);

  // Run the OPS tool

  if (oops::mpi::comm().rank() == rootProcessRank) {
    const eckit::PathName inputFilePathName(inputFilePath);
    if (!inputFilePathName.exists())
      throw std::runtime_error("File '" + inputFilePathName + "' not found");

    std::string exePath;
    if (char *dir = getenv("OPSINPUTS_OPS_BIN_DIR")) {
      exePath = dir;
      exePath += PATH_SEPARATOR;
      exePath += printUtilName;
    } else {
      exePath = printUtilName;
    }

    // TODO(wsmigaj): perhaps read the name of the MPI runner from an environment variable
    const std::string cmd = "mpiexec -n 1 " + exePath + " \"" + inputFilePathName +
        "\" --all --outfile=\"" + tempFile->name() + "\"";
    if (oops::mpi::comm().rank() == 0) {
      oops::Log::info() << "Running " << cmd << "\n";
      const int exitCode = std::system(cmd.c_str());
      if (exitCode != 0)
        throw std::runtime_error(std::string(printUtilName) + " failed with exit code " +
                                 std::to_string(exitCode));
    }
  }

  exceptionSynchronizer.throwIfAnyProcessHasThrown();
  // Ensure rootProcessRank has written the file
  oops::mpi::comm().barrier();

  // Read the temporary file into a string.

  std::string result = readTextFile(tempFileName);

  exceptionSynchronizer.throwIfAnyProcessHasThrown();
  // Ensure all processes have read the file before rootProcessRank deletes it
  // as TempFile goes out of scope
  oops::mpi::comm().barrier();

  return result;
}

}  // namespace test
}  // namespace opsinputs
