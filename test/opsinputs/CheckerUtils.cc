/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details.
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

#include "oops/mpi/mpi.h"
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

std::string quoted(const std::string &s) {
  return '"' + s + '"';
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
  if (oops::mpi::world().rank() == rootProcessRank) {
    std::tmpnam(tempFileName);
    tempFile.reset(new TempFile(tempFileName));
  }
  exceptionSynchronizer.throwIfAnyProcessHasThrown();
  oops::mpi::world().broadcast(tempFileName, L_tmpnam, rootProcessRank);

  // Run the OPS tool

  if (oops::mpi::world().rank() == rootProcessRank) {
    const eckit::PathName inputFilePathName(inputFilePath);
    if (!inputFilePathName.exists())
      throw std::runtime_error("File '" + inputFilePathName + "' not found");

    std::string cmd;
    if (char *runner = getenv("OPSINPUTS_OPSPROG_RUNNER")) {
      cmd += quoted(runner) + ' ';
    }
    cmd += quoted(printUtilName) + ' ';
    cmd += quoted(inputFilePathName);
    cmd += " --all ";
    cmd += quoted("--outfile=" + tempFile->name());
    oops::Log::info() << "Running " << cmd << "\n";
    const int exitCode = std::system(cmd.c_str());
    if (exitCode != 0)
      throw std::runtime_error(std::string(printUtilName) + " failed with exit code " +
                               std::to_string(exitCode));
  }

  exceptionSynchronizer.throwIfAnyProcessHasThrown();
  // Ensure rootProcessRank has written the file
  oops::mpi::world().barrier();

  // Read the temporary file into a string.

  std::string result = readTextFile(tempFileName);

  exceptionSynchronizer.throwIfAnyProcessHasThrown();
  // Ensure all processes have read the file before rootProcessRank deletes it
  // as TempFile goes out of scope
  oops::mpi::world().barrier();

  return result;
}

}  // namespace test
}  // namespace opsinputs
