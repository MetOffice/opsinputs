/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details.
 */

#include <cstdlib>
#include <fstream>
#include <list>
#include <sstream>
// NOLINTNEXTLINE(build/c++11)  [Apparently 'thread' isn't among Google "approved" C++11 headers]
#include <thread>

#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/trim.hpp>

#include "../../test/opsinputs/CheckerUtils.h"
#include "../../test/opsinputs/TempFile.h"

#include "eckit/mpi/Comm.h"
#include "eckit/mpi/Parallel.h"

#include "oops/mpi/mpi.h"
#include "oops/util/Logger.h"
#include "oops/util/stringFunctions.h"

#include "opsinputs/MPIExceptionSynchronizer.h"

namespace opsinputs {
namespace test {

namespace {

std::string readTextFile(const std::string &fileName) {
  std::ostringstream sstream;
  std::ifstream fstream(fileName);
  sstream << fstream.rdbuf();
  return sstream.str();
}

/// Try to open the specified file. If that fails, return boost::none, otherwise read and return
/// a single integer representing an application's exit code.
boost::optional<int> tryToReadExitCodeFrom(const char *fileName) {
  std::ifstream f(fileName);
  if (!f.good())
    return boost::none;  // Opening file failed
  // Initialise to non-zero so that in the unlikely case of a failure of the read operation
  // we return a non-zero exit code
  int exitCode = 1;
  f >> exitCode;
  return exitCode;
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

  // Construct the name of a temporary file that will receive the print util's output

  char outputFilePath[L_tmpnam];
  std::unique_ptr<TempFile> outputFile;
  if (oops::mpi::world().rank() == rootProcessRank) {
    std::tmpnam(outputFilePath);
    outputFile.reset(new TempFile(outputFilePath));
  }
  exceptionSynchronizer.throwIfAnyProcessHasThrown();
  oops::mpi::world().broadcast(outputFilePath, L_tmpnam, rootProcessRank);

  // Run the OPS tool

  if (oops::mpi::world().rank() == rootProcessRank) {
    const eckit::PathName inputFilePathName(inputFilePath);
    if (!inputFilePathName.exists())
      throw std::runtime_error("File '" + inputFilePathName + "' not found");

    // Construct the name of a temporary file that will receive the print util's exit code
    char exitCodeFilePath[L_tmpnam];
    std::tmpnam(exitCodeFilePath);
    TempFile exitCodeFile(exitCodeFilePath);

    // Build the list of arguments of the application to run
    std::list<std::string> exeAndArgs;
    if (char *runner = getenv("OPSINPUTS_OPSPROG_RUNNER")) {
      exeAndArgs.push_back(runner);
    } else {
      throw eckit::UserError("The OPSINPUTS_OPSPROG_RUNNER environment variable must contain "
                             "the path to the opsprog_runner.sh script", Here());
    }

    exeAndArgs.push_back(exitCodeFilePath);
    exeAndArgs.push_back(printUtilName);
    exeAndArgs.push_back(inputFilePath);
    exeAndArgs.push_back("--all");
    exeAndArgs.push_back("--outfile=" + outputFile->name());

    const eckit::mpi::Comm &myself = oops::mpi::myself();

    int exitCode;
    if (auto parallelComm = dynamic_cast<const eckit::mpi::Parallel*>(&myself)) {
      // We're being run by an MPI application. We can't call system() to spawn a process running
      // the print util (especially since it's another MPI application); instead, we have to call
      // MPI_Comm_spawn().

      std::vector<char*> args;
      for (auto it = ++exeAndArgs.begin(); it != exeAndArgs.end(); ++it)
        args.push_back(const_cast<char*>(it->c_str()));
      args.push_back(nullptr);

      oops::Log::info() << "Running";
      for (const std::string &s : exeAndArgs)
        oops::Log::info() << " \"" << s << "\"";
      oops::Log::info() << std::endl;

      // Spawn the runner as a child process
      MPI_Comm childComm;
      int spawnError;
      MPI_Comm_spawn(exeAndArgs.front().c_str(), args.data(), 1 /*maxProcs*/, MPI_INFO_NULL,
          myself.rank(), MPI_COMM_SELF, &childComm, &spawnError);
      if (spawnError != MPI_SUCCESS)
        throw eckit::UnexpectedState("MPI_Comm_spawn failed with error code " +
                                     std::to_string(spawnError), Here());

      // There doesn't seem to be a way of being notified when the process launched by
      // MPI_Comm_spawn() completes -- at least given that the OPS print utils call MPI_Finalize()
      // at the end, which makes it impossible to use MPI to communicate with the child process. So
      // we resort to communicating through files and poll the filesystem until the runner script
      // creates a file containing the print tool's exit status.
      boost::optional<int> optExitCode;
      while (optExitCode == boost::none) {
        optExitCode = tryToReadExitCodeFrom(exitCodeFilePath);
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
      }
      exitCode = *optExitCode;
    } else {
      // We're being run by a non-MPI application. We can call system() and mpiexec to run the
      // print util.
      exeAndArgs.push_front("1");
      exeAndArgs.push_front("-n");
      exeAndArgs.push_front("mpiexec");
      const std::string cmd = util::stringfunctions::join(
            " ", exeAndArgs.begin(), exeAndArgs.end(),
            [](const std::string &s) { return '"' + s + '"'; });

      oops::Log::info() << "Running " << cmd << std::endl;
      exitCode = std::system(cmd.c_str());
    }
    if (exitCode != 0)
      throw std::runtime_error(std::string(printUtilName) + " failed with exit code " +
                               std::to_string(exitCode));
  }

  exceptionSynchronizer.throwIfAnyProcessHasThrown();
  // Ensure rootProcessRank has written the file
  oops::mpi::world().barrier();

  // Read the temporary file into a string.

  std::string result = readTextFile(outputFilePath);

  exceptionSynchronizer.throwIfAnyProcessHasThrown();
  // Ensure all processes have read the file before rootProcessRank deletes it
  // as TempFile goes out of scope
  oops::mpi::world().barrier();

  return result;
}

}  // namespace test
}  // namespace opsinputs
