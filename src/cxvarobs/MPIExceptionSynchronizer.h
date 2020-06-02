#ifndef CXVAROBS_MPIEXCEPTIONSYNCHRONIZER_H_
#define CXVAROBS_MPIEXCEPTIONSYNCHRONIZER_H_

#include <exception>

#include "oops/parallel/mpi/mpi.h"
#include "oops/util/Logger.h"

namespace cxvarobs {

class MPIExceptionSynchronizer
{
public:
  void throwIfAnyProcessHasThrown() {
    int maxNumExceptions = 0;
    oops::Log::debug() << "throwIfAnyProcessHasThrown before reduce (" << oops::mpi::comm().rank() << "/" << oops::mpi::comm().size() << "): " << maxNumExceptions << std::endl;
    oops::mpi::comm().allReduceInPlace(maxNumExceptions, eckit::mpi::Operation::MAX);
    oops::Log::debug() << "throwIfAnyProcessHasThrown after reduce (" << oops::mpi::comm().rank() << "/" << oops::mpi::comm().size() << "): " << maxNumExceptions << std::endl;
    if (maxNumExceptions > 0)
      throw std::runtime_error("An exception has been thrown by another process");
  }

  ~MPIExceptionSynchronizer() noexcept(false) {
    const int numExceptionsOnThisProcess = std::uncaught_exception();
    int maxNumExceptions;
    oops::Log::debug() << "~MPIExceptionSynchronizer before reduce (" << oops::mpi::comm().rank() << "/" << oops::mpi::comm().size() << "): " << numExceptionsOnThisProcess << std::endl;
    oops::mpi::comm().allReduce(numExceptionsOnThisProcess, maxNumExceptions, eckit::mpi::Operation::MAX);
    oops::Log::debug() << "~MPIExceptionSynchronizer after reduce (" << oops::mpi::comm().rank() << "/" << oops::mpi::comm().size() << "): " << maxNumExceptions << std::endl;
    if (maxNumExceptions > 0 && numExceptionsOnThisProcess == 0)
      throw std::runtime_error("An exception has been thrown by another process");
  }
};

}  // namespace cxvarobs

#endif  // CXVAROBS_MPIEXCEPTIONSYNCHRONIZER_H_
