#ifndef CXVAROBS_MPIEXCEPTIONSYNCHRONIZER_H_
#define CXVAROBS_MPIEXCEPTIONSYNCHRONIZER_H_

#include <exception>

#include "oops/parallel/mpi/mpi.h"
#include "oops/util/Logger.h"

namespace cxvarobs {

class MPIExceptionSynchronizer {
public:
  void throwIfAnyProcessHasThrown() {
    if (unhealthy_)
      throw std::logic_error("You shouldn't call throwIfAnyProcessHasThrown() "
                             "if a previous call to this function has thrown an exception");
    int anyUnhealthy;
    oops::mpi::comm().allReduce(unhealthy_, anyUnhealthy, eckit::mpi::Operation::MAX);
    if (anyUnhealthy) {
      unhealthy_ = 1;
      throw std::runtime_error("An exception has been thrown by another MPI process");
    }
  }

  ~MPIExceptionSynchronizer() {
    if (!unhealthy_ && std::uncaught_exception()) {
      unhealthy_ = 1;
      int anyUnhealthy;
      oops::mpi::comm().allReduce(unhealthy_, anyUnhealthy, eckit::mpi::Operation::MAX);
    }
  }

 private:
  int unhealthy_ = 0;
};

}  // namespace cxvarobs

#endif  // CXVAROBS_MPIEXCEPTIONSYNCHRONIZER_H_
