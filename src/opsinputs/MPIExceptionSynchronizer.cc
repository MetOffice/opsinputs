/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details.
 */

#include <exception>

#include "oops/mpi/mpi.h"
#include "oops/util/Logger.h"
#include "opsinputs/MPIExceptionSynchronizer.h"

namespace opsinputs {

MPIExceptionSynchronizer::~MPIExceptionSynchronizer() {
  if (!unhealthy_ && std::uncaught_exception()) {
    unhealthy_ = 1;
    int anyUnhealthy;
    oops::mpi::world().allReduce(unhealthy_, anyUnhealthy, eckit::mpi::Operation::MAX);
  }
}

void MPIExceptionSynchronizer::throwIfAnyProcessHasThrown() {
  if (unhealthy_)
    throw std::logic_error("You shouldn't call throwIfAnyProcessHasThrown() "
                           "if a previous call to this function has thrown an exception");
  int anyUnhealthy;
  oops::mpi::world().allReduce(unhealthy_, anyUnhealthy, eckit::mpi::Operation::MAX);
  if (anyUnhealthy) {
    unhealthy_ = 1;
    throw std::runtime_error("An exception has been thrown by another MPI process");
  }
}

}  // namespace opsinputs
