/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <exception>

#include "opsinputs/MPIExceptionSynchronizer.h"
#include "oops/parallel/mpi/mpi.h"
#include "oops/util/Logger.h"

namespace opsinputs {

MPIExceptionSynchronizer::~MPIExceptionSynchronizer() {
  if (!unhealthy_ && std::uncaught_exception()) {
    unhealthy_ = 1;
    int anyUnhealthy;
    oops::mpi::comm().allReduce(unhealthy_, anyUnhealthy, eckit::mpi::Operation::MAX);
  }
}

void MPIExceptionSynchronizer::throwIfAnyProcessHasThrown() {
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

}  // namespace opsinputs
