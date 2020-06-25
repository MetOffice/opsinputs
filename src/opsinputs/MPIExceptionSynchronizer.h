/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef OPSINPUTS_MPIEXCEPTIONSYNCHRONIZER_H_
#define OPSINPUTS_MPIEXCEPTIONSYNCHRONIZER_H_

#include <exception>

#include "oops/parallel/mpi/mpi.h"
#include "oops/util/Logger.h"

namespace opsinputs {

/// \brief Prevents deadlocks in MPI code throwing exceptions.
///
/// Usage: Create an MPIExceptionSynchronizer instance at the beginning of a code section that calls
/// MPI functions and may throw exceptions. Precede each call to an MPI function by a call to
/// throwIfAnyProcessHasThrown(). The latter will throw an exception if one has been thrown in the
/// meantime by any other MPI process. This ensures that the MPI function will be called either by
/// all MPI processes or none, thus avoiding a deadlock (provided that an MPIExceptionSynchroniser
/// object has been constructed on each MPI process).
///
/// Note: an exception thrown by throwIfAnyProcess
class MPIExceptionSynchronizer {
 public:
  MPIExceptionSynchronizer() = default;
  MPIExceptionSynchronizer(const MPIExceptionSynchronizer &) = delete;
  MPIExceptionSynchronizer & operator=(const MPIExceptionSynchronizer &) = delete;
  ~MPIExceptionSynchronizer();

  /// Throw an exception if any other MPI process has thrown an exception (which hadn't been
  /// caught before the MPIExceptionSynchronizer instance on that process went out of scope) since
  /// the construction of this object or the previous call to this function.
  ///
  /// If this function throws an exception, it shouldn't be called any more on the same
  /// MPIExceptionSynchroniser instance. In other words, that exception should normally not be
  /// caught within the scope where the MPIExceptionSynchroniser instance is defined.
  void throwIfAnyProcessHasThrown();

 private:
  int unhealthy_ = 0;
};

}  // namespace opsinputs

#endif  // OPSINPUTS_MPIEXCEPTIONSYNCHRONIZER_H_
