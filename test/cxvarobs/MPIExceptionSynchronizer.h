/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef TEST_CXVAROBS_MPIEXCEPTIONSYNCHRONIZER_H_
#define TEST_CXVAROBS_MPIEXCEPTIONSYNCHRONIZER_H_

#include "cxvarobs/MPIExceptionSynchronizer.h"
#include "eckit/testing/Test.h"
#include "oops/../test/TestEnvironment.h"
#include "oops/parallel/mpi/mpi.h"
#include "oops/runs/Test.h"
#include "oops/util/Expect.h"

namespace cxvarobs {
namespace test {

void noException() {
  cxvarobs::MPIExceptionSynchronizer synchronizer;

  int term = 1;
  int sum;

  synchronizer.throwIfAnyProcessHasThrown();
  oops::mpi::comm().allReduce(term, sum, eckit::mpi::Operation::SUM);

  EXPECT_EQUAL(sum, oops::mpi::comm().size());
}

void exceptionBeforeMPICall() {
  cxvarobs::MPIExceptionSynchronizer synchronizer;

  int term = 1;
  int sum;

  if (oops::mpi::comm().rank() == 0)
    throw std::runtime_error("An exception in one process only");

  synchronizer.throwIfAnyProcessHasThrown();
  oops::mpi::comm().allReduce(term, sum, eckit::mpi::Operation::SUM);

  EXPECT_EQUAL(sum, oops::mpi::comm().size());
}

void exceptionAfterMPICall() {
  cxvarobs::MPIExceptionSynchronizer synchronizer;

  int term = 1;
  int sum;

  synchronizer.throwIfAnyProcessHasThrown();
  oops::mpi::comm().allReduce(term, sum, eckit::mpi::Operation::SUM);

  EXPECT_EQUAL(sum, oops::mpi::comm().size());

  if (oops::mpi::comm().rank() == 0)
    throw std::runtime_error("An exception in one process only");
}

CASE("cxvarobs/MPIExceptionSynchronizer/No exception") {
  EXPECT_NO_THROW(noException());
}

CASE("cxvarobs/MPIExceptionSynchronizer/Exception before MPI call") {
  EXPECT_THROWS_AS(exceptionBeforeMPICall(), std::runtime_error);
}

CASE("cxvarobs/MPIExceptionSynchronizer/Exception after MPI call") {
  EXPECT_THROWS_AS(exceptionAfterMPICall(), std::runtime_error);
}

class MPIExceptionSynchronizer : public oops::Test {
 private:
  std::string testid() const override {return "cxvarobs::test::MPIExceptionSynchronizer";}

  void register_tests() const override {}
};

}  // namespace test
}  // namespace cxvarobs

#endif  // TEST_CXVAROBS_MPIEXCEPTIONSYNCHRONIZER_H_
