/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef TEST_OPSINPUTS_MPIEXCEPTIONSYNCHRONIZER_H_
#define TEST_OPSINPUTS_MPIEXCEPTIONSYNCHRONIZER_H_

#include <string>

#include "opsinputs/MPIExceptionSynchronizer.h"
#include "eckit/testing/Test.h"
#include "oops/../test/TestEnvironment.h"
#include "oops/parallel/mpi/mpi.h"
#include "oops/runs/Test.h"
#include "oops/util/Expect.h"

namespace opsinputs {
namespace test {

void noException() {
  opsinputs::MPIExceptionSynchronizer synchronizer;

  int term = 1;
  int sum;

  synchronizer.throwIfAnyProcessHasThrown();
  oops::mpi::comm().allReduce(term, sum, eckit::mpi::Operation::SUM);

  EXPECT_EQUAL(sum, oops::mpi::comm().size());

  synchronizer.throwIfAnyProcessHasThrown();
  int product;
  oops::mpi::comm().allReduce(term, product, eckit::mpi::Operation::PROD);

  EXPECT_EQUAL(product, 1);
}

void exceptionBeforeFirstMPICall() {
  opsinputs::MPIExceptionSynchronizer synchronizer;

  int term = 1;

  if (oops::mpi::comm().rank() == 0)
    throw std::runtime_error("An exception in one process only");

  synchronizer.throwIfAnyProcessHasThrown();
  int sum;
  oops::mpi::comm().allReduce(term, sum, eckit::mpi::Operation::SUM);

  EXPECT_EQUAL(sum, oops::mpi::comm().size());

  synchronizer.throwIfAnyProcessHasThrown();
  int product;
  oops::mpi::comm().allReduce(term, product, eckit::mpi::Operation::PROD);

  EXPECT_EQUAL(product, 1);
}

void exceptionBeforeSecondMPICall() {
  opsinputs::MPIExceptionSynchronizer synchronizer;

  int term = 1;

  synchronizer.throwIfAnyProcessHasThrown();
  int sum;
  oops::mpi::comm().allReduce(term, sum, eckit::mpi::Operation::SUM);

  EXPECT_EQUAL(sum, oops::mpi::comm().size());

  if (oops::mpi::comm().rank() == 0)
    throw std::runtime_error("An exception in one process only");

  synchronizer.throwIfAnyProcessHasThrown();
  int product;
  oops::mpi::comm().allReduce(term, product, eckit::mpi::Operation::PROD);

  EXPECT_EQUAL(product, 1);
}

CASE("opsinputs/MPIExceptionSynchronizer/No exception") {
  EXPECT_NO_THROW(noException());
}

CASE("opsinputs/MPIExceptionSynchronizer/Exception before first MPI call") {
  EXPECT_THROWS_AS(exceptionBeforeFirstMPICall(), std::runtime_error);
}

CASE("opsinputs/MPIExceptionSynchronizer/Exception before second MPI call") {
  EXPECT_THROWS_AS(exceptionBeforeSecondMPICall(), std::runtime_error);
}

class MPIExceptionSynchronizer : public oops::Test {
 private:
  std::string testid() const override {return "opsinputs::test::MPIExceptionSynchronizer";}

  void register_tests() const override {}
};

}  // namespace test
}  // namespace opsinputs

#endif  // TEST_OPSINPUTS_MPIEXCEPTIONSYNCHRONIZER_H_
