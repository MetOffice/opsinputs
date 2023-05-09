/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 */

#include "../opsinputs/MPIExceptionSynchronizer.h"
#include "oops/runs/Run.h"

int main(int argc,  char ** argv) {
  oops::Run run(argc, argv);
  opsinputs::test::MPIExceptionSynchronizer tests;
  return run.execute(tests);
}
