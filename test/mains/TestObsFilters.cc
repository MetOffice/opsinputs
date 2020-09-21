/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details.
 */

#include "../opsinputs/instantiateObsFilterFactory.h"
#include "../test/ufo/ObsFilters.h"
#include "oops/runs/Run.h"
#include "opsinputs/instantiateObsFilterFactory.h"
#include "ufo/instantiateObsFilterFactory.h"
#include "ufo/ObsTraits.h"

int main(int argc,  char ** argv) {
  oops::Run run(argc, argv);
  ufo::instantiateObsFilterFactory<ufo::ObsTraits>();
  opsinputs::instantiateObsFilterFactory<ufo::ObsTraits>();
  opsinputs::test::instantiateObsFilterFactory<ufo::ObsTraits>();
  ufo::test::ObsFilters tests;
  return run.execute(tests);
}
