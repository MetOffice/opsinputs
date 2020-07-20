/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
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
