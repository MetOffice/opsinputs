/*
 * (C) Copyright 2020 UCAR
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#include "../test/ufo/ObsFilters.h"
#include "cxvarobs/instantiateObsFilterFactory.h"
#include "oops/runs/Run.h"
#include "ufo/instantiateObsFilterFactory.h"
#include "ufo/UfoTrait.h"

int main(int argc,  char ** argv) {
  oops::Run run(argc, argv);
  ufo::instantiateObsFilterFactory<ufo::UfoTrait>();
  cxvarobs::instantiateObsFilterFactory<ufo::UfoTrait>();
  ufo::test::ObsFilters tests;
  return run.execute(tests);
}
