/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef TEST_OPSINPUTS_INSTANTIATEOBSFILTERFACTORY_H_
#define TEST_OPSINPUTS_INSTANTIATEOBSFILTERFACTORY_H_

#include "../opsinputs/ResetFlagsToPass.h"
#include "../opsinputs/VarObsChecker.h"
#include "oops/interface/ObsFilter.h"

namespace opsinputs {
namespace test {

template<typename MODEL>
void instantiateObsFilterFactory() {
  static oops::FilterMaker<MODEL, oops::ObsFilter<MODEL, VarObsChecker> >
    varObsCheckerMaker("VarObs Checker");
  static oops::FilterMaker<MODEL, oops::ObsFilter<MODEL, ResetFlagsToPass> >
    resetFlagsToPassMaker("Reset Flags to Pass");
}

}  // namespace test
}  // namespace opsinputs

#endif  // TEST_OPSINPUTS_INSTANTIATEOBSFILTERFACTORY_H_
