/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef TEST_CXVAROBS_INSTANTIATEOBSFILTERFACTORY_H_
#define TEST_CXVAROBS_INSTANTIATEOBSFILTERFACTORY_H_

#include "../cxvarobs/ResetFlagsToPass.h"
#include "../cxvarobs/VarObsChecker.h"
#include "oops/interface/ObsFilter.h"

namespace cxvarobs {
namespace test {

template<typename MODEL>
void instantiateObsFilterFactory() {
  static oops::FilterMaker<MODEL, oops::ObsFilter<MODEL, VarObsChecker> >
    varObsCheckerMaker("VarObs Checker");
  static oops::FilterMaker<MODEL, oops::ObsFilter<MODEL, ResetFlagsToPass> >
    resetFlagsToPassMaker("Reset Flags to Pass");
}

}  // namespace test
}  // namespace cxvarobs

#endif  // TEST_CXVAROBS_INSTANTIATEOBSFILTERFACTORY_H_
