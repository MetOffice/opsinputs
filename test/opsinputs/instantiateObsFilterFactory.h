/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details.
 */

#ifndef TEST_OPSINPUTS_INSTANTIATEOBSFILTERFACTORY_H_
#define TEST_OPSINPUTS_INSTANTIATEOBSFILTERFACTORY_H_

#include "../opsinputs/CxChecker.h"
#include "../opsinputs/ResetFlagsToPass.h"
#include "../opsinputs/VarObsChecker.h"
#include "oops/interface/ObsFilter.h"

namespace opsinputs {
namespace test {

template<typename MODEL>
void instantiateObsFilterFactory() {
  static oops::FilterMaker<MODEL, oops::ObsFilter<MODEL, VarObsChecker> >
    varObsCheckerMaker("VarObs Checker");
  static oops::FilterMaker<MODEL, oops::ObsFilter<MODEL, CxChecker> >
    cxCheckerMaker("Cx Checker");
  static oops::FilterMaker<MODEL, oops::ObsFilter<MODEL, ResetFlagsToPass> >
    resetFlagsToPassMaker("Reset Flags to Pass");
}

}  // namespace test
}  // namespace opsinputs

#endif  // TEST_OPSINPUTS_INSTANTIATEOBSFILTERFACTORY_H_
