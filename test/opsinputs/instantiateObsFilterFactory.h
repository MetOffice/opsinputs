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
#include "oops/interface/ObsFilterBase.h"

namespace opsinputs {
namespace test {

template<typename OBS>
void instantiateObsFilterFactory() {
  static oops::interface::FilterMaker<OBS, VarObsChecker>
    varObsCheckerMaker("VarObs Checker");
  static oops::interface::FilterMaker<OBS, CxChecker>
    cxCheckerMaker("Cx Checker");
  static oops::interface::FilterMaker<OBS, ResetFlagsToPass>
    resetFlagsToPassMaker("Reset Flags to Pass");
}

}  // namespace test
}  // namespace opsinputs

#endif  // TEST_OPSINPUTS_INSTANTIATEOBSFILTERFACTORY_H_
