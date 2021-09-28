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
#include "ufo/ObsTraits.h"

namespace opsinputs {
namespace test {

inline void instantiateObsFilterFactory() {
  static oops::interface::FilterMaker<ufo::ObsTraits, VarObsChecker>
    varObsCheckerMaker("VarObs Checker");
  static oops::interface::FilterMaker<ufo::ObsTraits, CxChecker>
    cxCheckerMaker("Cx Checker");
  static oops::interface::FilterMaker<ufo::ObsTraits, ResetFlagsToPass>
    resetFlagsToPassMaker("Reset Flags to Pass");
}

}  // namespace test
}  // namespace opsinputs

#endif  // TEST_OPSINPUTS_INSTANTIATEOBSFILTERFACTORY_H_
