/*
 * (C) Crown Copyright 2021, the Met Office. All rights reserved.
 *
 */

// This rather strange relative path is required by the opsinputs_coding_norms test...
#include "../../test/opsinputs/instantiateObsFilterFactory.h"

#include "../opsinputs/CxChecker.h"
#include "../opsinputs/ResetFlagsToPass.h"
#include "../opsinputs/VarObsChecker.h"
#include "ufo/ObsFilterBase.h"
#include "ufo/ObsTraits.h"

namespace opsinputs {
namespace test {

void instantiateObsFilterFactory() {
  static ufo::FilterMaker<VarObsChecker>
    varObsCheckerMaker("VarObs Checker");
  static ufo::FilterMaker<CxChecker>
    cxCheckerMaker("Cx Checker");
  static ufo::FilterMaker<ResetFlagsToPass>
    resetFlagsToPassMaker("Reset Flags to Pass");
}

}  // namespace test
}  // namespace opsinputs
