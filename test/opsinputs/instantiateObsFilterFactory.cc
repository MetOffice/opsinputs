/*
 * (C) Crown Copyright 2021, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details. 
 */

// This rather strange relative path is required by the opsinputs_coding_norms test...
#include "../../test/opsinputs/instantiateObsFilterFactory.h"

#include "../opsinputs/CxChecker.h"
#include "../opsinputs/ResetFlagsToPass.h"
#include "../opsinputs/VarObsChecker.h"
#include "oops/interface/ObsFilterBase.h"
#include "ufo/ObsTraits.h"

namespace opsinputs {
namespace test {

void instantiateObsFilterFactory() {
  static oops::interface::FilterMaker<ufo::ObsTraits, VarObsChecker>
    varObsCheckerMaker("VarObs Checker");
  static oops::interface::FilterMaker<ufo::ObsTraits, CxChecker>
    cxCheckerMaker("Cx Checker");
  static oops::interface::FilterMaker<ufo::ObsTraits, ResetFlagsToPass>
    resetFlagsToPassMaker("Reset Flags to Pass");
}

}  // namespace test
}  // namespace opsinputs
