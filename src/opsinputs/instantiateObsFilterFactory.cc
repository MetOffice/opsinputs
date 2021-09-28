/*
 * (C) Crown Copyright 2021, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details.
 */

#include "opsinputs/instantiateObsFilterFactory.h"

#include "oops/interface/ObsFilterBase.h"
#include "opsinputs/CxWriter.h"
#include "opsinputs/VarObsWriter.h"
#include "ufo/ObsTraits.h"

namespace opsinputs {

void instantiateObsFilterFactory() {
  static oops::interface::FilterMaker<ufo::ObsTraits, VarObsWriter>
    makerVarObsWriter_("VarObs Writer");
  static oops::interface::FilterMaker<ufo::ObsTraits, CxWriter>
    makerCxWriter_("Cx Writer");
}

}  // namespace opsinputs
