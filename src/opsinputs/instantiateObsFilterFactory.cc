/*
 * (C) Crown Copyright 2021, the Met Office. All rights reserved.
 *
 */

#include "opsinputs/instantiateObsFilterFactory.h"

#include "opsinputs/CxWriter.h"
#include "opsinputs/VarObsWriter.h"
#include "ufo/ObsFilterBase.h"

namespace opsinputs {

void instantiateObsFilterFactory() {
  static ufo::FilterMaker<VarObsWriter>
    makerVarObsWriter_("VarObs Writer");
  static ufo::FilterMaker<CxWriter>
    makerCxWriter_("Cx Writer");
}

}  // namespace opsinputs
