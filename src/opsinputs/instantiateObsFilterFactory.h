/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details.
 */

#ifndef OPSINPUTS_INSTANTIATEOBSFILTERFACTORY_H_
#define OPSINPUTS_INSTANTIATEOBSFILTERFACTORY_H_

#include "oops/interface/ObsFilterBase.h"
#include "opsinputs/CxWriter.h"
#include "opsinputs/VarObsWriter.h"

namespace opsinputs {

template<typename OBS>
void instantiateObsFilterFactory() {
  static oops::interface::FilterMaker<OBS, VarObsWriter>
    makerVarObsWriter_("VarObs Writer");
  static oops::interface::FilterMaker<OBS, CxWriter>
    makerCxWriter_("Cx Writer");
}

}  // namespace opsinputs

#endif  // OPSINPUTS_INSTANTIATEOBSFILTERFACTORY_H_
