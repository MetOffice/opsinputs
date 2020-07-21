/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef OPSINPUTS_INSTANTIATEOBSFILTERFACTORY_H_
#define OPSINPUTS_INSTANTIATEOBSFILTERFACTORY_H_

#include "oops/interface/ObsFilter.h"
#include "opsinputs/CxWriter.h"
#include "opsinputs/VarObsWriter.h"

namespace opsinputs {

template<typename MODEL>
void instantiateObsFilterFactory() {
  static oops::FilterMaker<MODEL, oops::ObsFilter<MODEL, VarObsWriter> >
    makerVarObsWriter_("VarObs Writer");
  static oops::FilterMaker<MODEL, oops::ObsFilter<MODEL, CxWriter> >
    makerCxWriter_("Cx Writer");
}

}  // namespace opsinputs

#endif  // OPSINPUTS_INSTANTIATEOBSFILTERFACTORY_H_
