/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef CXVAROBS_INSTANTIATEOBSFILTERFACTORY_H_
#define CXVAROBS_INSTANTIATEOBSFILTERFACTORY_H_

#include "cxvarobs/VarObsWriter.h"
#include "oops/interface/ObsFilter.h"
#include "ufo/instantiateObsFilterFactory.h"

namespace cxvarobs {

template<typename MODEL> 
void instantiateObsFilterFactory() {
  ufo::instantiateObsFilterFactory<MODEL>();
  static oops::FilterMaker<MODEL, oops::ObsFilter<MODEL, VarObsWriter> >
    makerVarObsWriter_("VarObsWriter");
}

}  // namespace cxvarobs

#endif  // CXVAROBS_INSTANTIATEOBSFILTERFACTORY_H_
