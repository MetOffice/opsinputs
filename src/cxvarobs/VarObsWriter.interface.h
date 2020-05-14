/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef CXVAROBS_VAROBSWRITER_INTERFACE_H_
#define CXVAROBS_VAROBSWRITER_INTERFACE_H_

#include "ioda/ObsSpace.h"
#include "oops/base/Variables.h"
#include "ufo/Fortran.h"

// Forward declarations
namespace eckit {
  class Configuration;
}

namespace ioda {
  class ObsSpace;
  template <typename T> class ObsDataVector;
}

namespace cxvarobs {

typedef int F90check;

/// Interface to Fortran routines

extern "C" {
  void cxvarobs_varobswriter_create_f90(F90check &, const eckit::Configuration *,
                              oops::Variables &);
  void cxvarobs_varobswriter_delete_f90(F90check &);
  void cxvarobs_varobswriter_prior_f90(const F90check &, const ioda::ObsSpace &,
                                       const ufo::F90goms &);
  void cxvarobs_varobswriter_post_f90(const F90check &self,
                                      const ioda::ObsSpace &obsSpace,
                                      const ioda::ObsDataVector<int> &flags,
                                      const ioda::ObsDataVector<float> &obsErrors,
                                      const int &nvars, const int &nlocs, const double &hofx);
}  // extern C

}  // namespace cxvarobs

#endif  // CXVAROBS_VAROBSWRITER_INTERFACE_H_
