/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details.
 */

#ifndef OPSINPUTS_VAROBSWRITER_INTERFACE_H_
#define OPSINPUTS_VAROBSWRITER_INTERFACE_H_

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

namespace opsinputs {

typedef int F90check;

/// Interface to Fortran routines

extern "C" {
  int opsinputs_varobswriter_create_f90(F90check &, const eckit::Configuration *,
                                        const size_t &comm,
                                        oops::Variables &);
  void opsinputs_varobswriter_delete_f90(F90check &);
  void opsinputs_varobswriter_prior_f90(const F90check &, const ioda::ObsSpace &,
                                       const ufo::F90goms &);
  void opsinputs_varobswriter_post_f90(const F90check &self,
                                      const ioda::ObsSpace &obsSpace,
                                      const int &nchannels,
                                      const int *channels,
                                      const ioda::ObsDataVector<int> &flags,
                                      const ioda::ObsDataVector<float> &obsErrors,
                                      const int &nvars, const int &nlocs, const double &hofx);
}  // extern C

}  // namespace opsinputs

#endif  // OPSINPUTS_VAROBSWRITER_INTERFACE_H_
