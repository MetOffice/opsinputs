/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 */

#ifndef OPSINPUTS_CXWRITER_INTERFACE_H_
#define OPSINPUTS_CXWRITER_INTERFACE_H_

#include "ioda/ObsSpace.h"
#include "oops/base/ObsVariables.h"
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
  int opsinputs_cxwriter_create_f90(F90check &, const eckit::Configuration *,
                                    oops::Variables &);
  void opsinputs_cxwriter_delete_f90(F90check &);
  void opsinputs_cxwriter_prior_f90(const F90check &, const ioda::ObsSpace &,
                                    const ufo::F90goms &);
  void opsinputs_cxwriter_post_f90(const F90check &self,
                                   const ioda::ObsSpace &obsSpace,
                                   const ufo::F90goms &,
                                   const ioda::ObsDataVector<int> &flags,
                                   const int &nvars, const int &nlocs,
                                   const oops::ObsVariables &varnames, const double *hofx);
}  // extern C

}  // namespace opsinputs

#endif  // OPSINPUTS_CXWRITER_INTERFACE_H_
