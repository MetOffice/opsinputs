/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 */

#ifndef OPSINPUTS_OPSINPUTS_OBSDATAVECTOR_F_H_
#define OPSINPUTS_OPSINPUTS_OBSDATAVECTOR_F_H_

#include <cstddef>

/// \file Fortran-callable interface to ioda::ObsDataVector.

namespace ioda {
  template <typename T> class ObsDataVector;
}  // namespace ioda

namespace oops {
  class ObsVariables;
  class Variables;
}  // namespace oops

namespace opsinputs {

extern "C" {
  int opsinputs_obsdatavector_int_nlocs_f(const ioda::ObsDataVector<int> &vec);
  const oops::ObsVariables * opsinputs_obsdatavector_int_varnames_f(
      const ioda::ObsDataVector<int> &vec);
  bool opsinputs_obsdatavector_int_has_f(const ioda::ObsDataVector<int> &vec,
                                        const char *variable);
  void opsinputs_obsdatavector_int_get_f(const ioda::ObsDataVector<int> &vec,
                                        const char *variable,
                                        const size_t &length, int* data);

  int opsinputs_obsdatavector_float_nlocs_f(const ioda::ObsDataVector<float> &vec);
  const oops::ObsVariables * opsinputs_obsdatavector_float_varnames_f(
      const ioda::ObsDataVector<float> &vec);
  bool opsinputs_obsdatavector_float_has_f(const ioda::ObsDataVector<float> &vec,
                                          const char *variable);
  void opsinputs_obsdatavector_float_get_f(const ioda::ObsDataVector<float> &vec,
                                          const char *variable,
                                          const size_t &length, float* data);
}

}  // namespace opsinputs

#endif  // OPSINPUTS_OPSINPUTS_OBSDATAVECTOR_F_H_
