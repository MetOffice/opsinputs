/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef CXVAROBS_OBSDATAVECTOR_F_H_
#define CXVAROBS_OBSDATAVECTOR_F_H_

#include <cstddef>

/// \file Fortran-callable interface to ioda::ObsDataVector.

namespace ioda {
  template <typename T> class ObsDataVector;
}  // namespace ioda

namespace cxvarobs {

extern "C" {
  int cxvarobs_obsdatavector_int_nlocs_f(const ioda::ObsDataVector<int> &vec);
  bool cxvarobs_obsdatavector_int_has_f(const ioda::ObsDataVector<int> &vec,
                                        const char *variable);
  void cxvarobs_obsdatavector_int_get_f(const ioda::ObsDataVector<int> &vec,
                                        const char *variable,
                                        const size_t &length, int* data);

  int cxvarobs_obsdatavector_float_nlocs_f(const ioda::ObsDataVector<float> &vec);
  bool cxvarobs_obsdatavector_float_has_f(const ioda::ObsDataVector<float> &vec,
                                          const char *variable);
  void cxvarobs_obsdatavector_float_get_f(const ioda::ObsDataVector<float> &vec,
                                          const char *variable,
                                          const size_t &length, float* data);
}

}  // namespace cxvarobs

#endif  // CXVAROBS_OBSDATAVECTOR_F_H_
