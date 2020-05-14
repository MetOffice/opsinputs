/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "cxvarobs/obsdatavector_f.h"

#include <algorithm>
#include <cstring>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"

#include "ioda/ObsDataVector.h"

namespace cxvarobs {

int cxvarobs_obsdatavector_int_nlocs_f(const ioda::ObsDataVector<int> &vec) {
  return vec.nlocs();
}

const oops::Variables * cxvarobs_obsdatavector_int_varnames_f(
    const ioda::ObsDataVector<int> &vec) {
  return &vec.varnames();
}

bool cxvarobs_obsdatavector_int_has_f(const ioda::ObsDataVector<int> &vec,
                                      const char *variable) {
  return vec.has(variable);
}

void cxvarobs_obsdatavector_int_get_f(const ioda::ObsDataVector<int> &vec,
                                      const char *variable,
                                      const std::size_t &length, int* data) {
  const ioda::ObsDataRow<int> &row = vec[variable];
  ASSERT(length >= row.size());
  std::copy(row.begin(), row.end(), data);
}

int cxvarobs_obsdatavector_float_nlocs_f(const ioda::ObsDataVector<float> &vec) {
  return vec.nlocs();
}

const oops::Variables * cxvarobs_obsdatavector_float_varnames_f(
    const ioda::ObsDataVector<float> &vec) {
  return &vec.varnames();
}

bool cxvarobs_obsdatavector_float_has_f(const ioda::ObsDataVector<float> &vec,
                                        const char *variable) {
  return vec.has(variable);
}

void cxvarobs_obsdatavector_float_get_f(const ioda::ObsDataVector<float> &vec,
                                        const char *variable,
                                        const std::size_t &length, float* data) {
  const ioda::ObsDataRow<float> &row = vec[variable];
  ASSERT(length >= row.size());
  std::copy(row.begin(), row.end(), data);
}

}  // namespace cxvarobs
