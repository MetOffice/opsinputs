/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 */

#include "opsinputs/opsinputs_obsdatavector_f.h"

#include <algorithm>
#include <cstring>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"

#include "ioda/ObsDataVector.h"

namespace opsinputs {

namespace {

template <typename T>
int opsinputs_obsdatavector_nlocs_f(const ioda::ObsDataVector<T> &vec) {
  return vec.nlocs();
}

template <typename T>
const oops::ObsVariables * opsinputs_obsdatavector_varnames_f(
    const ioda::ObsDataVector<T> &vec) {
  return &vec.varnames();
}

template <typename T>
bool opsinputs_obsdatavector_has_f(const ioda::ObsDataVector<T> &vec,
                                   const char *variable) {
  return vec.has(variable);
}

template <typename T>
void opsinputs_obsdatavector_get_f(const ioda::ObsDataVector<T> &vec,
                                       const char *variable,
                                       const std::size_t &length, T* data) {
  const ioda::ObsDataRow<T> &row = vec[variable];
  ASSERT(length >= row.size());
  std::copy(row.begin(), row.end(), data);
}

}  // namespace

int opsinputs_obsdatavector_int_nlocs_f(const ioda::ObsDataVector<int> &vec) {
  return opsinputs_obsdatavector_nlocs_f(vec);
}

const oops::ObsVariables * opsinputs_obsdatavector_int_varnames_f(
    const ioda::ObsDataVector<int> &vec) {
  return opsinputs_obsdatavector_varnames_f(vec);
}

bool opsinputs_obsdatavector_int_has_f(const ioda::ObsDataVector<int> &vec,
                                       const char *variable) {
  return opsinputs_obsdatavector_has_f(vec, variable);
}

void opsinputs_obsdatavector_int_get_f(const ioda::ObsDataVector<int> &vec,
                                       const char *variable,
                                       const std::size_t &length, int* data) {
  opsinputs_obsdatavector_get_f(vec, variable, length, data);
}

int opsinputs_obsdatavector_float_nlocs_f(const ioda::ObsDataVector<float> &vec) {
  return opsinputs_obsdatavector_nlocs_f(vec);
}

const oops::ObsVariables * opsinputs_obsdatavector_float_varnames_f(
    const ioda::ObsDataVector<float> &vec) {
  return opsinputs_obsdatavector_varnames_f(vec);
}

bool opsinputs_obsdatavector_float_has_f(const ioda::ObsDataVector<float> &vec,
                                         const char *variable) {
  return opsinputs_obsdatavector_has_f(vec, variable);
}

void opsinputs_obsdatavector_float_get_f(const ioda::ObsDataVector<float> &vec,
                                         const char *variable,
                                         const std::size_t &length, float* data) {
  opsinputs_obsdatavector_get_f(vec, variable, length, data);
}

}  // namespace opsinputs
