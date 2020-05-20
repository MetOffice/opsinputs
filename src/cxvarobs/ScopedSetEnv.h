/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef CXVAROBS_SCOPEDSETENV_H
#define CXVAROBS_SCOPEDSETENV_H

#include <map>
#include <set>
#include <string>

namespace cxvarobs {

/// \brief Sets environment variables to values specified in the contructor, restoring the original
/// values on destruction.
class ScopedSetEnv {
 public:
  ScopedSetEnv();
  explicit ScopedSetEnv(const std::map<std::string, std::string> &variableNamesAndValues);

  ~ScopedSetEnv();
  ScopedSetEnv(const ScopedSetEnv &) = delete;
  ScopedSetEnv& operator=(const ScopedSetEnv &) = delete;

  void set(const std::string &variableName, const std::string &value);

 private:
  std::map<std::string, std::string> variableNamesAndOriginalValues_;
  std::set<std::string> originallyUnsetVariables_;
};

}  // namespace cxvarobs

#endif // CXVAROBS_SCOPEDSETENV_H
