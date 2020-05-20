/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef CXVAROBS_LOCALENVIRONMENT_H
#define CXVAROBS_LOCALENVIRONMENT_H

#include <map>
#include <set>
#include <string>

namespace cxvarobs {

/// \brief Sets environment variables to values specified in the contructor, restoring the original
/// values on destruction.
class LocalEnvironment {
 public:
  LocalEnvironment();
  explicit LocalEnvironment(const std::map<std::string, std::string> &variableNamesAndValues);

  ~LocalEnvironment();
  LocalEnvironment(const LocalEnvironment &) = delete;
  LocalEnvironment& operator=(const LocalEnvironment &) = delete;

  void set(const std::string &variableName, const std::string &value);

 private:
  std::map<std::string, std::string> variableNamesAndOriginalValues_;
  std::set<std::string> originallyUnsetVariables_;
};

}  // namespace cxvarobs

#endif // CXVAROBS_LOCALENVIRONMENT_H
