/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef CXVAROBS_LOCALENVIRONMENT_H_
#define CXVAROBS_LOCALENVIRONMENT_H_

#include <map>
#include <set>
#include <string>

namespace cxvarobs {

/// \brief Change environment variables, restoring their original values on destruction.
class LocalEnvironment {
 public:
  /// \brief Default constructor.
  LocalEnvironment();

  /// \brief For each (\c key, \c value) pair in variableNamesAndValues, set the environment
  /// value \c key to \c value.
  explicit LocalEnvironment(const std::map<std::string, std::string> &variableNamesAndValues);

  /// \brief Restore all environment variables changed by the constructor of the set() function
  /// to their original values.
  ~LocalEnvironment();

  LocalEnvironment(const LocalEnvironment &) = delete;
  LocalEnvironment& operator=(const LocalEnvironment &) = delete;

  /// Set the environment variable \p variableName to \p value.
  void set(const std::string &variableName, const std::string &value);

 private:
  std::map<std::string, std::string> variableNamesAndOriginalValues_;
  std::set<std::string> originallyUnsetVariables_;
};

}  // namespace cxvarobs

#endif  // CXVAROBS_LOCALENVIRONMENT_H_
