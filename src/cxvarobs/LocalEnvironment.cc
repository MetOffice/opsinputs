/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "cxvarobs/LocalEnvironment.h"

#include <cstdlib>

namespace cxvarobs {

LocalEnvironment::LocalEnvironment()
{}

LocalEnvironment::LocalEnvironment(const std::map<std::string, std::string> &variableNamesAndValues) {
  for (const auto &nameAndValue : variableNamesAndValues) {
    set(nameAndValue.first, nameAndValue.second);
  }
}

LocalEnvironment::~LocalEnvironment() {
  for (const auto &nameAndValue : variableNamesAndOriginalValues_)
    setenv(nameAndValue.first.c_str(), nameAndValue.second.c_str(), 1 /*overwrite*/);
  for (const auto &name : originallyUnsetVariables_)
    unsetenv(name.c_str());
}

void LocalEnvironment::set(const std::string &variableName, const std::string &value) {
  const bool originalValueAlreadyRecorded =
      variableNamesAndOriginalValues_.find(variableName) != variableNamesAndOriginalValues_.end() ||
      originallyUnsetVariables_.find(variableName) != originallyUnsetVariables_.end();

  if (!originalValueAlreadyRecorded) {
    char* originalValue = getenv(variableName.c_str());
    if (originalValue != nullptr)
      variableNamesAndOriginalValues_[variableName] = originalValue;
    else
      originallyUnsetVariables_.insert(variableName);
  }

  setenv(variableName.c_str(), value.c_str(), 1 /*overwrite*/);
}

}  // namespace cxvarobs
