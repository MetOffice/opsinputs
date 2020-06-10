/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef TEST_CXVAROBS_CHECKERUTILS_H_
#define TEST_CXVAROBS_CHECKERUTILS_H_

#include <string>

namespace cxvarobs {
namespace test {

// This could be made OS-dependent.
constexpr char PATH_SEPARATOR = '/';

std::string getEnvVariableOrThrow(const char *variableName);

bool startsWith(const std::string &string, const char *prefix);

/// \brief Return the contents of \p inputFilePath printed in a textual form using the OPS utility
/// \p printUtilName.
std::string runOpsPrintUtil(const char *printUtilName, const std::string &inputFilePath);

}  // namespace test
}  // namespace cxvarobs

#endif  // TEST_CXVAROBS_CHECKERUTILS_H_
