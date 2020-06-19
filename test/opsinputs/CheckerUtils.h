/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef TEST_OPSINPUTS_CHECKERUTILS_H_
#define TEST_OPSINPUTS_CHECKERUTILS_H_

#include <string>
#include <utility>
#include <vector>

#include <boost/optional.hpp>

namespace opsinputs {
namespace test {

/// \brief Directory separator.
constexpr char PATH_SEPARATOR = '/';  // This could be made OS-dependent.

std::string getEnvVariableOrThrow(const char *variableName);

/// \brief Return true if \p string starts with \p prefix, false otherwise.
bool startsWith(const std::string &string, const char *prefix);

/// \brief If \p line contains the substring " = ", return the trimmed left- and right-hand side of
/// the equality. Otherwise return boost::none.
boost::optional<std::pair<std::string, std::string>> splitAtEqualsSignAndTrim(
    const std::string &line);

/// \brief Split \p line into chunks of exactly \p chunkSize characters, trim them and collect
/// into a vector.
///
/// Any leftover characters at the end of \p line are ignored.
std::vector<std::string> splitIntoFixedLengthChunksAndTrim(const std::string &line,
                                                           size_t chunkSize);

/// \brief Return the contents of \p inputFilePath printed in a textual form using the OPS utility
/// \p printUtilName.
std::string runOpsPrintUtil(const char *printUtilName, const std::string &inputFilePath);

}  // namespace test
}  // namespace opsinputs

#endif  // TEST_OPSINPUTS_CHECKERUTILS_H_
