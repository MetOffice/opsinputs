/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef TEST_OPSINPUTS_TEMPFILE_H_
#define TEST_OPSINPUTS_TEMPFILE_H_

#include <string>

#include "eckit/filesystem/PathName.h"

namespace opsinputs {
namespace test {

/// Manages a temporary file (deleted when this object is destroyed).
class TempFile {
 public:
  explicit TempFile(const eckit::PathName &fileName);
  explicit TempFile(const char *fileName);

  TempFile(const TempFile &) = delete;
  TempFile(TempFile &&other);

  TempFile & operator=(const TempFile &) = delete;
  TempFile & operator=(TempFile &&other);

  /// \brief Deletes the file, it it exists.
  ~TempFile();

  /// \returns Name of the temporary file.
  std::string name() const;

 private:
  void deleteFileAndResetFileName();

 private:
  eckit::PathName fileName_;
};

}  // namespace test
}  // namespace opsinputs

#endif  // TEST_OPSINPUTS_TEMPFILE_H_
