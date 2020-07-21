/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "../../test/opsinputs/TempFile.h"

namespace opsinputs {
namespace test {

TempFile::TempFile(const eckit::PathName &fileName)
  : fileName_(fileName)
{}

TempFile::TempFile(const char *fileName) :
  fileName_(fileName)
{}

TempFile::TempFile(TempFile &&other) {
  std::swap(fileName_, other.fileName_);
}

TempFile & TempFile::operator=(TempFile &&other) {
  if (this != &other) {
    deleteFileAndResetFileName();
    std::swap(fileName_, other.fileName_);
  }
  return *this;
}

TempFile::~TempFile() {
  deleteFileAndResetFileName();
}

std::string TempFile::name() const {
  return fileName_.asString();
}

void TempFile::deleteFileAndResetFileName() {
  if (fileName_.exists()) {
    fileName_.unlink();
  }
  fileName_ = eckit::PathName();
}

}  // namespace test
}  // namespace opsinputs
