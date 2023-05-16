/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
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
