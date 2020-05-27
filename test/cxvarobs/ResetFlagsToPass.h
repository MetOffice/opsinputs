/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef TEST_CXVAROBS_RESETFLAGSTOPASS_H_
#define TEST_CXVAROBS_RESETFLAGSTOPASS_H_

#include <memory>
#include <ostream>
#include <string>
#include <vector>

#include <boost/optional.hpp>
#include <boost/shared_ptr.hpp>

#include "ioda/ObsDataVector.h"
#include "oops/util/ObjectCounter.h"
#include "ufo/filters/FilterBase.h"
#include "ufo/filters/QCflags.h"

namespace eckit {
  class Configuration;
}

namespace ioda {
  template <typename DATATYPE> class ObsDataVector;
  class ObsSpace;
}

namespace cxvarobs {
namespace test {

/// \brief Resets the QC flag of all observations (even those failed by previous tests) to "pass".
class ResetFlagsToPass : public ufo::FilterBase,
                         private util::ObjectCounter<ResetFlagsToPass> {
 public:
  static const std::string classname() {return "cxvarobs::ResetFlagsToPass";}

  ResetFlagsToPass(ioda::ObsSpace &obsdb, const eckit::Configuration &config,
                   boost::shared_ptr<ioda::ObsDataVector<int> > flags,
                   boost::shared_ptr<ioda::ObsDataVector<float> > obserr);

 private:
  void print(std::ostream &) const override;
  void applyFilter(const std::vector<bool> &, const ufo::Variables &,
                   std::vector<std::vector<bool>> &) const override;
  int qcFlag() const override { return ufo::QCflags::pass; }
};

}  // namespace test
}  // namespace cxvarobs

#endif  // TEST_CXVAROBS_RESETFLAGSTOPASS_H_
