/*
 * (C) Copyright 2020 Met Office UK
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 */

#ifndef CXVAROBS_VAROBSWRITER_H_
#define CXVAROBS_VAROBSWRITER_H_

#include <ostream>
#include <string>

#include "boost/shared_ptr.hpp"

#include "ioda/ObsDataVector.h"
#include "oops/base/Variables.h"
#include "oops/util/ObjectCounter.h"
#include "oops/util/Printable.h"
#include "cxvarobs/VarObsWriter.interface.h"

namespace eckit {
  class Configuration;
}

namespace ioda {
  class ObsSpace;
  class ObsVector;
}

namespace ufo {
  class GeoVaLs;
  class ObsDiagnostics;
}

namespace cxvarobs {

class VarObsWriter : public util::Printable,
                private util::ObjectCounter<VarObsWriter> {
 public:
  static const std::string classname() {return "ufo::VarObsWriter";}

  VarObsWriter(ioda::ObsSpace &, const eckit::Configuration &,
            boost::shared_ptr<ioda::ObsDataVector<int> > flags,
            boost::shared_ptr<ioda::ObsDataVector<float> > obsErrors);
  ~VarObsWriter();

  void preProcess() const {}
  void priorFilter(const ufo::GeoVaLs &) const;
  void postFilter(const ioda::ObsVector &, const ufo::ObsDiagnostics & diags) const;

  const oops::Variables & requiredVars() const {return geovars_;}
  const oops::Variables & requiredHdiagnostics() const {return extradiagvars_;}

 private:
  void print(std::ostream &) const;
  F90check key_;

  ioda::ObsSpace & obsdb_;
  oops::Variables geovars_;
  oops::Variables extradiagvars_;
  boost::shared_ptr<ioda::ObsDataVector<int>> flags_;
  boost::shared_ptr<ioda::ObsDataVector<float>> obsErrors_;
};

}  // namespace cxvarobs

#endif  // CXVAROBS_VAROBSWRITER_H_
