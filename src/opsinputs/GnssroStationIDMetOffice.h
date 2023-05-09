/* -----------------------------------------------------------------------------
 * (C) Crown Copyright 2021, the Met Office. All rights reserved.
 *
 * -----------------------------------------------------------------------------
 */

#ifndef OPSINPUTS_GNSSROSTATIONIDMETOFFICE_H_
#define OPSINPUTS_GNSSROSTATIONIDMETOFFICE_H_

#include <string>

#include "ufo/filters/ObsFilterData.h"
#include "ufo/filters/obsfunctions/ObsFunctionBase.h"
#include "ufo/filters/Variables.h"

namespace ufo {
///
/// \brief Function calculates the GNSS-RO station ID for GNSS-RO observations
///        as a 16-character string of the form AAAABBBBBCCCCCCC where:
///        AAAA is the satellite ID
///        BBBBB is the profile number
///        CCCCCCC is the observation number
///
class GnssroStationIDMetOffice : public ObsFunctionBase<std::string> {
 public:
  explicit GnssroStationIDMetOffice(const eckit::LocalConfiguration &);
  ~GnssroStationIDMetOffice();

  void compute(const ObsFilterData &,
               ioda::ObsDataVector<std::string> &) const;
  const ufo::Variables & requiredVariables() const;
 private:
  ufo::Variables invars_;
  std::string stringFormat(const size_t, const size_t) const;
};
// -----------------------------------------------------------------------------

}  // namespace ufo

#endif  // OPSINPUTS_GNSSROSTATIONIDMETOFFICE_H_
