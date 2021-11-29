/* -----------------------------------------------------------------------------
 * (C) British Crown Copyright 2021 Met Office
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * -----------------------------------------------------------------------------
 */

#ifndef UFO_FILTERS_OBSFUNCTIONS_GNSSROSTATIONIDMETOFFICE_H_
#define UFO_FILTERS_OBSFUNCTIONS_GNSSROSTATIONIDMETOFFICE_H_

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
  std::string stringFormat(const size_t&, const size_t&) const;
};
// -----------------------------------------------------------------------------

}  // namespace ufo

#endif  // UFO_FILTERS_OBSFUNCTIONS_GNSSROSTATIONIDMETOFFICE_H_
