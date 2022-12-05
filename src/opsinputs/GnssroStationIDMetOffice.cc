/* -----------------------------------------------------------------------------
 * (C) British Crown Copyright 2021 Met Office
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * -----------------------------------------------------------------------------
 *
 * Find the station ID for GNSS-RO observations used by the Met Office's DA
 * system.
 *
 * Example yaml snippet:
 *  - filter: Variable Assignment
 *   assignments:
 *   - name: MetaData/stationIdentification
 *     type: string
 *     function:
 *       name: StringObsFunction/GnssroStationIDMetOffice
 * -----------------------------------------------------------------------------
 */
#include "opsinputs/GnssroStationIDMetOffice.h"

#include <math.h>
#include <string>
#include <vector>

#include "ioda/ObsDataVector.h"
#include "oops/util/missingValues.h"
#include "ufo/filters/Variable.h"

namespace ufo {

static ObsFunctionMaker<GnssroStationIDMetOffice>
  makerGnssroStationIDMetOffice_("GnssroStationIDMetOffice");

/* -----------------------------------------------------------------------------
 * Specify that satellite identifier is noted as required by this function.
 * -----------------------------------------------------------------------------
 */
GnssroStationIDMetOffice::GnssroStationIDMetOffice(const eckit::LocalConfiguration & conf)
  : invars_() {
  invars_ += Variable("MetaData/satelliteIdentifier");
}

// -----------------------------------------------------------------------------

GnssroStationIDMetOffice::~GnssroStationIDMetOffice() {}

/* -----------------------------------------------------------------------------
 * Perform the computation.  For each profile construct the stationIdentification as a
 * fixed length string.
 * -----------------------------------------------------------------------------
 */
void GnssroStationIDMetOffice::compute(const ObsFilterData & in,
                                       ioda::ObsDataVector<std::string> & out) const {
  // Get the satellite identifier
  const size_t nlocs = in.nlocs();
  std::vector<int> satid;
  in.get(Variable("MetaData/satelliteIdentifier"), satid);

  // Get the record number of each profile
  std::vector<size_t> recordNumbers = out.space().recidx_all_recnums();
  oops::Log::debug() << "Unique record numbers" << std::endl;
  for (size_t record : recordNumbers) {
    oops::Log::debug() << record << " ";
  }
  oops::Log::debug() << std::endl;

  // For each profile, apply the function to each member
  for (const size_t& iProfile : recordNumbers) {
    const std::vector<size_t> & obsNumbers = out.space().recidx_vector(iProfile);
    for (size_t jobs : obsNumbers) {
      out[0][jobs] = stringFormat(satid[jobs], 4) +
                     stringFormat(iProfile, 5) +
                     stringFormat(jobs, 7);
    }
  }
}

/// Convert an integer to a fixed length string, checking whether the string is
/// too long for the output
std::string GnssroStationIDMetOffice::stringFormat
                            (const size_t input, const size_t width) const {
  std::stringstream returnStream;
  if (input >= pow(10, width)) {
    oops::Log::warning() << "stringFormat: " << input << " is too wide for a "
                         << width << " string" << std::endl;
    returnStream << std::setw(width) << std::setfill('0');
  } else {
    returnStream << std::setw(width) << std::setfill('0') << input;
  }
  return returnStream.str();
}

// -----------------------------------------------------------------------------

const ufo::Variables & GnssroStationIDMetOffice::requiredVariables() const {
  return invars_;
}

// -----------------------------------------------------------------------------

}  // namespace ufo
