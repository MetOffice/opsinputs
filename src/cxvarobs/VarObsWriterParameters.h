/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef CXVAROBS_VAROBSWRITERPARAMETERS_H
#define CXVAROBS_VAROBSWRITERPARAMETERS_H

#include <string>

#include "eckit/exception/Exceptions.h"
#include "oops/util/parameters/OptionalParameter.h"
#include "oops/util/parameters/Parameter.h"
#include "oops/util/parameters/Parameters.h"
#include "oops/util/parameters/RequiredParameter.h"

namespace cxvarobs {

/// \brief VarObsWriter options.
class VarObsWriterParameters : public oops::Parameters {
 public:
  /// Directory containing namelist files specifying the varfields to output.
  ///
  /// The list of varfields to output for an observation group ObsGroup is controlled by the file
  /// ObsGroup.nl located in the namelist directory.
  ///
  /// If this option is not set, the location of the namelist directory is controlled by the
  /// OPS_VAROBSCONTROL_NL_DIR environment variable. If the namelist directory or the namelist file
  /// corresponding to a particular observation group is not found, the default list of varfields
  /// specified by OPS for that observation group is used.
  oops::OptionalParameter<std::string> namelist_directory{"namelist_directory", this};

  /// Output directory for VarObs files.
  ///
  /// If this option is not set, the location of the output directory is controlled by the
  /// OPS_VAROB_OUTPUT_DIR environment variable.
  oops::OptionalParameter<std::string> output_directory{"output_directory", this};

  oops::RequiredParameter<std::string> obs_group{"obs_group", this};
  oops::Parameter<bool> account_for_gpsro_tangent_point_drift{
    "account_for_gpsro_tangent_point_drift", false, this};
  oops::Parameter<bool> use_radar_family{"use_radar_family", false, this};

  oops::Parameter<std::string> FH_VertCoord{"FH_VertCoord", "FH_VertCoord_Hybrid", this};
  oops::Parameter<std::string> FH_HorizGrid{"FH_HorizGrid", "FH_HorizGrid_Global", this};
  oops::Parameter<std::string> FH_GridStagger{"FH_GridStagger", "FH_GridStagger_EndGame", this};
  oops::Parameter<int> FH_ModelVersion{"FH_ModelVersion", 0, this};

  oops::Parameter<int> IC_XLen{"IC_XLen", 0, this};
  oops::Parameter<int> IC_YLen{"IC_YLen", 0, this};
  oops::Parameter<int> IC_PLevels{"IC_PLevels", 0, this};
  oops::Parameter<int> IC_WetLevels{"IC_WetLevels", 0, this};
  oops::Parameter<std::string> IC_TorTheta{"IC_TorTheta", "IC_TorTheta_T", this};
  oops::Parameter<bool> IC_ShipWind{"IC_ShipWind", false, this};
  oops::Parameter<std::string> IC_GroundGPSOperator{"IC_GroundGPSOperator", "", this};
  oops::Parameter<bool> IC_GPSRO_Operator_pseudo{"IC_GPSRO_Operator_pseudo", false, this};
  oops::Parameter<bool> IC_GPSRO_Operator_press{"IC_GPSRO_Operator_press", false, this};

  oops::Parameter<double> RC_LongSpacing{"RC_LongSpacing", 0.0, this};
  oops::Parameter<double> RC_LatSpacing{"RC_LatSpacing", 0.0, this};
  oops::Parameter<double> RC_FirstLat{"RC_FirstLat", 0.0, this};
  oops::Parameter<double> RC_FirstLong{"RC_FirstLong", 0.0, this};
  oops::Parameter<double> RC_PoleLat{"RC_PoleLat", 0.0, this};
  oops::Parameter<double> RC_PoleLong{"RC_PoleLong", 0.0, this};
};

}  // namespace cxvarobs

#endif // CXVAROBS_VAROBSWRITERPARAMETERS_H
