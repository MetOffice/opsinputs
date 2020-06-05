/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef CXVAROBS_CXWRITERPARAMETERS_H_
#define CXVAROBS_CXWRITERPARAMETERS_H_

#include <string>

#include "eckit/exception/Exceptions.h"
#include "oops/util/parameters/OptionalParameter.h"
#include "oops/util/parameters/Parameter.h"
#include "oops/util/parameters/Parameters.h"
#include "oops/util/parameters/RequiredParameter.h"

namespace cxvarobs {

/// \brief CxWriter options.
class CxWriterParameters : public oops::Parameters {
 public:
  /// Determines OPS verbosity.
  ///
  /// Valid values (case-insensitive): Operational, Quiet, Production, Normal, Diagnostic, Debug,
  /// Verbose.
  oops::Parameter<std::string> generalMode{"general_mode", "normal", this};

  /// Directory containing namelist files specifying the cxfields to output.
  ///
  /// The list of varfields to output for an observation group ObsGroup is controlled by the file
  /// ObsGroup.nl located in the namelist directory.
  ///
  /// If this option is not set, the location of the namelist directory is controlled by the
  /// OPS_VAROBSCONTROL_NL_DIR environment variable. If the namelist directory or the namelist file
  /// corresponding to a particular observation group is not found, the default list of varfields
  /// specified by OPS for that observation group is used.
  oops::OptionalParameter<std::string> namelistDirectory{"namelist_directory", this};

  /// Output directory for VarObs files.
  ///
  /// If this option is not set, the location of the output directory is controlled by the
  /// OPS_VAROB_OUTPUT_DIR environment variable.
  oops::OptionalParameter<std::string> outputDirectory{"output_directory", this};

  /// Output only observations that passed the quality check in all variables.
  oops::Parameter<bool> rejectObsWithAnyVariableFailingQC{
    "reject_obs_with_any_variable_failing_qc", false, this};
  /// Output only observations that passed the quality check in at least one variable.
  oops::Parameter<bool> rejectObsWithAllariablesFailingQC{
    "reject_obs_with_all_variables_failing_qc", false, this};

// TODO(wsmigaj): needed?
//  /// Account for the GPSRO tangent point drift.
//  oops::Parameter<bool> accountForGPSROTangentPointDrift{
//    "account_for_gpsro_tangent_point_drift", false, this};
  /// Output the Family field (taken from the radar_family variable). Used for radar observations.
  oops::Parameter<bool> useRadarFamily{"use_radar_family", false, this};

  // Values of UM header elements. Ultimately some of them might be set using data retrieved
  // from the model.

  /// Vertical coordinate type.
  ///
  /// Valid values (case-insensitive): Hybrid, Sigma, Pressure, Depth, CP, Wave.
  oops::Parameter<std::string> FH_VertCoord{"FH_VertCoord", "Hybrid", this};
  /// Horizontal grid type.
  ///
  /// Valid values (case-insensitive): Global, NH, SH, LamNoWrap, LamWrap, Eq, LamNoWrapEq,
  /// LamWrapEq.
  oops::Parameter<std::string> FH_HorizGrid{"FH_HorizGrid", "Global", this};
  /// Grid staggering indicator.
  ///
  /// Valid values (case-insensitive): ArakawaB, ArakawaC, EndGame.
  oops::Parameter<std::string> FH_GridStagger{"FH_GridStagger", "EndGame", this};
  /// Model version number x 100 + release number.
  oops::Parameter<int> FH_ModelVersion{"FH_ModelVersion", 0, this};

  /// Number of points E-W.
  oops::Parameter<int> IC_XLen{"IC_XLen", 0, this};
  /// Number of points N-S.
  oops::Parameter<int> IC_YLen{"IC_YLen", 0, this};
  /// Number of levels (PLEVELS).
  oops::Parameter<int> IC_PLevels{"IC_PLevels", 0, this};
  /// Number of wet levels (QLEVELS).
  oops::Parameter<int> IC_WetLevels{"IC_WetLevels", 0, this};

  /// As described in the Unified Model Documentation Paper F03.
  oops::Parameter<double> RC_LongSpacing{"RC_LongSpacing", 0.0, this};
  /// As described in the Unified Model Documentation Paper F03.
  oops::Parameter<double> RC_LatSpacing{"RC_LatSpacing", 0.0, this};
  /// As described in the Unified Model Documentation Paper F03.
  oops::Parameter<double> RC_FirstLat{"RC_FirstLat", 0.0, this};
  /// As described in the Unified Model Documentation Paper F03.
  oops::Parameter<double> RC_FirstLong{"RC_FirstLong", 0.0, this};
  /// As described in the Unified Model Documentation Paper F03.
  oops::Parameter<double> RC_PoleLat{"RC_PoleLat", 0.0, this};
  /// As described in the Unified Model Documentation Paper F03.
  oops::Parameter<double> RC_PoleLong{"RC_PoleLong", 0.0, this};
};

}  // namespace cxvarobs

#endif  // CXVAROBS_CXWRITERPARAMETERS_H_
