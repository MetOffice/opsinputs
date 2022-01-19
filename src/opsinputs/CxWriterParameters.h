/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details.
 */

#ifndef OPSINPUTS_CXWRITERPARAMETERS_H_
#define OPSINPUTS_CXWRITERPARAMETERS_H_

#include <string>
#include <vector>

#include "oops/generic/ObsFilterParametersBase.h"
#include "oops/util/parameters/OptionalParameter.h"
#include "oops/util/parameters/Parameter.h"

namespace opsinputs {

/// \brief CxWriter options.
class CxWriterParameters : public oops::ObsFilterParametersBase {
  OOPS_CONCRETE_PARAMETERS(CxWriterParameters, oops::ObsFilterParametersBase)

 public:
  /// Determines OPS verbosity.
  ///
  /// Valid values (case-insensitive): Operational, Quiet, Production, Normal, Diagnostic, Debug,
  /// Verbose.
  oops::Parameter<std::string> generalMode{"general_mode", "normal", this};

  /// Directory containing namelist files specifying the cxfields to output.
  ///
  /// The list of cxfields to output for an observation group ObsGroup is controlled by the file
  /// ObsGroup.nl located in the namelist directory.
  ///
  /// If this option is not set, the location of the namelist directory is controlled by the
  /// OPS_CX_CONTROL_NL_DIR environment variable. If the namelist directory or the namelist file
  /// corresponding to a particular observation group is not found, the default list of cxfields
  /// specified by OPS for that observation group is used.
  oops::OptionalParameter<std::string> namelistDirectory{"namelist_directory", this};

  /// Output directory for Cx files.
  ///
  /// If this option is not set, the location of the output directory is controlled by the
  /// OPS_CX_DIR_LIST environment variable.
  oops::OptionalParameter<std::string> outputDirectory{"output_directory", this};

  /// Output only model columns corresponding to observations that passed the quality check in all
  /// variables.
  oops::Parameter<bool> rejectObsWithAnyVariableFailingQC{
    "reject_obs_with_any_variable_failing_qc", false, this};

  /// Output only model columns corresponding to observations that passed the quality check in
  /// at least one variable.
  oops::Parameter<bool> rejectObsWithAllariablesFailingQC{
    "reject_obs_with_all_variables_failing_qc", false, this};

  /// Should the GeoVals be treated as toptobottom or bottomtotop.
  oops::Parameter<bool> topToBottom{"geovals_are_top_to_bottom", true, this};

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
  /// Type of observation file
  ///
  /// Valid values (case-insensitive): Atmos, Ocean, SST, Wave
  oops::Parameter<std::string> FH_ObsFileType{"FH_ObsFileType", "Atmos", this};
  /// Model version number x 100 + release number.
  oops::Parameter<int> FH_ModelVersion{"FH_ModelVersion", 0, this};
  /// Valid values (case-insensitive): Atmos, Ocean, Wave
  oops::Parameter<std::string> FH_SubModel{"FH_SubModel", "Atmos", this};

  /// Number of points E-W.
  oops::Parameter<int> IC_XLen{"IC_XLen", 0, this};
  /// Number of points N-S.
  oops::Parameter<int> IC_YLen{"IC_YLen", 0, this};
  /// Number of levels (PLEVELS).
  oops::Parameter<int> IC_PLevels{"IC_PLevels", 0, this};
  /// Number of wet levels (QLEVELS).
  oops::Parameter<int> IC_WetLevels{"IC_WetLevels", 0, this};
  /// First rho level at which height is constant.
  oops::Parameter<int> IC_FirstConstantRhoLevel{"IC_FirstConstantRhoLevel", 0, this};

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
  /// Height at top theta level.
  oops::Parameter<double> RC_z_ModelTop{"RC_z_ModelTop", 0.0, this};

  /// The LBTIM element of the UM header look-up table.
  oops::Parameter<int> timeIndicator{"time_indicator", 0, this};
  /// The LBFT element of the UM header look-up table.
  oops::Parameter<int> forecastPeriod{"forecast_period", 0, this};

  // TODO(wsmigaj): I hope these comments are correct -- please let me know if not.
  // Can these parameters be given names understandable to a layman (without Greek letters)?
  /// New dynamics vertical coordinate theta. Should have length IC_PLevels + 1 (with the
  /// first element denoting the value at the surface).
  oops::OptionalParameter<std::vector<double>> etaThetaLevels{"eta_theta_levels", this};
  /// New dynamics vertical coordinate rho. Should have length IC_PLevels.
  oops::OptionalParameter<std::vector<double>> etaRhoLevels{"eta_rho_levels", this};

  // Parameters corresponding to certain global variables in the OPS.

  /// Model type.
  ///
  /// Valid values (case-insensitive): Atmos, Ocean, SST.
  oops::Parameter<std::string> modelType{"model_type", "atmos", this};

  /// Number of model dust bins.
  ///
  /// Valid values: 2 (global), 6 (CAM).
  oops::Parameter<int> numDustBins{"num_dust_bins", 2, this};
};

}  // namespace opsinputs

#endif  // OPSINPUTS_CXWRITERPARAMETERS_H_
