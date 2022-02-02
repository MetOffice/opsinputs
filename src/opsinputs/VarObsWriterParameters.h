/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details.
 */

#ifndef OPSINPUTS_VAROBSWRITERPARAMETERS_H_
#define OPSINPUTS_VAROBSWRITERPARAMETERS_H_

#include <string>

#include "eckit/exception/Exceptions.h"
#include "oops/generic/ObsFilterParametersBase.h"
#include "oops/util/parameters/OptionalParameter.h"
#include "oops/util/parameters/Parameter.h"
#include "oops/util/parameters/RequiredParameter.h"

namespace opsinputs {

/// \brief VarObsWriter options.
class VarObsWriterParameters : public oops::ObsFilterParametersBase {
  OOPS_CONCRETE_PARAMETERS(VarObsWriterParameters, oops::ObsFilterParametersBase)

 public:
  /// Determines OPS verbosity.
  ///
  /// Valid values (case-insensitive): Operational, Quiet, Production, Normal, Diagnostic, Debug,
  /// Verbose.
  oops::Parameter<std::string> generalMode{"general_mode", "normal", this};

  /// Directory containing namelist files specifying the varfields to output.
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

  /// Define the orientation of the GeoVaLs, true if they are toptobottom and otherwise false.
  /// default is true.
  oops::Parameter<bool> geoValsAreTopToBottom{"geovals_are_top_to_bottom", true, this};

  /// Account for the GPSRO tangent point drift.
  oops::Parameter<bool> accountForGPSROTangentPointDrift{
    "account_for_gpsro_tangent_point_drift", false, this};
  /// Output the Family field (taken from the radar_family variable). Used for radar observations.
  oops::Parameter<bool> useRadarFamily{"use_radar_family", false, this};

  /// Require ObsValue/air_temperature is present if writing out the theta varfield
  oops::Parameter<bool> requireTforTheta{"require_T_for_theta_varfield", true, this};

  /// Fill Ops % ObsType from MetaData/ops_subtype. If false, use ObsGroupName.
  oops::Parameter<bool> fillObsTypeFromOpsSubtype{"fill_obstype_from_ops_subtype", false, this};

  /// Update OPS flag to output the varbc predictors
  oops::Parameter<bool> outputVarBCPredictors{"output_varbc_predictors", false, this};

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
  /// True if ship winds have been adjusted to 10m.
  oops::Parameter<bool> IC_ShipWind{"IC_ShipWind", false, this};
  /// Version of the ground GPS operator.
  ///
  /// Valid values (case-insensitive): Choice, Generic.
  oops::Parameter<std::string> IC_GroundGPSOperator{"IC_GroundGPSOperator", "choice", this};
  /// TODO(Neill): Describe this parameter.
  oops::Parameter<bool> IC_GPSRO_Operator_pseudo{"IC_GPSRO_Operator_pseudo", false, this};
  /// TODO(Neill): Describe this parameter.
  oops::Parameter<bool> IC_GPSRO_Operator_press{"IC_GPSRO_Operator_press", false, this};

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

}  // namespace opsinputs

#endif  // OPSINPUTS_VAROBSWRITERPARAMETERS_H_
