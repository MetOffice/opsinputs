/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 */

#ifndef TEST_OPSINPUTS_CXCHECKERPARAMETERS_H_
#define TEST_OPSINPUTS_CXCHECKERPARAMETERS_H_

#include <map>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"
#include "oops/util/parameters/OptionalParameter.h"
#include "oops/util/parameters/Parameter.h"
#include "ufo/ObsFilterParametersBase.h"

namespace opsinputs {

/// \brief CxChecker options.
class CxCheckerParameters : public ufo::ObsFilterParametersBase {
    OOPS_CONCRETE_PARAMETERS(CxCheckerParameters, ObsFilterParametersBase)

 public:
  /// Output directory for Cx files.
  ///
  /// If this option is not set, the location of the output directory is controlled by the
  /// OPS_CX_DIR_LIST environment variable.
  oops::OptionalParameter<std::string> outputDirectory{"output_directory", this};

  /// Expected values of header fields.
  oops::Parameter<std::map<std::string, std::string>> expectedHeaderFields{
    "expected_header_fields", {}, this};

  /// Expected Eta_Theta levels.
  oops::OptionalParameter<std::vector<std::string>> expectedEtaThetaLevels{
    "expected_eta_theta_levels", this};

  /// Expected Eta_Rho levels.
  oops::OptionalParameter<std::vector<std::string>> expectedEtaRhoLevels{
    "expected_eta_rho_levels", this};

  /// Expected surface variable indices (corresponding to constants from OpsMod_CXIndices.
  oops::OptionalParameter<std::vector<std::string>> expectedSurfaceVariables{
    "expected_surface_variables", this};

  /// Expected upper-air variable indices (corresponding to constants from OpsMod_CXIndices.
  oops::OptionalParameter<std::vector<std::string>> expectedUpperAirVariables{
    "expected_upper_air_variables", this};

  /// Expected values of lookup table fields.
  oops::Parameter<std::map<std::string, std::string>> expectedLookupFields{
    "expected_lookup_fields", {}, this};

  /// expected_main_table_columns[i][j][k] is the expected value of kth element of jth model column
  /// in ith batch.
  oops::OptionalParameter<std::vector<std::vector<std::vector<std::string>>>>
    expectedMainTableColumns{"expected_main_table_columns", this};
};

}  // namespace opsinputs

#endif  // TEST_OPSINPUTS_CXCHECKERPARAMETERS_H_
