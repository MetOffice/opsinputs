/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef TEST_OPSINPUTS_CXCHECKERPARAMETERS_H_
#define TEST_OPSINPUTS_CXCHECKERPARAMETERS_H_

#include <map>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"
#include "oops/util/parameters/OptionalParameter.h"
#include "oops/util/parameters/Parameter.h"
#include "oops/util/parameters/Parameters.h"

namespace opsinputs {

/// \brief CxChecker options.
class CxCheckerParameters : public oops::Parameters {
 public:
  /// Output directory for Cx files.
  ///
  /// If this option is not set, the location of the output directory is controlled by the
  /// OPS_CX_DIR_LIST environment variable.
  oops::OptionalParameter<std::string> outputDirectory{"output_directory", this};

  /// Expected values of header fields.
  oops::Parameter<std::map<std::string, std::string>> expectedHeaderFields{
    "expected_header_fields", {}, this};

  oops::OptionalParameter<std::vector<std::string>> expectedEtaThetaLevels{
    "expected_eta_theta_levels", this};

  oops::OptionalParameter<std::vector<std::string>> expectedEtaRhoLevels{
    "expected_eta_rho_levels", this};

  oops::Parameter<std::map<std::string, std::string>> expectedLookupFields{
    "expected_lookup_fields", {}, this};

  /// Expected contents of columns of the main table with per-observation data.
  oops::OptionalParameter<std::vector<std::vector<std::vector<std::string>>>>
    expectedMainTableColumns{"expected_main_table_columns", this};
};

}  // namespace opsinputs

#endif  // TEST_OPSINPUTS_CXCHECKERPARAMETERS_H_
