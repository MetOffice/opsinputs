/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef TEST_CXVAROBS_CXCHECKERPARAMETERS_H_
#define TEST_CXVAROBS_CXCHECKERPARAMETERS_H_

#include <map>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"
#include "oops/util/parameters/OptionalParameter.h"
#include "oops/util/parameters/Parameter.h"
#include "oops/util/parameters/Parameters.h"

namespace cxvarobs {

/// \brief CxChecker options.
class CxCheckerParameters : public oops::Parameters {
 public:
  // TODO(wsmigaj): remove (together with corresp. fragment in the cc file)?
//  /// Directory containing namelist files specifying the cxfields to output.
//  ///
//  /// The list of cxfields to output for an observation group ObsGroup is controlled by the file
//  /// ObsGroup.nl located in the namelist directory.
//  ///
//  /// If this option is not set, the location of the namelist directory is controlled by the
//  /// OPS_VAROBSCONTROL_NL_DIR environment variable. If the namelist directory or the namelist file
//  /// corresponding to a particular observation group is not found, the default list of varfields
//  /// specified by OPS for that observation group is used.
//  oops::OptionalParameter<std::string> namelistDirectory{"namelist_directory", this};

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
  oops::OptionalParameter<std::vector<std::vector<std::vector<std::string>>>> expectedMainTableColumns{
    "expected_main_table_columns", this};
};

}  // namespace cxvarobs

#endif  // TEST_CXVAROBS_CXCHECKERPARAMETERS_H_
