/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef TEST_CXVAROBS_VAROBSCHECKERPARAMETERS_H_
#define TEST_CXVAROBS_VAROBSCHECKERPARAMETERS_H_

#include <map>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"
#include "oops/util/parameters/OptionalParameter.h"
#include "oops/util/parameters/Parameter.h"
#include "oops/util/parameters/Parameters.h"
#include "oops/util/parameters/RequiredParameter.h"

namespace cxvarobs {

/// \brief VarObsChecker options.
class VarObsCheckerParameters : public oops::Parameters {
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
  oops::OptionalParameter<std::string> namelistDirectory{"namelist_directory", this};

  /// Output directory for VarObs files.
  ///
  /// If this option is not set, the location of the output directory is controlled by the
  /// OPS_VAROB_OUTPUT_DIR environment variable.
  oops::OptionalParameter<std::string> outputDirectory{"output_directory", this};

  oops::Parameter<std::map<std::string, std::string>> expectedHeaderFields{
    "expected_header_fields", {}, this};

  oops::Parameter<std::map<std::string, std::vector<std::string>>> expectedMainTableColumns{
    "expected_main_table_columns", {}, this};
};

}  // namespace cxvarobs

#endif  // TEST_CXVAROBS_VAROBSCHECKERPARAMETERS_H_
