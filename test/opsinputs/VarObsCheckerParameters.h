/*
 * (C) Crown Copyright 2020, the Met Office. All rights reserved.
 *
 * Refer to COPYRIGHT.txt of this distribution for details.
 */

#ifndef TEST_OPSINPUTS_VAROBSCHECKERPARAMETERS_H_
#define TEST_OPSINPUTS_VAROBSCHECKERPARAMETERS_H_

#include <map>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"
#include "oops/generic/ObsFilterParametersBase.h"
#include "oops/util/parameters/OptionalParameter.h"
#include "oops/util/parameters/Parameter.h"
#include "oops/util/parameters/RequiredParameter.h"

namespace opsinputs {

/// \brief VarObsChecker options.
class VarObsCheckerParameters : public oops::ObsFilterParametersBase {
  OOPS_CONCRETE_PARAMETERS(VarObsCheckerParameters, ObsFilterParametersBase)

 public:
  /// Directory containing the VarObs files.
  ///
  /// If this option is not set, the location of this directory is controlled by the
  /// OPS_VAROB_OUTPUT_DIR environment variable.
  oops::OptionalParameter<std::string> outputDirectory{"output_directory", this};

  /// Expected values of header fields.
  oops::Parameter<std::map<std::string, std::string>> expectedHeaderFields{
    "expected_header_fields", {}, this};

  /// Expected contents of columns of the main table with per-observation data.
  oops::Parameter<std::map<std::string, std::vector<std::string>>> expectedMainTableColumns{
    "expected_main_table_columns", {}, this};
};

}  // namespace opsinputs

#endif  // TEST_OPSINPUTS_VAROBSCHECKERPARAMETERS_H_
