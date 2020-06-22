/*
 * (C) Copyright 2020 Met Office UK
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef TEST_OPSINPUTS_OBSFILTERS_H_
#define TEST_OPSINPUTS_OBSFILTERS_H_

#define ECKIT_TESTING_SELF_REGISTER_CASES 0

#include <string>
#include <vector>

#include "../test/ufo/ObsFilters.h"

#include "eckit/mpi/Comm.h"
#include "oops/parallel/mpi/mpi.h"
#include "oops/runs/Test.h"

namespace opsinputs {
namespace test {

/// \brief A test fixture similar to ufo::test::ObsFilters, but making it possible to pass
/// different configurations to different MPI processes.
///
/// To do that,
///
/// 1. Set the top-level option \c process_colors in the input YAML file to a list of integers
///    representing the indices of "colors" to be assigned to each MPI process. The text fixture
///    will then split the MPI_COMM_WORLD communicator into as many communicators as there are
///    colors and set the default OOPS communicator on each process to the communicator produced
///    by the split.
///
/// 2. List the configurations to be passed to the test on processes belonging to successive colors
///    in the top-level option \c color_configs in the input YAML file.
class ObsFilters : public oops::Test {
 public:
  int execute(const eckit::Configuration & config) const override {
    if (config.has("process_colors")) {
      // Retrieve the color of this process
      const std::vector<int> rankColors = config.getIntVector("process_colors");
      ASSERT(rankColors.size() == getComm().size());
      const int myColor = rankColors[getComm().rank()];

      // Retrieve this color's configuration
      const std::vector<eckit::LocalConfiguration> colorConfigs =
          config.getSubConfigurations("color_configs");
      ASSERT(myColor < colorConfigs.size());

      // Split the communicator
      const char *colorCommName = "color";
      oops::mpi::comm().split(myColor, colorCommName);
      eckit::mpi::setCommDefault(colorCommName);

      // Pass this color's configuration to the test
      return oops::Test::execute(colorConfigs[myColor]);
    } else {
      return oops::Test::execute(config);
    }
  }

 private:
  std::string testid() const override {return "opsinputs::test::ObsFilters";}

  void register_tests() const override {
    std::vector<eckit::testing::Test>& ts = eckit::testing::specification();

    ts.emplace_back(CASE("opsinputs/ObsFilters/testFilters")
    { ufo::test::testFilters(); });
  }
};

}  // namespace test
}  // namespace opsinputs

#endif  // TEST_OPSINPUTS_OBSFILTERS_H_
