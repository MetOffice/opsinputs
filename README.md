(C) Copyright 2020 Met Office UK

This software is licensed under the terms of the Apache Licence Version 2.0
which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

Overview
========

This software makes it possible for NG-OPS to write observation data to files in the VarObs format. In future it will also allow writing geovals to files in the Cx format.

Dependencies
============

1. The `ufo` JEDI project and its dependencies, e.g. `oops` and `ioda`.

2. The Observation Processing System (OPS) and its dependencies.

Note that both of the above need to be built with the same compiler and use the same libraries (e.g. MPI, HDF5, NetCDF).

Building
========

The instructions below assume that the software will be built with version 7.2.0 of the GNU compiler suite.

1. Build OPS as described at https://code.metoffice.gov.uk/trac/ops/wiki/WorkingPractices. Use the `ops_x86_64_gfortran_debug` configuration.

2. Find the OPS build folder. It should have a sibling called `build.tgz` containing `include` and `o` directories with Fortran module files and object files. Unzip that archive into the `build` folder. The latter should now contain `include` and `o` subdirectories at the same level as `bin` and `installs`.

3. Clone the `ufo-bundle` into a folder of your choice. 
4. Open the `CMakeLists.txt` file in the `ufo-bundle` directory and add the following line after all other lines starting with `ecbuild_bundle`:

       ecbuild_bundle( PROJECT cxvarobs GIT "https://github.com/JCSDA/cxvarobs.git" BRANCH develop UPDATE)

   (Git repository path to be confirmed.)

5. Run the following commands to set up your build environment:

      module use /data/users/wsmigaj/Projects/JediModules/modulefiles
      module unload R/3.6.1
      module load jedi/gnu/7.2.0/ufo-bundle-dev-stack

6. Create a build directory next to the `ufo-bundle` directory, enter it and run ecbuild to configure a build, passing the path to the OPS build folder to the OPS_ROOT variable:

      ecbuild -DOPS_ROOT=/path/to/OPS/build ../ufo-bundle

7. Run make to build the the bundle:

      make -j4

8. Optionally, run tests to verify that components of the CxVarObs package work correctly:

      ctest -R cxvarobs

Usage
=====

VarObs files are written by the `VarObsWriter` observation filter. See the documentation of this filter for more information. YAML files illustrating its use can be found in the `test/cxvarobs/testinput` folder.

Development
===========

Only a subset of varfields recognised by OPS and VAR can currently be output. To add support for a new varfield:

1. Determine where the input data will come from (a variable stored in the `ObsSpace` object? a GeoVaL?).
2. Edit the `case` corresponding to the varfield in question in the `select` statement in the `cxvarobs_varobswriter_populateobservations` subroutine in the `src/cxvarobs/cxvarobs_varobswriter_mod.F90` file. In the vast majority of cases, you will simply need to replace the (commented) call to `Ops_Alloc` with a call to an appropriate `cxvarobs_varobswriter_fill...` subroutine. For example, suppose that the `VarField_logvis` varfield should be filled with the data (observed values, observation errors, gross error probabilities, QC flags) stored in the `logarithmic_visibility` observation variable in the `ObsSpace`. The comment

        ! call Ops_Alloc(Ob % Header % logvis, "logvis", Ob % Header % NumObsLocal, Ob % logvis)

should then be replaced by 

        call cxvarobs_varobswriter_fillelementtypefromsimulatedvariable( &
          Ob % Header % logvis, "logvis", Ob % Header % NumObsLocal, Ob % logvis, &
          ObsSpace, Flags, ObsErrors, "logarithmic_visibility")

If in doubt, look at similar varfields that have already been implemented or read the documentation of relevant `cxvarobs_varobswriter_fill...` subroutines.

