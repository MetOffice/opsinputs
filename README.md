(C) Copyright 2020 Met Office UK

This software is licensed under the terms of the Apache Licence Version 2.0,
which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

Overview
========

This software makes it possible for NG-OPS to write observation data to files in the VarObs format and model data (geovals) to files in the Cx format.

Dependencies
============

1. The `ufo` JEDI project and its dependencies, e.g. `oops` and `ioda`.

2. The Observation Processing System (OPS) and its dependencies.

Note that both of the above need to be built with the same compiler and use the same libraries (e.g. MPI, HDF5, NetCDF).

Building
========

1. Build OPS as described at https://code.metoffice.gov.uk/trac/ops/wiki/WorkingPractices. Select the `ifort` or `gfortran` compiler. Alternatively, you can use an existing build from the `~opsrc` folder.

2. Find the OPS build folder. It should have a sibling called `build.tgz` containing `include` and `o` directories with Fortran module files and object files. Unzip that archive into the `build` folder. The latter should now contain `include` and `o` subdirectories at the same level as `bin` and `installs`.

3. Clone the `ufo-bundle` into a folder of your choice.

4. Open the `CMakeLists.txt` file in the `ufo-bundle` directory and add the following line after all other lines starting with `ecbuild_bundle`:

       ecbuild_bundle( PROJECT opsinputs GIT "https://github.com/MetOffice/opsinputs.git" BRANCH develop UPDATE)

5. Run the following commands to set up your build environment:

       module use /data/users/wsmigaj/Projects/JediModules/modulefiles
       module unload R/3.6.1

   and, depending on whether you are using a `gfortran` or an `ifort` build of OPS, either

       module load jedi/ufo-bundle-dev-stack/gnu/7.2.0

   or 

       module load jedi/ufo-bundle-dev-stack/intel/17.0.64

6. Create a build directory next to the `ufo-bundle` directory, enter it and run ecbuild to configure a build, passing the path to the OPS build folder to the OPS_ROOT variable:

       ecbuild -DOPS_ROOT=/path/to/OPS/build ../ufo-bundle

7. Run make to build the the bundle:

       make -j4

   Note: if using the gfortran 7.2 compiler in the Debug configuration, you may need to remove the -finit-derived flag from the saber/cmake/compiler_flags_GNU_Fortran.cmake file to avoid an internal compiler error during compilation of some saber source files.

8. Optionally, run tests to verify that components of the `opsinputs` package work correctly:

       ctest -R opsinputs

Usage
=====

VarObs and Cx files are written by the `VarObsWriter` and `CxWriter` observation filters, respectively. See the documentation of these filters for more information. YAML files illustrating their use can be found in the `test/opsinputs/testinput` folder.

Development
===========

VarObs
------

Only a subset of varfields recognised by OPS and VAR can currently be output. To add support for a new varfield:

1. Determine where the input data will come from (a variable stored in the `ObsSpace` object? a GeoVaL?).

2. Edit the `case` corresponding to the varfield in question in the `select` statement in the `opsinputs_varobswriter_populateobservations` subroutine in the `src/opsinputs/opsinputs_varobswriter_mod.F90` file. In the vast majority of cases, you will simply need to replace a commented-out call to `Ops_Alloc` with a call to an appropriate `opsinputs_fill_fill...` subroutine. For example, suppose that the `VarField_logvis` varfield should be filled with the data (observed values, observation errors, gross error probabilities and QC flags) stored in the `logarithmic_visibility` observation variable in the `ObsSpace`. The comment

        ! call Ops_Alloc(Ob % Header % logvis, "logvis", Ob % Header % NumObsLocal, Ob % logvis)

   should then be replaced by 

        call opsinputs_fill_fillelementtypefromsimulatedvariable( &
          Ob % Header % logvis, "logvis", Ob % Header % NumObsLocal, Ob % logvis, &
          ObsSpace, Flags, ObsErrors, "logarithmic_visibility")

   If in doubt, look at similar varfields that have already been implemented or read the documentation of relevant subroutines from the `opsinputs_fill_mod.F90` module.

3. Add a unit test for the new varfield:

   a. Create an input YAML file and put it in the `test/testinput` folder. Typically, you can copy an existing YAML file used by the test of a varfield whose implementation calls the same subroutine as that of the new varfield, and simply adjust the observation group, variable and varfield name embedded in the YAML file.

   b. Create an input NetCDF file and put it in the same folder. You can use the `test/generate_unittest_netcdfs.py` script to create the file (add an appropriate function call in the `VarObs` section at the end of the script, again mimicking one generating input data for a previously implemented varfield, and run the script). You may need to load the `satools-py3` module before running the script to give it access to NumPy and SciPy.

   c. Add a call to the `ADD_WRITER_TEST` function in the `test/CMakeLists.txt` file, specifying the name of the test and its input YAML and data files.

4. Update the list of implemented varfields in `Varfields.md`. 

Cx
--

Only a subset of cxfields recognised by OPS and VAR can currently be output. To add support for a new cxfield:

1. Determine the name of the GeoVal from which the cxfield will be retrieved and the name of the component of the `CX_type` type in OPS holding the value of that cxfield.

2. Set the appropriate `opsinputs_cxfields_*` constant in the `opsinputs_cxfields_mod.F90` file to the name of the GeoVal identified in the previous step.

3. Add a unit test for the new cxfield:

   a. Create an input NetCDF file and put it in the `test/testinput` folder. You can use the `test/generate_unittest_netcdfs.py` script to create the file (add a call to `output_1d_geoval_to_netcdf` or `output_2d_geoval_to_netcdf` in the `Cx` section at the end of the script, mimicking one generating input data for a previously implemented cxfield, and run the script). You may need to load the `satools-py3` module before running the script to give it access to NumPy and SciPy.
   
   b. Create an input namelist file that will be read by OPS to determine which cxfield needs to be written, and put it in an appropriate subfolder of `test/testinput`. Typically, you can:
      
      - Copy the existing `CxWriterNamelists_007_SurfaceCxField_modelsurface` or `CxWriterNamelists_001_UpperAirCxField_theta` subfolder (for surface, i.e. 1D, and upper-air, i.e. 2D, cxfields, respectively) to a new subfolder. For consistency, name that folder so that the embedded number is the index of the newly implemented cxfield (defined in the `OpsMod_CXIndexes.f90` file in OPS) and the suffix is the cxfield name. 
      - Adjust the name of the `*.nl` file in the subfolder; the part before the `.nl` extension must be the name of an observation group defined in OPS. It's best to pick a group to which the cxfield is relevant.
      - Edit the `CxFields=...` line in that file so that the number following `CxFields=` is the stash/ancillary code corresponding to the cxfield. To determine it, find the name of the appropriate `StashItem_*`, `StashCode_*` or `AncilCode_` constant in the `select case` statement in the `opsinputs_cxwriter_addrequiredgeovars` subroutine in `opsinputs_cxwriter_mod.F90` and look up the numeric value of that constant in the `OpsMod_Stash.f90` or `OpsMod_Ancil.f90` file in OPS.

   c. Create an input YAML file and put it in the `test/testinput` folder. Typically, you can

      - Copy the existing `007_SurfaceCxField_modelsurface.yaml` or `001_UpperAirCxField_theta.yaml` file and rename it, following the pattern described above for the folder containing a namelist file.
      - Edit the file, setting the ObsSpace name to the name of the OPS observation group, the GeoVaLs filename to the name of the file created in step a, and the expected variable index in the `expected_surface_variables` or `expected_upper_air_variables` section to the index of the newly defined cxfield.
   
   d. Add a call to the `ADD_WRITER_TEST` function in the `test/CMakeLists.txt` file, specifying the name of the test and its input YAML and data files.

Working practices
=================

The JEDI working principles are detailed at https://jointcenterforsatellitedataassimilation-jedi-docs.readthedocs-hosted.com/en/latest/working-practices/index.html.
